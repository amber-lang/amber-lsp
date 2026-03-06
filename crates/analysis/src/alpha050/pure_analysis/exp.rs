//! Expression-level analysis: type checking, variable references, function calls.

use amber_grammar::alpha050::{
    Expression,
    FailableHandler,
    InterpolatedCommand,
    InterpolatedText,
};
use amber_grammar::{
    CommandModifier,
    Spanned,
};
use amber_types::{
    DataType,
    PureGenericsMap,
};

use amber_types::matches_type_pure;

use crate::types::make_union_type;
use crate::{
    BlockContext,
    Context,
    FunctionArgument,
    FunctionSymbol,
    SymbolInfo,
    SymbolType,
    VariableSymbol,
};

use super::context::PureContext;
use super::stmnts::analyze_failable_handlers_pure;
use super::utils::get_constrain_ty_for_compare_pure;
use super::ExpAnalysisResult;

// ─── Expression Analysis ──────────────────────────────────────────────────────

pub fn analyze_exp_pure(
    (exp, exp_span): &Spanned<Expression>,
    expected_type: DataType,
    ctx: &mut PureContext,
    scoped_generic_types: &mut PureGenericsMap,
    contexts: &[Context],
) -> ExpAnalysisResult {
    let exp_span_inclusive = exp_span.start..=exp_span.end;

    if exp_span_inclusive.is_empty() {
        return ExpAnalysisResult {
            exp_ty: DataType::Null,
            is_propagating_failure: false,
            return_ty: None,
        };
    }

    let mut return_types: Vec<DataType> = vec![];
    let mut is_propagating_failure = false;

    macro_rules! analyze_expr {
        ($exp:expr, $exp_type:expr) => {{
            let result = analyze_exp_pure($exp, $exp_type, ctx, scoped_generic_types, contexts);
            is_propagating_failure |= result.is_propagating_failure;
            return_types.extend(result.return_ty.iter().cloned());
            result
        }};
    }

    macro_rules! analyze_binop_codep {
        ($exp1:expr, $exp1_type:expr, $exp2:expr) => {{
            let lhs_result = analyze_expr!($exp1, $exp1_type);
            let rhs_result = analyze_expr!($exp2, lhs_result.exp_ty.clone());

            if let DataType::Generic(id) = lhs_result.exp_ty.clone() {
                scoped_generic_types.constrain_generic_type(id, rhs_result.exp_ty.clone());
            }

            if !matches_type_pure(&rhs_result.exp_ty, &lhs_result.exp_ty, scoped_generic_types) {
                ctx.report_error(
                    &format!(
                        "Expected type {}, found type {}",
                        scoped_generic_types.to_display_string(&rhs_result.exp_ty),
                        scoped_generic_types.to_display_string(&lhs_result.exp_ty),
                    ),
                    $exp1.1,
                );
            }

            lhs_result.exp_ty
        }};
    }

    let ty: DataType = match exp {
        Expression::ArrayIndex(array_exp, index_exp) => {
            let array_result = analyze_expr!(array_exp, DataType::Array(Box::new(DataType::Any)));
            let index_result = analyze_expr!(
                index_exp,
                DataType::Union(vec![
                    DataType::Int,
                    DataType::Array(Box::new(DataType::Int))
                ])
            );

            match array_result.exp_ty {
                DataType::Generic(id) => match scoped_generic_types.get_recursive(id) {
                    DataType::Array(inner) => {
                        match scoped_generic_types.deref_type(&index_result.exp_ty) {
                            DataType::Array(_) => array_result.exp_ty,
                            _ => match *inner {
                                DataType::Any => {
                                    let inner_id = ctx.allocator.next_id();
                                    scoped_generic_types.constrain_generic_type(
                                        id,
                                        DataType::Array(Box::new(DataType::Generic(inner_id))),
                                    );
                                    DataType::Generic(inner_id)
                                }
                                other => other,
                            },
                        }
                    }
                    DataType::Any => {
                        let inner_id = ctx.allocator.next_id();
                        scoped_generic_types.constrain_generic_type(
                            id,
                            DataType::Array(Box::new(DataType::Generic(inner_id))),
                        );
                        match scoped_generic_types.deref_type(&index_result.exp_ty) {
                            DataType::Array(_) => {
                                DataType::Array(Box::new(DataType::Generic(inner_id)))
                            }
                            _ => DataType::Generic(inner_id),
                        }
                    }
                    _ => DataType::Null,
                },
                DataType::Array(ref inner) => {
                    match scoped_generic_types.deref_type(&index_result.exp_ty) {
                        DataType::Array(_) => array_result.exp_ty,
                        _ => *inner.clone(),
                    }
                }
                _ => DataType::Null,
            }
        }
        Expression::Exit(_, exit_code) => {
            if let Some(exit_code) = exit_code {
                analyze_expr!(exit_code, DataType::Int);
            }
            DataType::Null
        }
        Expression::FunctionInvocation(modifiers, (name, name_span), args, failable_handlers) => {
            let fun_symbol = ctx.get_definition_info(name, name_span.start);

            let expected_types = match fun_symbol {
                Some(SymbolInfo {
                    symbol_type: SymbolType::Function(ref fun_sym),
                    ..
                }) => fun_sym
                    .arguments
                    .iter()
                    .map(|(arg, _)| {
                        (
                            arg.data_type.clone(),
                            arg.is_optional,
                            arg.is_ref,
                            arg.default_value_type.clone(),
                        )
                    })
                    .collect::<Vec<_>>(),
                Some(_) => {
                    ctx.report_error(&format!("{name} is not a function"), *exp_span);
                    vec![]
                }
                None => {
                    ctx.report_error(&format!("{name} is not defined"), *exp_span);
                    vec![]
                }
            };

            args.iter().enumerate().for_each(|(idx, arg)| {
                if let Some((ty, _, is_ref, _)) = expected_types.get(idx) {
                    match (is_ref, arg.0.clone()) {
                        (true, Expression::Var((ref_name, ref_span))) => {
                            if let Some(var) = ctx.get_definition_info(&ref_name, ref_span.start) {
                                if let SymbolType::Variable(ref var_sym) = var.symbol_type {
                                    if var_sym.is_const {
                                        ctx.report_error(
                                            "Cannot modify a constant variable",
                                            ref_span,
                                        );
                                    }
                                }
                            }
                        }
                        (true, _) => {
                            ctx.report_error("Cannot pass a non-variable as a reference", arg.1);
                        }
                        _ => {}
                    }

                    let arg_result = analyze_expr!(arg, ty.clone());
                    if let DataType::Generic(id) = ty {
                        scoped_generic_types.constrain_generic_type(*id, arg_result.exp_ty.clone());
                    }
                } else {
                    ctx.report_error(
                        &format!("Function takes only {} arguments", expected_types.len()),
                        arg.1,
                    );
                }
            });

            if expected_types
                .iter()
                .filter(|(_, is_optional, ..)| !*is_optional)
                .count()
                > args.len()
                && fun_symbol.is_some()
            {
                ctx.report_error(
                    &format!("Function takes {} arguments", expected_types.len()),
                    *name_span,
                );
            } else {
                expected_types
                    .iter()
                    .skip(args.len())
                    .filter(|(_, is_optional, ..)| *is_optional)
                    .for_each(|(ty, _, _, default_type)| {
                        if let DataType::Generic(id) = ty {
                            scoped_generic_types
                                .constrain_generic_type(*id, default_type.clone().unwrap());
                        }
                    });
            }

            let exp_ty = fun_symbol
                .clone()
                .map(|f| f.data_type)
                .unwrap_or(DataType::Null);

            let handler_result = analyze_failable_handlers_pure(
                failable_handlers,
                ctx,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= handler_result.is_propagating_failure;
            return_types.extend(handler_result.return_ty);

            let function_call_scope_end = failable_handlers
                .first()
                .map(|(_, s)| s.start - 1)
                .unwrap_or(exp_span.end - 1);

            // Insert function call symbol info
            if let Some(SymbolInfo {
                symbol_type: SymbolType::Function(ref fun_sym),
                ref data_type,
                ..
            }) = fun_symbol
            {
                use chumsky::span::SimpleSpan;

                let mut last_span_end = name_span.end + 1;
                let fun_info = SymbolInfo {
                    name: name.clone(),
                    symbol_type: SymbolType::Function(FunctionSymbol {
                        arguments: fun_sym
                            .arguments
                            .iter()
                            .enumerate()
                            .map(|(idx, (arg, _))| {
                                let arg_span = args
                                    .get(idx)
                                    .map(|(_, s)| *s)
                                    .unwrap_or(SimpleSpan::from(last_span_end..exp_span.end));
                                last_span_end = arg_span.end;
                                (
                                    FunctionArgument {
                                        name: arg.name.clone(),
                                        data_type: scoped_generic_types.deref_type(&arg.data_type),
                                        is_optional: arg.is_optional,
                                        default_value_type: arg.default_value_type.clone(),
                                        is_ref: arg.is_ref,
                                    },
                                    arg_span,
                                )
                            })
                            .collect(),
                        ..fun_sym.clone()
                    }),
                    data_type: scoped_generic_types.deref_type(data_type),
                    is_definition: false,
                    undefined: false,
                    span: *name_span,
                    contexts: contexts.to_vec(),
                };

                ctx.symbol_table
                    .symbols
                    .insert(name_span.start..=name_span.end, fun_info.clone());
                ctx.symbol_table
                    .fun_call_arg_scope
                    .insert(name_span.end..=function_call_scope_end, fun_info);
            }

            // Check for unhandled failable
            let has_failure_handler = failable_handlers.iter().any(|(m, _)| {
                matches!(m, FailableHandler::Failure(_))
                    || matches!(m, FailableHandler::Exited(_, _, _))
            });

            if matches!(
                scoped_generic_types.deref_type(&exp_ty),
                DataType::Failable(_)
            ) && modifiers
                .iter()
                .all(|(m, _)| *m != CommandModifier::Unsafe && *m != CommandModifier::Trust)
                && contexts.iter().all(|c| match c {
                    Context::Block(BlockContext { modifiers }) => modifiers
                        .iter()
                        .all(|m| *m != CommandModifier::Unsafe && *m != CommandModifier::Trust),
                    _ => true,
                })
                && !has_failure_handler
            {
                ctx.report_error(
                    "Failable function must be handled with a failure handler or marked with `trust` modifier",
                    *name_span,
                );
            }

            exp_ty
        }
        Expression::Var((name, name_span)) => {
            ctx.insert_reference(
                name,
                name_span.start,
                name_span.end,
                scoped_generic_types,
                contexts,
            );

            match ctx.get_definition_info(name, name_span.start) {
                Some(info) => {
                    if matches!(info.symbol_type, SymbolType::Function(_)) {
                        ctx.report_error(&format!("{name} is a function"), *name_span);
                    }
                    info.data_type
                }
                None => DataType::Null,
            }
        }
        Expression::Add(exp1, exp2) => {
            analyze_binop_codep!(
                exp1,
                DataType::Union(vec![
                    DataType::Number,
                    DataType::Int,
                    DataType::Text,
                    DataType::Array(Box::new(DataType::Union(vec![
                        DataType::Number,
                        DataType::Int,
                        DataType::Text,
                    ]))),
                ]),
                exp2
            )
        }
        Expression::And(exp1, _, exp2) | Expression::Or(exp1, _, exp2) => {
            analyze_binop_codep!(exp1, DataType::Boolean, exp2);
            DataType::Boolean
        }
        Expression::Array(elements) => {
            let types: Vec<DataType> = elements
                .iter()
                .map(|el| {
                    let result = analyze_expr!(
                        el,
                        DataType::Union(vec![DataType::Number, DataType::Int, DataType::Text])
                    );
                    result.exp_ty
                })
                .collect();

            let array_type = make_union_type(types);

            if let DataType::Union(_) = array_type {
                ctx.report_error("Array must have elements of the same type", *exp_span);
            }

            DataType::Array(Box::new(array_type))
        }
        Expression::Cast(exp, _, (ty, _)) => {
            analyze_expr!(exp, DataType::Any);
            ty.clone()
        }
        Expression::Command(modifiers, inter_cmd, failable_handlers) => {
            inter_cmd.iter().for_each(|(cmd, _)| {
                if let InterpolatedCommand::Expression(exp) = cmd {
                    analyze_expr!(exp, DataType::Any);
                }
            });

            let handler_result = analyze_failable_handlers_pure(
                failable_handlers,
                ctx,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= handler_result.is_propagating_failure;
            return_types.extend(handler_result.return_ty);

            let has_failure_handler = failable_handlers.iter().any(|(m, _)| {
                matches!(m, FailableHandler::Failure(_))
                    || matches!(m, FailableHandler::Exited(_, _, _))
            });

            if !has_failure_handler
                && !modifiers
                    .iter()
                    .any(|(m, _)| *m == CommandModifier::Unsafe || *m == CommandModifier::Trust)
                && !contexts.iter().any(|c| match c {
                    Context::Block(BlockContext { modifiers }) => modifiers
                        .iter()
                        .any(|m| *m == CommandModifier::Unsafe || *m == CommandModifier::Trust),
                    _ => false,
                })
            {
                ctx.report_error("Command must have a failure handler", *exp_span);
            }

            DataType::Text
        }
        Expression::Multiply(exp1, exp2)
        | Expression::Divide(exp1, exp2)
        | Expression::Modulo(exp1, exp2)
        | Expression::Subtract(exp1, exp2) => {
            analyze_binop_codep!(
                exp1,
                DataType::Union(vec![DataType::Number, DataType::Int]),
                exp2
            )
        }
        Expression::Eq(exp1, exp2) | Expression::Neq(exp1, exp2) => {
            analyze_binop_codep!(exp1, DataType::Any, exp2);
            DataType::Boolean
        }
        Expression::Ge(exp1, exp2)
        | Expression::Gt(exp1, exp2)
        | Expression::Le(exp1, exp2)
        | Expression::Lt(exp1, exp2) => {
            let lhs = analyze_expr!(
                exp1,
                DataType::Union(vec![
                    DataType::Number,
                    DataType::Int,
                    DataType::Text,
                    DataType::Array(Box::new(DataType::Union(vec![
                        DataType::Number,
                        DataType::Int,
                        DataType::Text,
                    ]))),
                ])
            );

            let mut left_constrain_ty =
                get_constrain_ty_for_compare_pure(lhs.exp_ty.clone(), scoped_generic_types);
            let rhs = analyze_expr!(exp2, left_constrain_ty.clone());
            let right_constrain_ty =
                get_constrain_ty_for_compare_pure(rhs.exp_ty, scoped_generic_types);

            if let DataType::Generic(id) = lhs.exp_ty.clone() {
                scoped_generic_types.constrain_generic_type(id, right_constrain_ty.clone());
                left_constrain_ty =
                    get_constrain_ty_for_compare_pure(lhs.exp_ty, scoped_generic_types);
            }

            if !matches_type_pure(
                &right_constrain_ty,
                &left_constrain_ty,
                scoped_generic_types,
            ) {
                ctx.report_error(
                    &format!(
                        "Expected type {}, found type {}",
                        scoped_generic_types.to_display_string(&right_constrain_ty),
                        scoped_generic_types.to_display_string(&left_constrain_ty),
                    ),
                    exp1.1,
                );
            }

            DataType::Boolean
        }
        Expression::Is(exp, _, _) => {
            analyze_expr!(exp, DataType::Any);
            DataType::Boolean
        }
        Expression::Nameof(_, exp) => {
            analyze_expr!(exp, DataType::Any);
            DataType::Text
        }
        Expression::Neg(_, exp) => {
            analyze_expr!(exp, DataType::Union(vec![DataType::Number, DataType::Int])).exp_ty
        }
        Expression::Not(_, exp) => {
            analyze_expr!(exp, DataType::Boolean);
            DataType::Boolean
        }
        Expression::Parentheses(exp) => analyze_expr!(exp, DataType::Any).exp_ty,
        Expression::Range(exp1, exp2) => {
            analyze_binop_codep!(exp1, DataType::Int, exp2);
            DataType::Array(Box::new(DataType::Int))
        }
        Expression::Ternary(exp1, _, exp2, _, exp3) => {
            analyze_expr!(exp1, DataType::Boolean);
            let if_true = analyze_expr!(exp2, expected_type.clone()).exp_ty;
            let if_false = analyze_expr!(exp3, expected_type.clone()).exp_ty;
            make_union_type(vec![if_true, if_false])
        }
        Expression::Text(int_text) => {
            int_text.iter().for_each(|(text, _)| {
                if let InterpolatedText::Expression(exp) = text {
                    analyze_expr!(exp, DataType::Any);
                }
            });
            DataType::Text
        }
        Expression::Number(_) => DataType::Number,
        Expression::Int(_) => DataType::Int,
        Expression::Boolean(_) => DataType::Boolean,
        Expression::Null => DataType::Null,
        Expression::Status => {
            ctx.symbol_table.symbols.insert(
                exp_span_inclusive,
                SymbolInfo {
                    name: "status".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                    data_type: DataType::Int,
                    is_definition: false,
                    undefined: false,
                    span: *exp_span,
                    contexts: contexts.to_vec(),
                },
            );
            DataType::Int
        }
        Expression::Error => DataType::Any,
    };

    // Final type check against expected type
    if !matches_type_pure(&expected_type, &ty, scoped_generic_types) {
        ctx.report_error(
            &format!(
                "Expected type `{}`, found type `{}`",
                scoped_generic_types.to_display_string(&expected_type),
                scoped_generic_types.to_display_string(&ty),
            ),
            *exp_span,
        );
    } else if let DataType::Generic(id) = ty {
        scoped_generic_types.constrain_generic_type(id, expected_type.clone());
    }

    ExpAnalysisResult {
        exp_ty: ty,
        is_propagating_failure,
        return_ty: if return_types.is_empty() {
            None
        } else {
            Some(make_union_type(return_types))
        },
    }
}
