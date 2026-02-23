use std::vec;

use chumsky::span::SimpleSpan;

use crate::files::{
    FileVersion,
    Files,
};
use crate::types::{
    make_union_type,
    matches_type,
    DataType,
    GenericsMap,
};
use crate::{
    get_symbol_definition_info,
    insert_symbol_reference,
    BlockContext,
    Context,
    FunctionArgument,
    FunctionSymbol,
    SymbolInfo,
    SymbolLocation,
    SymbolType,
    VariableSymbol,
};
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
use amber_types::paths::FileId;

use super::stmnts::{
    analyze_failable_handlers,
    StmntAnalysisResult,
};

#[derive(Debug, Clone)]
pub struct ExpAnalysisResult {
    pub exp_ty: DataType,
    pub is_propagating_failure: bool,
    pub return_ty: Option<DataType>,
}

#[tracing::instrument(skip_all)]
pub fn analyze_exp(
    file_id: FileId,
    file_version: FileVersion,
    (exp, exp_span): &Spanned<Expression>,
    expected_type: DataType,
    files: &Files,
    scoped_generic_types: &GenericsMap,
    contexts: &Vec<Context>,
) -> ExpAnalysisResult {
    let exp_span_inclusive = exp_span.start..=exp_span.end;

    if exp_span_inclusive.is_empty() {
        return ExpAnalysisResult {
            exp_ty: DataType::Null,
            is_propagating_failure: false,
            return_ty: None,
        };
    }

    let file = (file_id, file_version);

    let mut return_types = vec![];
    let mut is_propagating_failure = false;

    macro_rules! analyze_expr {
        ($exp:expr, $exp_type:expr) => {{
            let result = analyze_exp(
                file_id,
                file_version,
                $exp,
                $exp_type,
                files,
                scoped_generic_types,
                contexts,
            );

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

            if !matches_type(
                &rhs_result.exp_ty,
                &lhs_result.exp_ty,
                &scoped_generic_types,
            ) {
                files.report_error(
                    &file,
                    &format!(
                        "Expected type {}, found type {}",
                        rhs_result.exp_ty.to_string(&scoped_generic_types),
                        lhs_result.exp_ty.to_string(&scoped_generic_types),
                    ),
                    $exp1.1,
                );
            }

            lhs_result.exp_ty
        }};
    }

    let ty: DataType = match exp {
        Expression::ArrayIndex(exp, index) => {
            let array_result = analyze_expr!(exp, DataType::Array(Box::new(DataType::Any)));

            let index_result = analyze_expr!(
                index,
                DataType::Union(vec![
                    DataType::Int,
                    DataType::Array(Box::new(DataType::Int)),
                ])
            );

            match array_result.exp_ty {
                DataType::Generic(id) => match scoped_generic_types.get_recursive(id) {
                    DataType::Array(inner) => {
                        match scoped_generic_types.deref_type(&index_result.exp_ty) {
                            DataType::Array(_) => array_result.exp_ty,
                            _ => match *inner {
                                DataType::Any => {
                                    // Create a parametric inner type so the element type
                                    // can be resolved at call sites via generic unification.
                                    let inner_id = scoped_generic_types.new_generic_id();
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
                        // The generic is unconstrained; we know it's used as an array.
                        let inner_id = scoped_generic_types.new_generic_id();
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
            let fun_symbol = get_symbol_definition_info(files, name, &file, name_span.start);

            let expected_types = match fun_symbol {
                Some(SymbolInfo {
                    symbol_type: SymbolType::Function(ref fun_symbol),
                    ..
                }) => fun_symbol
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
                    .collect::<Vec<(DataType, bool, bool, Option<DataType>)>>(),
                Some(_) => {
                    files.report_error(&file, &format!("{name} is not a function"), *exp_span);

                    vec![]
                }
                None => {
                    files.report_error(&file, &format!("{name} is not defined"), *exp_span);

                    vec![]
                }
            };

            args.iter().enumerate().for_each(|(idx, arg)| {
                if let Some((ty, _, is_ref, _)) = expected_types.get(idx) {
                    match (is_ref, arg.0.clone()) {
                        (true, Expression::Var((name, span))) => {
                            if let Some(var) =
                                get_symbol_definition_info(files, &name, &file, span.start)
                            {
                                if let SymbolType::Variable(ref var_symbol) = var.symbol_type {
                                    if var_symbol.is_const {
                                        files.report_error(
                                            &file,
                                            "Cannot modify a constant variable",
                                            span,
                                        );
                                    }
                                }
                            }
                        }
                        (true, _) => {
                            files.report_error(
                                &file,
                                "Cannot pass a non-variable as a reference",
                                arg.1,
                            );
                        }
                        _ => {}
                    }

                    let arg_result = analyze_expr!(arg, ty.clone());

                    if let DataType::Generic(id) = ty {
                        scoped_generic_types.constrain_generic_type(*id, arg_result.exp_ty.clone());
                    }
                } else {
                    files.report_error(
                        &file,
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
                files.report_error(
                    &file,
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
                .map(|fun_symbol| fun_symbol.data_type)
                .unwrap_or(DataType::Null);

            let StmntAnalysisResult {
                return_ty: failure_return_ty,
                is_propagating_failure: is_prop,
            } = analyze_failable_handlers(
                file_id,
                file_version,
                failable_handlers,
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= is_prop;
            return_types.extend(failure_return_ty);

            let function_call_scope_end = failable_handlers
                .first()
                .map(|(_, span)| span.start - 1)
                .unwrap_or(exp_span.end - 1);

            if let Some(SymbolInfo {
                symbol_type: SymbolType::Function(ref fun_symbol),
                ref data_type,
                ..
            }) = fun_symbol
            {
                let mut symbol_table = match files.symbol_table.get_mut(&file) {
                    Some(symbol_table) => symbol_table,
                    None => {
                        return ExpAnalysisResult {
                            exp_ty: DataType::Null,
                            is_propagating_failure: false,
                            return_ty: None,
                        }
                    }
                };

                let mut last_span_end = name_span.end + 1;
                let fun_symbol = SymbolInfo {
                    name: name.clone(),
                    symbol_type: SymbolType::Function(FunctionSymbol {
                        arguments: fun_symbol
                            .arguments
                            .iter()
                            .enumerate()
                            .map(|(idx, (arg, _))| {
                                let arg_span = args
                                    .get(idx)
                                    .map(|(_, span)| *span)
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
                        ..fun_symbol.clone()
                    }),
                    data_type: scoped_generic_types.deref_type(data_type),
                    is_definition: false,
                    undefined: false,
                    span: *name_span,
                    contexts: contexts.clone(),
                };

                symbol_table
                    .symbols
                    .insert(name_span.start..=name_span.end, fun_symbol.clone());

                symbol_table
                    .fun_call_arg_scope
                    .insert(name_span.end..=function_call_scope_end, fun_symbol);
            }

            let has_failure_handler = failable_handlers.iter().any(|(modifier, _)| {
                matches!(modifier, FailableHandler::Failure(_))
                    || matches!(modifier, FailableHandler::Exited(_, _, _))
            });

            if matches!(
                scoped_generic_types.deref_type(&exp_ty),
                DataType::Failable(_)
            ) && modifiers.iter().all(|(modifier, _)| {
                *modifier != CommandModifier::Unsafe && *modifier != CommandModifier::Trust
            }) && contexts.iter().all(|ctx| match ctx {
                Context::Block(BlockContext { modifiers }) => modifiers.iter().all(|modifier| {
                    *modifier != CommandModifier::Unsafe && *modifier != CommandModifier::Trust
                }),
                _ => true,
            }) && !has_failure_handler
            {
                files.report_error(
                    &file,
                    "Failable function must be handled with a failure handler or marked with `trust` modifier",
                    *name_span,
                );
            }

            exp_ty
        }
        Expression::Var((name, name_span)) => {
            insert_symbol_reference(
                name,
                files,
                &SymbolLocation {
                    file,
                    start: name_span.start,
                    end: name_span.end,
                },
                scoped_generic_types,
                contexts,
            );

            match get_symbol_definition_info(files, name, &file, name_span.start) {
                Some(info) => {
                    if matches!(info.symbol_type, SymbolType::Function(_)) {
                        files.report_error(&file, &format!("{name} is a function"), *name_span);
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
                .map(|exp| {
                    let result = analyze_expr!(
                        exp,
                        DataType::Union(vec![DataType::Number, DataType::Int, DataType::Text])
                    );

                    result.exp_ty
                })
                .collect();

            let array_type = make_union_type(types);

            if let DataType::Union(_) = array_type {
                files.report_error(
                    &file,
                    "Array must have elements of the same type",
                    *exp_span,
                );
            }

            DataType::Array(Box::new(array_type))
        }
        Expression::Cast(exp, _, (ty, _)) => {
            analyze_expr!(exp, DataType::Any);

            ty.clone()
        }
        Expression::Command(modifiers, inter_cmd, failable_handlers) => {
            inter_cmd.iter().for_each(|(inter_cmd, _)| {
                if let InterpolatedCommand::Expression(exp) = inter_cmd {
                    analyze_expr!(exp, DataType::Any);
                }
            });

            let StmntAnalysisResult {
                return_ty: failure_return_ty,
                is_propagating_failure: is_prop,
            } = analyze_failable_handlers(
                file_id,
                file_version,
                failable_handlers,
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= is_prop;
            return_types.extend(failure_return_ty);

            let has_failure_handler = failable_handlers.iter().any(|(modifier, _)| {
                matches!(modifier, FailableHandler::Failure(_))
                    || matches!(modifier, FailableHandler::Exited(_, _, _))
            });

            if !has_failure_handler
                && !modifiers.iter().any(|(modifier, _)| {
                    *modifier == CommandModifier::Unsafe || *modifier == CommandModifier::Trust
                })
                && !contexts.iter().any(|ctx| match ctx {
                    Context::Block(BlockContext { modifiers }) => {
                        modifiers.iter().any(|modifier| {
                            *modifier == CommandModifier::Unsafe
                                || *modifier == CommandModifier::Trust
                        })
                    }
                    _ => false,
                })
            {
                files.report_error(&file, "Command must have a failure handler", *exp_span);
            }

            DataType::Text
        }
        Expression::Multiply(exp1, exp2)
        | Expression::Divide(exp1, exp2)
        | Expression::Modulo(exp1, exp2)
        | Expression::Subtract(exp1, exp2) => {
            analyze_binop_codep!(
                exp1,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
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
                DataType::Union(
                    [
                        DataType::Number,
                        DataType::Int,
                        DataType::Text,
                        DataType::Array(Box::new(DataType::Union(
                            [DataType::Number, DataType::Int, DataType::Text].to_vec(),
                        ))),
                    ]
                    .to_vec(),
                )
            );

            let mut left_constrain_ty =
                get_constrain_ty_for_compare(lhs.exp_ty.clone(), scoped_generic_types);

            let rhs = analyze_expr!(exp2, left_constrain_ty.clone());

            let right_constrain_ty = get_constrain_ty_for_compare(rhs.exp_ty, scoped_generic_types);

            if let DataType::Generic(id) = lhs.exp_ty.clone() {
                scoped_generic_types.constrain_generic_type(id, right_constrain_ty.clone());
                left_constrain_ty = get_constrain_ty_for_compare(lhs.exp_ty, scoped_generic_types);
            }

            if !matches_type(
                &right_constrain_ty,
                &left_constrain_ty,
                scoped_generic_types,
            ) {
                files.report_error(
                    &file,
                    &format!(
                        "Expected type {}, found type {}",
                        right_constrain_ty.to_string(scoped_generic_types),
                        left_constrain_ty.to_string(scoped_generic_types),
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
            analyze_expr!(
                exp,
                DataType::Union([DataType::Number, DataType::Int].to_vec())
            )
            .exp_ty
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
            let mut symbol_table = match files.symbol_table.get_mut(&file) {
                Some(symbol_table) => symbol_table,
                None => {
                    return ExpAnalysisResult {
                        exp_ty: DataType::Null,
                        is_propagating_failure: false,
                        return_ty: None,
                    };
                }
            };
            symbol_table.symbols.insert(
                exp_span_inclusive,
                SymbolInfo {
                    name: "status".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                    data_type: DataType::Int,
                    is_definition: false,
                    undefined: false,
                    span: *exp_span,
                    contexts: contexts.clone(),
                },
            );

            DataType::Int
        }
        Expression::Error => DataType::Any,
    };

    if !matches_type(&expected_type, &ty, scoped_generic_types) {
        files.report_error(
            &file,
            &format!(
                "Expected type `{}`, found type `{}`",
                expected_type.to_string(scoped_generic_types),
                ty.to_string(scoped_generic_types)
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

fn get_constrain_ty_for_compare(
    constrain: DataType,
    scoped_generic_types: &GenericsMap,
) -> DataType {
    match constrain.clone() {
        DataType::Generic(id) => {
            get_constrain_ty_for_compare(scoped_generic_types.get(id), scoped_generic_types)
        }
        DataType::Int => DataType::Union(vec![DataType::Int, DataType::Number]),
        DataType::Number => DataType::Union(vec![DataType::Int, DataType::Number]),
        DataType::Union(types) => DataType::Union(
            types
                .iter()
                .map(|ty| get_constrain_ty_for_compare(ty.clone(), scoped_generic_types))
                .collect(),
        ),
        ty => ty,
    }
}
