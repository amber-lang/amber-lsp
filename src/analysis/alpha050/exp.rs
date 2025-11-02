use std::vec;

use chumsky::span::SimpleSpan;

use crate::{
    analysis::{
        get_symbol_definition_info, insert_symbol_reference,
        types::{make_union_type, matches_type, DataType, GenericsMap},
        BlockContext, Context, FunctionArgument, FunctionSymbol, SymbolInfo, SymbolLocation,
        SymbolType, VariableSymbol,
    },
    files::{FileVersion, Files},
    grammar::{
        alpha050::{Expression, FailableHandler, InterpolatedCommand, InterpolatedText},
        CommandModifier, Spanned,
    },
    paths::FileId,
};

use super::stmnts::{analyze_failable_handlers, StmntAnalysisResult};

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

    let ty: DataType = match exp {
        Expression::ArrayIndex(exp, index) => {
            let ExpAnalysisResult {
                exp_ty: array_ty,
                return_ty,
                is_propagating_failure: prop,
            } = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Array(Box::new(DataType::Any)),
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop;
            return_types.extend(return_ty);

            let ExpAnalysisResult {
                return_ty: index_return_ty,
                is_propagating_failure: prop,
                exp_ty: index_ty,
            } = analyze_exp(
                file_id,
                file_version,
                index,
                DataType::Union(vec![
                    DataType::Int,
                    DataType::Array(Box::new(DataType::Int)),
                ]),
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop;
            return_types.extend(index_return_ty);

            match array_ty {
                DataType::Generic(id) => match scoped_generic_types.get_recursive(id) {
                    DataType::Array(inner) => match scoped_generic_types.deref_type(&index_ty) {
                        DataType::Array(_) => array_ty,
                        _ => *inner,
                    },
                    _ => DataType::Null,
                },
                DataType::Array(ref inner) => match scoped_generic_types.deref_type(&index_ty) {
                    DataType::Array(_) => array_ty,
                    _ => *inner.clone(),
                },
                _ => DataType::Null,
            }
        }
        Expression::Exit(_, exit_code) => {
            if let Some(exit_code) = exit_code {
                let ExpAnalysisResult {
                    return_ty,
                    is_propagating_failure: prop,
                    ..
                } = analyze_exp(
                    file_id,
                    file_version,
                    exit_code,
                    DataType::Int,
                    files,
                    scoped_generic_types,
                    contexts,
                );

                is_propagating_failure |= prop;
                return_types.extend(return_ty);
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
                    .map(|(arg, _)| (arg.data_type.clone(), arg.is_optional, arg.is_ref))
                    .collect::<Vec<(DataType, bool, bool)>>(),
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
                if let Some((ty, _, is_ref)) = expected_types.get(idx) {
                    let ExpAnalysisResult {
                        is_propagating_failure: propagates_failure,
                        return_ty,
                        exp_ty,
                    } = analyze_exp(
                        file_id,
                        file_version,
                        arg,
                        ty.clone(),
                        files,
                        scoped_generic_types,
                        contexts,
                    );

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

                    return_types.extend(return_ty);
                    is_propagating_failure |= propagates_failure;

                    if let DataType::Generic(id) = ty {
                        scoped_generic_types.constrain_generic_type(*id, exp_ty.clone());
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
                .filter(|(_, is_optional, _)| !*is_optional)
                .count()
                > args.len()
                && fun_symbol.is_some()
            {
                files.report_error(
                    &file,
                    &format!("Function takes {} arguments", expected_types.len()),
                    *name_span,
                );
            };

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
                                    .unwrap_or(SimpleSpan::new(last_span_end, exp_span.end));

                                last_span_end = arg_span.end;
                                (
                                    FunctionArgument {
                                        name: arg.name.clone(),
                                        data_type: scoped_generic_types.deref_type(&arg.data_type),
                                        is_optional: arg.is_optional,
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

            let has_failure_handler = failable_handlers
                .iter()
                .any(|(modifier, _)| matches!(modifier, FailableHandler::Failure(_)));

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
            let ExpAnalysisResult {
                exp_ty: ty,
                return_ty: return1,
                is_propagating_failure: is_prop1,
            } = analyze_exp(
                file_id,
                file_version,
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
                files,
                scoped_generic_types,
                contexts,
            );
            let ExpAnalysisResult {
                return_ty: return2,
                is_propagating_failure: is_prop2,
                exp_ty: right_hand_ty,
            } = analyze_exp(
                file_id,
                file_version,
                exp2,
                ty.clone(),
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= is_prop1 || is_prop2;

            return_types.extend(return1);
            return_types.extend(return2);

            if let DataType::Generic(id) = ty {
                scoped_generic_types.constrain_generic_type(id, right_hand_ty.clone());
            }

            if !matches_type(&right_hand_ty, &ty, scoped_generic_types) {
                files.report_error(
                    &file,
                    &format!(
                        "Expected type {}, found type {}",
                        right_hand_ty.to_string(scoped_generic_types),
                        ty.to_string(scoped_generic_types),
                    ),
                    exp1.1,
                );
            }

            ty
        }
        Expression::And(exp1, _, exp2) => {
            let ExpAnalysisResult {
                return_ty: return1,
                is_propagating_failure: prop1,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Boolean,
                files,
                scoped_generic_types,
                contexts,
            );
            let ExpAnalysisResult {
                return_ty: return2,
                is_propagating_failure: prop2,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Boolean,
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop1 || prop2;

            return_types.extend(return1);
            return_types.extend(return2);

            DataType::Boolean
        }
        Expression::Array(elements) => {
            let types: Vec<DataType> = elements
                .iter()
                .map(|exp| {
                    let ExpAnalysisResult {
                        exp_ty: ty,
                        return_ty,
                        is_propagating_failure: prop,
                    } = analyze_exp(
                        file_id,
                        file_version,
                        exp,
                        DataType::Union(vec![DataType::Number, DataType::Int, DataType::Text]),
                        files,
                        scoped_generic_types,
                        contexts,
                    );

                    is_propagating_failure |= prop;
                    return_types.extend(return_ty);

                    ty
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
            let ExpAnalysisResult {
                return_ty,
                is_propagating_failure: prop,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop;
            return_types.extend(return_ty);

            ty.clone()
        }
        Expression::Command(modifiers, inter_cmd, failable_handlers) => {
            inter_cmd.iter().for_each(|(inter_cmd, _)| {
                if let InterpolatedCommand::Expression(exp) = inter_cmd {
                    let ExpAnalysisResult {
                        return_ty,
                        is_propagating_failure: is_prop,
                        ..
                    } = analyze_exp(
                        file_id,
                        file_version,
                        exp,
                        DataType::Any,
                        files,
                        scoped_generic_types,
                        contexts,
                    );

                    is_propagating_failure |= is_prop;
                    return_types.extend(return_ty);
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

            let has_failure_handler = failable_handlers
                .iter()
                .any(|(modifier, _)| matches!(modifier, FailableHandler::Failure(_)));

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
        Expression::Divide(exp1, exp2) => {
            let ExpAnalysisResult {
                return_ty: return1,
                is_propagating_failure: prop1,
                exp_ty: exp_ty1,
            } = analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
                files,
                scoped_generic_types,
                contexts,
            );
            let ExpAnalysisResult {
                return_ty: return2,
                is_propagating_failure: prop2,
                exp_ty: exp_ty2,
            } = analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop1 || prop2;
            return_types.extend(return1);
            return_types.extend(return2);

            if exp_ty1 == DataType::Number || exp_ty2 == DataType::Number {
                DataType::Number
            } else {
                DataType::Int
            }
        }
        Expression::Eq(exp1, exp2) => {
            let ExpAnalysisResult {
                return_ty: return1,
                is_propagating_failure: prop1,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );
            let ExpAnalysisResult {
                return_ty: return2,
                is_propagating_failure: prop2,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop1 || prop2;
            return_types.extend(return1);
            return_types.extend(return2);

            DataType::Boolean
        }
        Expression::Ge(exp1, exp2) => {
            let ExpAnalysisResult {
                return_ty: return1,
                is_propagating_failure: prop1,
                ..
            } = analyze_exp(
                file_id,
                file_version,
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
                ),
                files,
                scoped_generic_types,
                contexts,
            );
            let ExpAnalysisResult {
                return_ty: return2,
                is_propagating_failure: prop2,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp2,
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
                ),
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop1 || prop2;
            return_types.extend(return1);
            return_types.extend(return2);

            DataType::Boolean
        }
        Expression::Gt(exp1, exp2) => {
            let ExpAnalysisResult {
                return_ty: return1,
                is_propagating_failure: prop1,
                ..
            } = analyze_exp(
                file_id,
                file_version,
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
                ),
                files,
                scoped_generic_types,
                contexts,
            );
            let ExpAnalysisResult {
                return_ty: return2,
                is_propagating_failure: prop2,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp2,
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
                ),
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop1 || prop2;
            return_types.extend(return1);
            return_types.extend(return2);

            DataType::Boolean
        }
        Expression::Is(exp, _, _) => {
            let ExpAnalysisResult {
                return_ty,
                is_propagating_failure: prop,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop;
            return_types.extend(return_ty);

            DataType::Boolean
        }
        Expression::Le(exp1, exp2) => {
            let ExpAnalysisResult {
                return_ty: return1,
                is_propagating_failure: prop1,
                ..
            } = analyze_exp(
                file_id,
                file_version,
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
                ),
                files,
                scoped_generic_types,
                contexts,
            );
            let ExpAnalysisResult {
                return_ty: return2,
                is_propagating_failure: prop2,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp2,
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
                ),
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop1 || prop2;
            return_types.extend(return1);
            return_types.extend(return2);

            DataType::Boolean
        }
        Expression::Lt(exp1, exp2) => {
            let ExpAnalysisResult {
                return_ty: return1,
                is_propagating_failure: prop1,
                ..
            } = analyze_exp(
                file_id,
                file_version,
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
                ),
                files,
                scoped_generic_types,
                contexts,
            );
            let ExpAnalysisResult {
                return_ty: return2,
                is_propagating_failure: prop2,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp2,
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
                ),
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop1 || prop2;
            return_types.extend(return1);
            return_types.extend(return2);

            DataType::Boolean
        }
        Expression::Modulo(exp1, exp2) => {
            let ExpAnalysisResult {
                return_ty: return1,
                is_propagating_failure: prop1,
                exp_ty: exp_ty1,
            } = analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
                files,
                scoped_generic_types,
                contexts,
            );
            let ExpAnalysisResult {
                return_ty: return2,
                is_propagating_failure: prop2,
                exp_ty: exp_ty2,
            } = analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop1 || prop2;
            return_types.extend(return1);
            return_types.extend(return2);

            if exp_ty1 == DataType::Number || exp_ty2 == DataType::Number {
                DataType::Number
            } else {
                DataType::Int
            }
        }
        Expression::Multiply(exp1, exp2) => {
            let ExpAnalysisResult {
                return_ty: return1,
                is_propagating_failure: prop1,
                exp_ty: exp_ty1,
            } = analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
                files,
                scoped_generic_types,
                contexts,
            );
            let ExpAnalysisResult {
                return_ty: return2,
                is_propagating_failure: prop2,
                exp_ty: exp_ty2,
            } = analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop1 || prop2;
            return_types.extend(return1);
            return_types.extend(return2);

            if exp_ty1 == DataType::Number || exp_ty2 == DataType::Number {
                DataType::Number
            } else {
                DataType::Int
            }
        }
        Expression::Nameof(_, exp) => {
            let ExpAnalysisResult {
                return_ty,
                is_propagating_failure: prop,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop;
            return_types.extend(return_ty);

            DataType::Text
        }
        Expression::Neg(_, exp) => {
            let ExpAnalysisResult {
                return_ty,
                is_propagating_failure: prop,
                exp_ty,
            } = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop;
            return_types.extend(return_ty);

            exp_ty
        }
        Expression::Neq(exp1, exp2) => {
            let ExpAnalysisResult {
                return_ty: return1,
                is_propagating_failure: prop1,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );
            let ExpAnalysisResult {
                return_ty: return2,
                is_propagating_failure: prop2,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop1 || prop2;
            return_types.extend(return1);
            return_types.extend(return2);

            DataType::Boolean
        }
        Expression::Not(_, exp) => {
            let ExpAnalysisResult {
                return_ty,
                is_propagating_failure: prop,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Boolean,
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop;
            return_types.extend(return_ty);

            DataType::Boolean
        }
        Expression::Or(exp1, _, exp2) => {
            let ExpAnalysisResult {
                return_ty: return1,
                is_propagating_failure: prop1,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Boolean,
                files,
                scoped_generic_types,
                contexts,
            );
            let ExpAnalysisResult {
                return_ty: return2,
                is_propagating_failure: prop2,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Boolean,
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop1 || prop2;
            return_types.extend(return1);
            return_types.extend(return2);

            DataType::Boolean
        }
        Expression::Parentheses(exp) => {
            let ExpAnalysisResult {
                return_ty,
                is_propagating_failure: prop,
                exp_ty,
            } = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop;
            return_types.extend(return_ty);

            exp_ty
        }
        Expression::Range(exp1, exp2) => {
            let ExpAnalysisResult {
                return_ty: return1,
                is_propagating_failure: prop1,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Int,
                files,
                scoped_generic_types,
                contexts,
            );
            let ExpAnalysisResult {
                return_ty: return2,
                is_propagating_failure: prop2,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Int,
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop1 || prop2;
            return_types.extend(return1);
            return_types.extend(return2);

            DataType::Array(Box::new(DataType::Int))
        }
        Expression::Subtract(exp1, exp2) => {
            let ExpAnalysisResult {
                return_ty: return1,
                is_propagating_failure: prop1,
                exp_ty: exp_ty1,
            } = analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
                files,
                scoped_generic_types,
                contexts,
            );
            let ExpAnalysisResult {
                return_ty: return2,
                is_propagating_failure: prop2,
                exp_ty: exp_ty2,
            } = analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop1 || prop2;
            return_types.extend(return1);
            return_types.extend(return2);

            if exp_ty1 == DataType::Number || exp_ty2 == DataType::Number {
                DataType::Number
            } else {
                DataType::Int
            }
        }
        Expression::Ternary(exp1, _, exp2, _, exp3) => {
            let ExpAnalysisResult {
                return_ty: return1,
                is_propagating_failure: prop1,
                ..
            } = analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Boolean,
                files,
                scoped_generic_types,
                contexts,
            );
            let ExpAnalysisResult {
                exp_ty: if_true,
                return_ty: return2,
                is_propagating_failure: prop2,
            } = analyze_exp(
                file_id,
                file_version,
                exp2,
                expected_type.clone(),
                files,
                scoped_generic_types,
                contexts,
            );
            let ExpAnalysisResult {
                exp_ty: if_false,
                return_ty: return3,
                is_propagating_failure: prop3,
            } = analyze_exp(
                file_id,
                file_version,
                exp3,
                expected_type.clone(),
                files,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= prop1 || prop2 || prop3;
            return_types.extend(return1);
            return_types.extend(return2);
            return_types.extend(return3);

            make_union_type(vec![if_true, if_false])
        }
        Expression::Text(int_text) => {
            int_text.iter().for_each(|(text, _)| {
                if let InterpolatedText::Expression(exp) = text {
                    let ExpAnalysisResult {
                        return_ty,
                        is_propagating_failure: prop,
                        ..
                    } = analyze_exp(
                        file_id,
                        file_version,
                        exp,
                        DataType::Any,
                        files,
                        scoped_generic_types,
                        contexts,
                    );

                    is_propagating_failure |= prop;
                    return_types.extend(return_ty);
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
