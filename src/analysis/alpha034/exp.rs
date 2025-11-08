use std::vec;

use chumsky::span::SimpleSpan;

use crate::{
    analysis::{
        get_symbol_definition_info,
        insert_symbol_reference,
        types::{
            make_union_type,
            matches_type,
            DataType,
            GenericsMap,
        },
        Context,
        FunctionArgument,
        FunctionSymbol,
        SymbolInfo,
        SymbolLocation,
        SymbolType,
        VariableSymbol,
    },
    files::{
        FileVersion,
        Files,
    },
    grammar::{
        alpha034::{
            Expression,
            InterpolatedCommand,
            InterpolatedText,
        },
        Spanned,
    },
    paths::FileId,
};

use super::stmnts::analyze_failure_handler;

#[tracing::instrument(skip_all)]
pub fn analyze_exp(
    file_id: FileId,
    file_version: FileVersion,
    (exp, exp_span): &Spanned<Expression>,
    expected_type: DataType,
    files: &Files,
    scoped_generic_types: &GenericsMap,
    contexts: &Vec<Context>,
) -> DataType {
    let exp_span_inclusive = exp_span.start..=exp_span.end;

    if exp_span_inclusive.is_empty() {
        return DataType::Error;
    }

    let file = (file_id, file_version);

    let ty: DataType = match exp {
        Expression::FunctionInvocation(_, (name, name_span), args, failure) => {
            let fun_symbol = get_symbol_definition_info(files, name, &file, name_span.start);

            let expected_types = match fun_symbol {
                Some(SymbolInfo {
                    symbol_type: SymbolType::Function(ref fun_symbol),
                    ..
                }) => fun_symbol
                    .arguments
                    .iter()
                    .map(|(arg, _)| (arg.data_type.clone(), arg.is_ref))
                    .collect::<Vec<(DataType, bool)>>(),
                Some(_) => {
                    files.report_error(&file, &format!("{name} is not a function"), *name_span);

                    vec![]
                }
                None => {
                    files.report_error(&file, &format!("{name} is not defined"), *name_span);

                    vec![]
                }
            };

            args.iter().enumerate().for_each(|(idx, arg)| {
                if let Some((ty, is_ref)) = expected_types.get(idx) {
                    let exp_ty = analyze_exp(
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

            if expected_types.len() > args.len() && fun_symbol.is_some() {
                files.report_error(
                    &file,
                    &format!("Function takes {} arguments", expected_types.len()),
                    *name_span,
                );
            };

            if let Some(failure) = failure {
                analyze_failure_handler(
                    file_id,
                    file_version,
                    failure,
                    files,
                    scoped_generic_types,
                    contexts,
                );
            }

            if let Some(SymbolInfo {
                symbol_type: SymbolType::Function(ref fun_symbol),
                ref data_type,
                ..
            }) = fun_symbol
            {
                let mut symbol_table = match files.symbol_table.get_mut(&file) {
                    Some(symbol_table) => symbol_table,
                    None => {
                        tracing::error!("Symbol table not found for the file");
                        return DataType::Error;
                    }
                };

                let mut last_span = SimpleSpan::new(name_span.end, name_span.end);
                symbol_table.symbols.insert(
                    exp_span_inclusive,
                    SymbolInfo {
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
                                        .unwrap_or(SimpleSpan::new(last_span.end, exp_span.end));

                                    last_span = arg_span;
                                    (
                                        FunctionArgument {
                                            name: arg.name.clone(),
                                            data_type: scoped_generic_types
                                                .deref_type(&arg.data_type),
                                            is_optional: false,
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
                        span: *exp_span,
                        contexts: contexts.clone(),
                    },
                );

                symbol_table
                    .references
                    .entry(name.clone())
                    .or_default()
                    .push(SymbolLocation {
                        file,
                        start: name_span.start,
                        end: name_span.end,
                    });
            }

            fun_symbol
                .map(|fun_symbol| fun_symbol.data_type)
                .unwrap_or(DataType::Null)
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
            let ty = analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Union(vec![
                    DataType::Number,
                    DataType::Text,
                    DataType::Array(Box::new(DataType::Union(vec![
                        DataType::Number,
                        DataType::Text,
                    ]))),
                ]),
                files,
                scoped_generic_types,
                contexts,
            );
            let right_hand_ty = analyze_exp(
                file_id,
                file_version,
                exp2,
                ty.clone(),
                files,
                scoped_generic_types,
                contexts,
            );

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
            analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Boolean,
                files,
                scoped_generic_types,
                contexts,
            );
            analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Boolean,
                files,
                scoped_generic_types,
                contexts,
            );

            DataType::Boolean
        }
        Expression::Array(elements) => {
            let types: Vec<DataType> = elements
                .iter()
                .map(|exp| {
                    analyze_exp(
                        file_id,
                        file_version,
                        exp,
                        DataType::Union(vec![DataType::Number, DataType::Text]),
                        files,
                        scoped_generic_types,
                        contexts,
                    )
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
            analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );

            ty.clone()
        }
        Expression::Command(_, inter_cmd, failure) => {
            inter_cmd.iter().for_each(|(inter_cmd, _)| {
                if let InterpolatedCommand::Expression(exp) = inter_cmd {
                    analyze_exp(
                        file_id,
                        file_version,
                        exp,
                        DataType::Any,
                        files,
                        scoped_generic_types,
                        contexts,
                    );
                }
            });

            if let Some(failure) = failure {
                analyze_failure_handler(
                    file_id,
                    file_version,
                    failure,
                    files,
                    scoped_generic_types,
                    contexts,
                );
            }

            DataType::Text
        }
        Expression::Divide(exp1, exp2) => {
            analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );
            analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );

            DataType::Number
        }
        Expression::Eq(exp1, exp2) => {
            analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );
            analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );

            DataType::Boolean
        }
        Expression::Ge(exp1, exp2) => {
            analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );
            analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );

            DataType::Boolean
        }
        Expression::Gt(exp1, exp2) => {
            analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );
            analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );

            DataType::Boolean
        }
        Expression::Is(exp, _, _) => {
            analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );

            DataType::Boolean
        }
        Expression::Le(exp1, exp2) => {
            analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );
            analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );

            DataType::Boolean
        }
        Expression::Lt(exp1, exp2) => {
            analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );
            analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );

            DataType::Boolean
        }
        Expression::Modulo(exp1, exp2) => {
            analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );
            analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );

            DataType::Number
        }
        Expression::Multiply(exp1, exp2) => {
            analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );
            analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );

            DataType::Number
        }
        Expression::Nameof(_, exp) => {
            analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );

            DataType::Text
        }
        Expression::Neg(_, exp) => {
            analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );

            DataType::Number
        }
        Expression::Neq(exp1, exp2) => {
            analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );
            analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );

            DataType::Boolean
        }
        Expression::Not(_, exp) => {
            analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Boolean,
                files,
                scoped_generic_types,
                contexts,
            );

            DataType::Boolean
        }
        Expression::Or(exp1, _, exp2) => {
            analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Boolean,
                files,
                scoped_generic_types,
                contexts,
            );
            analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Boolean,
                files,
                scoped_generic_types,
                contexts,
            );

            DataType::Boolean
        }
        Expression::Parentheses(exp) => analyze_exp(
            file_id,
            file_version,
            exp,
            DataType::Any,
            files,
            scoped_generic_types,
            contexts,
        ),
        Expression::Range(exp1, exp2) => {
            analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );
            analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );

            DataType::Array(Box::new(DataType::Number))
        }
        Expression::Subtract(exp1, exp2) => {
            analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );
            analyze_exp(
                file_id,
                file_version,
                exp2,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
            );

            DataType::Number
        }
        Expression::Ternary(exp1, _, exp2, _, exp3) => {
            analyze_exp(
                file_id,
                file_version,
                exp1,
                DataType::Boolean,
                files,
                scoped_generic_types,
                contexts,
            );
            let if_true = analyze_exp(
                file_id,
                file_version,
                exp2,
                expected_type.clone(),
                files,
                scoped_generic_types,
                contexts,
            );
            let if_false = analyze_exp(
                file_id,
                file_version,
                exp3,
                expected_type.clone(),
                files,
                scoped_generic_types,
                contexts,
            );

            make_union_type(vec![if_true, if_false])
        }
        Expression::Text(int_text) => {
            int_text.iter().for_each(|(text, _)| {
                if let InterpolatedText::Expression(exp) = text {
                    analyze_exp(
                        file_id,
                        file_version,
                        exp,
                        DataType::Any,
                        files,
                        scoped_generic_types,
                        contexts,
                    );
                }
            });

            DataType::Text
        }
        Expression::Number(_) => DataType::Number,
        Expression::Boolean(_) => DataType::Boolean,
        Expression::Null => DataType::Null,
        Expression::Status => {
            let mut symbol_table = match files.symbol_table.get_mut(&file) {
                Some(symbol_table) => symbol_table,
                None => {
                    tracing::error!("Symbol table not found for the file");
                    return DataType::Error;
                }
            };
            symbol_table.symbols.insert(
                exp_span_inclusive,
                SymbolInfo {
                    name: "status".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                    data_type: DataType::Number,
                    is_definition: false,
                    undefined: false,
                    span: *exp_span,
                    contexts: contexts.clone(),
                },
            );

            DataType::Number
        }
        Expression::Error => DataType::Any,
    };

    if !matches_type(&expected_type, &ty, scoped_generic_types) {
        files.report_error(
            &file,
            &format!(
                "Expected type {}, found type {}",
                expected_type.to_string(scoped_generic_types),
                ty.to_string(scoped_generic_types)
            ),
            *exp_span,
        );
    } else if let DataType::Generic(id) = ty {
        scoped_generic_types.constrain_generic_type(id, expected_type.clone());
    }

    ty
}
