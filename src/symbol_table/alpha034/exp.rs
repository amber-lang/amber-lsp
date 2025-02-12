use crate::{
    backend::Backend,
    grammar::{
        alpha034::{DataType, Expression, InterpolatedCommand, InterpolatedText},
        Spanned,
    },
    paths::FileId,
    symbol_table::{
        get_symbol_definition_info, insert_symbol_reference,
        types::{make_union_type, matches_type, GenericsMap},
        SymbolInfo, SymbolLocation, SymbolType, VarSymbol,
    },
};

use super::stmnts::analyze_failure_handler;

#[tracing::instrument]
pub fn analyze_exp(
    file_id: &FileId,
    (exp, exp_span): &Spanned<Expression>,
    expected_type: DataType,
    backend: &Backend,
    scoped_generic_types: &GenericsMap,
) -> DataType {
    let exp_span_inclusive = exp_span.start..=exp_span.end;

    let ty: DataType = match exp {
        Expression::FunctionInvocation((name, name_span), args, failure) => {
            let return_type = {
                let fun_symbol =
                    get_symbol_definition_info(backend, name, file_id, name_span.start);

                let expected_types = fun_symbol
                    .clone()
                    .and_then(|fun_symbol| {
                        if let SymbolType::Function(fun_symbol) = fun_symbol.symbol_type {
                            Some(fun_symbol.arguments)
                        } else {
                            None
                        }
                    })
                    .unwrap_or(vec![])
                    .iter()
                    .map(|(_, ty)| ty.clone())
                    .collect::<Vec<DataType>>();

                args.iter().enumerate().for_each(|(idx, arg)| {
                    if let Some(ty) = expected_types.get(idx) {
                        let exp_ty =
                            analyze_exp(file_id, arg, ty.clone(), backend, scoped_generic_types);

                        match ty {
                            DataType::Generic(file_id, id) => {
                                scoped_generic_types
                                    .constrain_generic_type((*file_id, *id), exp_ty.clone());
                            }
                            _ => {}
                        }
                    } else {
                        backend.report_error(
                            file_id,
                            &format!("Function takes only {} arguments", expected_types.len()),
                            arg.1,
                        );
                    }
                });

                if expected_types.len() > args.len() {
                    backend.report_error(
                        file_id,
                        &format!("Function takes {} arguments", expected_types.len()),
                        *name_span,
                    );
                };

                fun_symbol
                    .and_then(|fun_symbol| Some(fun_symbol.data_type))
                    .unwrap_or(DataType::Null)
            };

            if let Some(failure) = failure {
                analyze_failure_handler(file_id, failure, backend, scoped_generic_types);
            }

            insert_symbol_reference(
                &name,
                backend,
                &SymbolLocation {
                    file: *file_id,
                    start: name_span.start,
                    end: name_span.end,
                    is_public: false,
                },
                scoped_generic_types,
            );

            return_type
        }
        Expression::Var((name, name_span)) => {
            insert_symbol_reference(
                &name,
                backend,
                &SymbolLocation {
                    file: *file_id,
                    start: name_span.start,
                    end: name_span.end,
                    is_public: false,
                },
                scoped_generic_types,
            );

            match get_symbol_definition_info(backend, &name, file_id, name_span.start) {
                Some(info) => info.data_type,
                None => DataType::Null,
            }
        }
        Expression::Add(exp1, exp2) => {
            let ty = analyze_exp(
                file_id,
                exp1,
                DataType::Union(vec![
                    DataType::Number,
                    DataType::Text,
                    DataType::Array(Box::new(DataType::Union(vec![
                        DataType::Number,
                        DataType::Text,
                    ]))),
                ]),
                backend,
                scoped_generic_types,
            );
            let right_hand_ty =
                analyze_exp(file_id, exp2, ty.clone(), backend, scoped_generic_types);

            if let DataType::Generic(file_id, id) = ty {
                scoped_generic_types.constrain_generic_type((file_id, id), right_hand_ty.clone());
            }

            if !matches_type(&right_hand_ty, &ty, scoped_generic_types) {
                backend.report_error(
                    file_id,
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
                exp1,
                DataType::Boolean,
                backend,
                scoped_generic_types,
            );
            analyze_exp(
                file_id,
                exp2,
                DataType::Boolean,
                backend,
                scoped_generic_types,
            );

            DataType::Boolean
        }
        Expression::Array(elements) => {
            let types: Vec<DataType> = elements
                .iter()
                .map(|exp| {
                    analyze_exp(
                        file_id,
                        exp,
                        DataType::Union(vec![DataType::Number, DataType::Text]),
                        backend,
                        scoped_generic_types,
                    )
                })
                .collect();

            let array_type = make_union_type(types);

            match array_type {
                DataType::Union(_) => {
                    backend.report_error(
                        file_id,
                        "Array must have elements of the same type",
                        *exp_span,
                    );
                }
                _ => {}
            }

            DataType::Array(Box::new(array_type))
        }
        Expression::Cast(exp, _, (ty, _)) => {
            analyze_exp(file_id, &exp, DataType::Any, backend, scoped_generic_types);

            ty.clone()
        }
        Expression::Command(inter_cmd, failure) => {
            inter_cmd.iter().for_each(|(inter_cmd, _)| match inter_cmd {
                InterpolatedCommand::Expression(exp) => {
                    analyze_exp(file_id, &exp, DataType::Any, backend, scoped_generic_types);
                }
                _ => {}
            });

            if let Some(failure) = failure {
                analyze_failure_handler(file_id, failure, backend, scoped_generic_types);
            }

            DataType::Text
        }
        Expression::Divide(exp1, exp2) => {
            analyze_exp(
                file_id,
                exp1,
                DataType::Number,
                backend,
                scoped_generic_types,
            );
            analyze_exp(
                file_id,
                exp2,
                DataType::Number,
                backend,
                scoped_generic_types,
            );

            DataType::Number
        }
        Expression::Eq(exp1, exp2) => {
            analyze_exp(file_id, exp1, DataType::Any, backend, scoped_generic_types);
            analyze_exp(file_id, exp2, DataType::Any, backend, scoped_generic_types);

            DataType::Boolean
        }
        Expression::Ge(exp1, exp2) => {
            analyze_exp(
                file_id,
                exp1,
                DataType::Number,
                backend,
                scoped_generic_types,
            );
            analyze_exp(
                file_id,
                exp2,
                DataType::Number,
                backend,
                scoped_generic_types,
            );

            DataType::Boolean
        }
        Expression::Gt(exp1, exp2) => {
            analyze_exp(
                file_id,
                exp1,
                DataType::Number,
                backend,
                scoped_generic_types,
            );
            analyze_exp(
                file_id,
                exp2,
                DataType::Number,
                backend,
                scoped_generic_types,
            );

            DataType::Boolean
        }
        Expression::Is(exp, _, _) => {
            analyze_exp(file_id, exp, DataType::Any, backend, scoped_generic_types);

            DataType::Boolean
        }
        Expression::Le(exp1, exp2) => {
            analyze_exp(
                file_id,
                exp1,
                DataType::Number,
                backend,
                scoped_generic_types,
            );
            analyze_exp(
                file_id,
                exp2,
                DataType::Number,
                backend,
                scoped_generic_types,
            );

            DataType::Boolean
        }
        Expression::Lt(exp1, exp2) => {
            analyze_exp(
                file_id,
                exp1,
                DataType::Number,
                backend,
                scoped_generic_types,
            );
            analyze_exp(
                file_id,
                exp2,
                DataType::Number,
                backend,
                scoped_generic_types,
            );

            DataType::Boolean
        }
        Expression::Modulo(exp1, exp2) => {
            analyze_exp(
                file_id,
                exp1,
                DataType::Number,
                backend,
                scoped_generic_types,
            );
            analyze_exp(
                file_id,
                exp2,
                DataType::Number,
                backend,
                scoped_generic_types,
            );

            DataType::Number
        }
        Expression::Multiply(exp1, exp2) => {
            analyze_exp(
                file_id,
                exp1,
                DataType::Number,
                backend,
                scoped_generic_types,
            );
            analyze_exp(
                file_id,
                exp2,
                DataType::Number,
                backend,
                scoped_generic_types,
            );

            DataType::Number
        }
        Expression::Nameof(_, exp) => {
            analyze_exp(file_id, exp, DataType::Any, backend, scoped_generic_types);

            DataType::Text
        }
        Expression::Neg(_, exp) => {
            analyze_exp(
                file_id,
                exp,
                DataType::Number,
                backend,
                scoped_generic_types,
            );

            DataType::Number
        }
        Expression::Neq(exp1, exp2) => {
            analyze_exp(file_id, exp1, DataType::Any, backend, scoped_generic_types);
            analyze_exp(file_id, exp2, DataType::Any, backend, scoped_generic_types);

            DataType::Boolean
        }
        Expression::Not(_, exp) => {
            analyze_exp(
                file_id,
                exp,
                DataType::Boolean,
                backend,
                scoped_generic_types,
            );

            DataType::Boolean
        }
        Expression::Or(exp1, _, exp2) => {
            analyze_exp(
                file_id,
                exp1,
                DataType::Boolean,
                backend,
                scoped_generic_types,
            );
            analyze_exp(
                file_id,
                exp2,
                DataType::Boolean,
                backend,
                scoped_generic_types,
            );

            DataType::Boolean
        }
        Expression::Parentheses(exp) => {
            analyze_exp(file_id, exp, DataType::Any, backend, scoped_generic_types)
        }
        Expression::Range(exp1, exp2) => {
            analyze_exp(
                file_id,
                exp1,
                DataType::Number,
                backend,
                scoped_generic_types,
            );
            analyze_exp(
                file_id,
                exp2,
                DataType::Number,
                backend,
                scoped_generic_types,
            );

            DataType::Array(Box::new(DataType::Number))
        }
        Expression::Subtract(exp1, exp2) => {
            analyze_exp(
                file_id,
                exp1,
                DataType::Number,
                backend,
                scoped_generic_types,
            );
            analyze_exp(
                file_id,
                exp2,
                DataType::Number,
                backend,
                scoped_generic_types,
            );

            DataType::Number
        }
        Expression::Ternary(exp1, _, exp2, _, exp3) => {
            analyze_exp(
                file_id,
                exp1,
                DataType::Boolean,
                backend,
                scoped_generic_types,
            );
            let if_true = analyze_exp(
                file_id,
                exp2,
                expected_type.clone(),
                backend,
                scoped_generic_types,
            );
            let if_false = analyze_exp(
                file_id,
                exp3,
                expected_type.clone(),
                backend,
                scoped_generic_types,
            );

            make_union_type(vec![if_true, if_false])
        }
        Expression::Text(int_text) => {
            int_text.iter().for_each(|(text, _)| match text {
                InterpolatedText::Expression(exp) => {
                    analyze_exp(file_id, exp, DataType::Any, backend, scoped_generic_types);
                }
                _ => {}
            });

            DataType::Text
        }
        Expression::Number(_) => DataType::Number,
        Expression::Boolean(_) => DataType::Boolean,
        Expression::Null => DataType::Null,
        Expression::Status => {
            let mut symbol_table = backend.symbol_table.get_mut(file_id).unwrap();
            symbol_table.symbols.insert(
                exp_span_inclusive,
                SymbolInfo {
                    name: "status".to_string(),
                    symbol_type: SymbolType::Variable(VarSymbol {}),
                    data_type: DataType::Number,
                    is_definition: false,
                    undefined: false,
                },
            );

            DataType::Number
        }
        Expression::Error => DataType::Any,
    };

    if !matches_type(&expected_type, &ty, scoped_generic_types) {
        backend.report_error(
            file_id,
            &format!(
                "Expected type {}, found type {}",
                expected_type.to_string(scoped_generic_types),
                ty.to_string(scoped_generic_types)
            ),
            *exp_span,
        );
    } else if let DataType::Generic(file_id, id) = ty {
        scoped_generic_types.constrain_generic_type((file_id, id), expected_type.clone());
    }

    ty
}
