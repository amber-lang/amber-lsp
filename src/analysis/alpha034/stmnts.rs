use crate::{
    backend::Backend,
    grammar::{alpha034::*, Spanned},
    paths::FileId,
    analysis::{
        get_symbol_definition_info, insert_symbol_definition, insert_symbol_reference,
        types::{make_union_type, matches_type, GenericsMap},
        DataType, SymbolLocation, SymbolType, VarSymbol,
    },
};

use super::exp::analyze_exp;

/// Analyze a statement.
///
/// Returns the data type of the return statement.
#[tracing::instrument]
pub fn analyze_stmnt(
    file_id: &FileId,
    (stmnt, span): &Spanned<Statement>,
    backend: &Backend,
    scope_end: usize,
    scoped_generic_types: &GenericsMap,
) -> Option<DataType> {
    match stmnt {
        Statement::Block(block) => {
            return analyze_block(file_id, block, backend, scoped_generic_types);
        }
        Statement::IfChain(_, if_chain) => {
            for (if_chain_content, _) in if_chain.iter() {
                match if_chain_content {
                    IfChainContent::IfCondition((condition, _)) => match condition {
                        IfCondition::IfCondition(exp, block) => {
                            analyze_exp(
                                file_id,
                                exp,
                                DataType::Boolean,
                                backend,
                                scoped_generic_types,
                            );
                            return analyze_block(file_id, block, backend, scoped_generic_types);
                        }
                        IfCondition::InlineIfCondition(exp, boxed_stmnt) => {
                            analyze_exp(
                                file_id,
                                exp,
                                DataType::Boolean,
                                backend,
                                scoped_generic_types,
                            );
                            return analyze_stmnt(
                                file_id,
                                boxed_stmnt,
                                backend,
                                boxed_stmnt.1.end,
                                scoped_generic_types,
                            );
                        }
                        _ => {}
                    },
                    IfChainContent::Else((else_cond, _)) => match else_cond {
                        ElseCondition::Else(_, block) => {
                            return analyze_block(file_id, block, backend, scoped_generic_types);
                        }
                        ElseCondition::InlineElse(_, stmnt) => {
                            return analyze_stmnt(
                                file_id,
                                stmnt,
                                backend,
                                stmnt.1.end,
                                scoped_generic_types,
                            );
                        }
                    },
                }
            }
        }
        Statement::IfCondition(_, if_cond, else_cond) => {
            match &if_cond.0 {
                IfCondition::IfCondition(exp, block) => {
                    analyze_exp(
                        file_id,
                        exp,
                        DataType::Boolean,
                        backend,
                        scoped_generic_types,
                    );
                    return analyze_block(file_id, block, backend, scoped_generic_types);
                }
                IfCondition::InlineIfCondition(exp, boxed_stmnt) => {
                    analyze_exp(
                        file_id,
                        exp,
                        DataType::Boolean,
                        backend,
                        scoped_generic_types,
                    );
                    return analyze_stmnt(
                        file_id,
                        boxed_stmnt,
                        backend,
                        boxed_stmnt.1.end,
                        scoped_generic_types,
                    );
                }
                _ => {}
            }

            if let Some(else_cond) = else_cond {
                match &else_cond.0 {
                    ElseCondition::Else(_, block) => {
                        return analyze_block(file_id, block, backend, scoped_generic_types);
                    }
                    ElseCondition::InlineElse(_, stmnt) => {
                        return analyze_stmnt(
                            file_id,
                            stmnt,
                            backend,
                            stmnt.1.end,
                            scoped_generic_types,
                        );
                    }
                }
            }
        }
        Statement::InfiniteLoop(_, block) => {
            return analyze_block(file_id, block, backend, scoped_generic_types);
        }
        Statement::IterLoop(_, (vars, _), _, exp, block) => {
            let block_span = block.1.clone();
            match &vars {
                IterLoopVars::WithIndex((var1, var1_span), (var2, var2_span)) => {
                    let mut symbol_table = backend.symbol_table.get_mut(file_id).unwrap();
                    insert_symbol_definition(
                        &mut symbol_table,
                        var1,
                        block_span.start..=block_span.end,
                        &SymbolLocation {
                            file: *file_id,
                            start: var1_span.start,
                            end: var1_span.end,
                            is_public: false,
                        },
                        DataType::Number,
                        SymbolType::Variable(VarSymbol {}),
                    );

                    insert_symbol_definition(
                        &mut symbol_table,
                        var2,
                        block_span.start..=block_span.end,
                        &SymbolLocation {
                            file: *file_id,
                            start: var2_span.start,
                            end: var2_span.end,
                            is_public: false,
                        },
                        DataType::Number,
                        SymbolType::Variable(VarSymbol {}),
                    );
                }
                IterLoopVars::Single((var, var_span)) => {
                    let mut symbol_table = backend.symbol_table.get_mut(file_id).unwrap();
                    insert_symbol_definition(
                        &mut symbol_table,
                        var,
                        block_span.start..=block_span.end,
                        &SymbolLocation {
                            file: *file_id,
                            start: var_span.start,
                            end: var_span.end,
                            is_public: false,
                        },
                        DataType::Number,
                        SymbolType::Variable(VarSymbol {}),
                    );
                }
                _ => {}
            }

            analyze_exp(
                file_id,
                exp,
                DataType::Array(Box::new(DataType::Number)),
                backend,
                scoped_generic_types,
            );
            return analyze_block(file_id, block, backend, scoped_generic_types);
        }
        Statement::VariableInit(_, (var_name, var_span), exp) => {
            let exp_type = analyze_exp(file_id, exp, DataType::Any, backend, scoped_generic_types);

            let mut symbol_table = backend.symbol_table.get_mut(file_id).unwrap();
            insert_symbol_definition(
                &mut symbol_table,
                var_name,
                span.end..=scope_end,
                &SymbolLocation {
                    file: *file_id,
                    start: var_span.start,
                    end: var_span.end,
                    is_public: false,
                },
                scoped_generic_types.deref_type(&exp_type),
                SymbolType::Variable(VarSymbol {}),
            );
        }
        Statement::Echo(_, exp) => {
            analyze_exp(file_id, exp, DataType::Any, backend, scoped_generic_types);
        }
        Statement::Expression(exp) => {
            analyze_exp(file_id, exp, DataType::Any, backend, scoped_generic_types);
        }
        Statement::Fail(_, exp) => {
            if let Some(exp) = exp {
                analyze_exp(
                    file_id,
                    exp,
                    DataType::Number,
                    backend,
                    scoped_generic_types,
                );
            }
        }
        Statement::Return(_, exp) => {
            if let Some(exp) = exp {
                let ty = analyze_exp(file_id, exp, DataType::Any, backend, scoped_generic_types);

                return Some(ty);
            }

            return Some(DataType::Null);
        }
        Statement::ShorthandAdd((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(backend, &var, file_id, var_span.start) {
                Some(info) => info.data_type,
                None => DataType::Any,
            };

            let default_ty = DataType::Union(vec![
                DataType::Text,
                DataType::Number,
                DataType::Array(Box::new(DataType::Union(vec![
                    DataType::Text,
                    DataType::Number,
                ]))),
            ]);

            let exp_ty = analyze_exp(file_id, exp, var_ty.clone(), backend, scoped_generic_types);

            if !matches_type(&default_ty, &var_ty, scoped_generic_types)
                || !matches_type(&exp_ty, &var_ty, scoped_generic_types)
            {
                backend.report_error(
                    file_id,
                    &format!(
                        "Cannot add to variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    var_span.clone(),
                );
            }

            insert_symbol_reference(
                &var,
                backend,
                &SymbolLocation {
                    file: *file_id,
                    start: var_span.start,
                    end: var_span.end,
                    is_public: false,
                },
                scoped_generic_types,
            );
        }
        Statement::ShorthandDiv((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(backend, &var, file_id, var_span.start) {
                Some(info) => info.data_type,
                None => DataType::Any,
            };

            if !matches_type(&DataType::Number, &var_ty, scoped_generic_types) {
                backend.report_error(
                    file_id,
                    &format!(
                        "Cannot divide variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    var_span.clone(),
                );
            }

            analyze_exp(
                file_id,
                exp,
                DataType::Number,
                backend,
                scoped_generic_types,
            );

            insert_symbol_reference(
                &var,
                backend,
                &SymbolLocation {
                    file: *file_id,
                    start: var_span.start,
                    end: var_span.end,
                    is_public: false,
                },
                scoped_generic_types,
            );
        }
        Statement::ShorthandModulo((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(backend, &var, file_id, var_span.start) {
                Some(info) => info.data_type,
                None => DataType::Any,
            };

            if !matches_type(&DataType::Number, &var_ty, scoped_generic_types) {
                backend.report_error(
                    file_id,
                    &format!(
                        "Cannot use modulo with variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    var_span.clone(),
                );
            }

            analyze_exp(
                file_id,
                exp,
                DataType::Number,
                backend,
                scoped_generic_types,
            );

            insert_symbol_reference(
                &var,
                backend,
                &SymbolLocation {
                    file: *file_id,
                    start: var_span.start,
                    end: var_span.end,
                    is_public: false,
                },
                scoped_generic_types,
            );
        }
        Statement::ShorthandMul((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(backend, &var, file_id, var_span.start) {
                Some(info) => info.data_type,
                None => DataType::Any,
            };

            if !matches_type(&DataType::Number, &var_ty, scoped_generic_types) {
                backend.report_error(
                    file_id,
                    &format!(
                        "Cannot use multiply with variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    var_span.clone(),
                );
            }

            analyze_exp(
                file_id,
                exp,
                DataType::Number,
                backend,
                scoped_generic_types,
            );

            insert_symbol_reference(
                &var,
                backend,
                &SymbolLocation {
                    file: *file_id,
                    start: var_span.start,
                    end: var_span.end,
                    is_public: false,
                },
                scoped_generic_types,
            );
        }
        Statement::ShorthandSub((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(backend, &var, file_id, var_span.start) {
                Some(info) => info.data_type,
                None => DataType::Any,
            };

            if !matches_type(&DataType::Number, &var_ty, scoped_generic_types) {
                backend.report_error(
                    file_id,
                    &format!(
                        "Cannot use subtract with variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    var_span.clone(),
                );
            }

            analyze_exp(
                file_id,
                exp,
                DataType::Number,
                backend,
                scoped_generic_types,
            );

            insert_symbol_reference(
                &var,
                backend,
                &SymbolLocation {
                    file: *file_id,
                    start: var_span.start,
                    end: var_span.end,
                    is_public: false,
                },
                scoped_generic_types,
            );
        }
        Statement::VariableSet((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(backend, &var, file_id, var_span.start) {
                Some(info) => info.data_type,
                None => DataType::Any,
            };

            analyze_exp(file_id, exp, var_ty, backend, scoped_generic_types);

            insert_symbol_reference(
                &var,
                backend,
                &SymbolLocation {
                    file: *file_id,
                    start: var_span.start,
                    end: var_span.end,
                    is_public: false,
                },
                scoped_generic_types,
            );
        }
        _ => {}
    };

    None
}

pub fn analyze_block(
    file_id: &FileId,
    (block, span): &Spanned<Block>,
    backend: &Backend,
    scoped_generic_types: &GenericsMap,
) -> Option<DataType> {
    let mut types = vec![];
    if let Block::Block(stmnt) = block {
        for stmnt in stmnt.iter() {
            if let Some(ty) = analyze_stmnt(file_id, stmnt, backend, span.end, scoped_generic_types)
            {
                types.push(ty);
            }
        }
    }

    if types.is_empty() {
        None
    } else {
        Some(make_union_type(types))
    }
}

pub fn analyze_failure_handler(
    file_id: &FileId,
    (failure, span): &Spanned<FailureHandler>,
    backend: &Backend,
    scoped_generic_types: &GenericsMap,
) {
    match failure {
        FailureHandler::Handle(_, stmnts) => {
            stmnts.iter().for_each(|stmnt| {
                analyze_stmnt(file_id, stmnt, backend, span.end, scoped_generic_types);
            });
        }
        _ => {}
    }
}
