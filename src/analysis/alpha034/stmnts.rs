use crate::{
    analysis::{
        get_symbol_definition_info, insert_symbol_definition, insert_symbol_reference,
        types::{make_union_type, matches_type, GenericsMap},
        DataType, SymbolLocation, SymbolType, VarSymbol,
    },
    files::{FileVersion, Files},
    grammar::{alpha034::*, Spanned},
    paths::FileId,
};

use super::exp::analyze_exp;

/// Analyze a statement.
///
/// Returns the data type of the return statement.
#[tracing::instrument(skip_all)]
pub fn analyze_stmnt(
    file_id: FileId,
    file_version: FileVersion,
    (stmnt, span): &Spanned<Statement>,
    files: &Files,
    scope_end: usize,
    scoped_generic_types: &GenericsMap,
) -> Option<DataType> {
    let file = (file_id, file_version);

    match stmnt {
        Statement::Block(block) => {
            return analyze_block(file_id, file_version, block, files, scoped_generic_types);
        }
        Statement::IfChain(_, if_chain) => {
            for (if_chain_content, _) in if_chain.iter() {
                match if_chain_content {
                    IfChainContent::IfCondition((condition, _)) => match condition {
                        IfCondition::IfCondition(exp, block) => {
                            analyze_exp(
                                file_id,
                                file_version,
                                exp,
                                DataType::Boolean,
                                files,
                                scoped_generic_types,
                            );
                            return analyze_block(
                                file_id,
                                file_version,
                                block,
                                files,
                                scoped_generic_types,
                            );
                        }
                        IfCondition::InlineIfCondition(exp, boxed_stmnt) => {
                            analyze_exp(
                                file_id,
                                file_version,
                                exp,
                                DataType::Boolean,
                                files,
                                scoped_generic_types,
                            );
                            return analyze_stmnt(
                                file_id,
                                file_version,
                                boxed_stmnt,
                                files,
                                boxed_stmnt.1.end,
                                scoped_generic_types,
                            );
                        }
                        _ => {}
                    },
                    IfChainContent::Else((else_cond, _)) => match else_cond {
                        ElseCondition::Else(_, block) => {
                            return analyze_block(
                                file_id,
                                file_version,
                                block,
                                files,
                                scoped_generic_types,
                            );
                        }
                        ElseCondition::InlineElse(_, stmnt) => {
                            return analyze_stmnt(
                                file_id,
                                file_version,
                                stmnt,
                                files,
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
                        file_version,
                        exp,
                        DataType::Boolean,
                        files,
                        scoped_generic_types,
                    );
                    return analyze_block(
                        file_id,
                        file_version,
                        block,
                        files,
                        scoped_generic_types,
                    );
                }
                IfCondition::InlineIfCondition(exp, boxed_stmnt) => {
                    analyze_exp(
                        file_id,
                        file_version,
                        exp,
                        DataType::Boolean,
                        files,
                        scoped_generic_types,
                    );
                    return analyze_stmnt(
                        file_id,
                        file_version,
                        boxed_stmnt,
                        files,
                        boxed_stmnt.1.end,
                        scoped_generic_types,
                    );
                }
                _ => {}
            }

            if let Some(else_cond) = else_cond {
                match &else_cond.0 {
                    ElseCondition::Else(_, block) => {
                        return analyze_block(
                            file_id,
                            file_version,
                            block,
                            files,
                            scoped_generic_types,
                        );
                    }
                    ElseCondition::InlineElse(_, stmnt) => {
                        return analyze_stmnt(
                            file_id,
                            file_version,
                            stmnt,
                            files,
                            stmnt.1.end,
                            scoped_generic_types,
                        );
                    }
                }
            }
        }
        Statement::InfiniteLoop(_, block) => {
            return analyze_block(file_id, file_version, block, files, scoped_generic_types);
        }
        Statement::IterLoop(_, (vars, _), _, exp, block) => {
            let block_span = block.1.clone();

            let iter_type = match analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Array(Box::new(DataType::Any)),
                files,
                scoped_generic_types,
            ) {
                DataType::Array(ty) => *ty,
                _ => DataType::Any,
            };

            match &vars {
                IterLoopVars::WithIndex((var1, var1_span), (var2, var2_span)) => {
                    let mut symbol_table = files
                        .symbol_table
                        .entry(file)
                        .or_insert_with(|| Default::default());
                    insert_symbol_definition(
                        &mut symbol_table,
                        var1,
                        block_span.start..=block_span.end,
                        &SymbolLocation {
                            file,
                            start: var1_span.start,
                            end: var1_span.end,
                        },
                        DataType::Number,
                        SymbolType::Variable(VarSymbol {}),
                        false,
                    );

                    insert_symbol_definition(
                        &mut symbol_table,
                        var2,
                        block_span.start..=block_span.end,
                        &SymbolLocation {
                            file,
                            start: var2_span.start,
                            end: var2_span.end,
                        },
                        iter_type,
                        SymbolType::Variable(VarSymbol {}),
                        false,
                    );
                }
                IterLoopVars::Single((var, var_span)) => {
                    let mut symbol_table = files
                        .symbol_table
                        .entry(file)
                        .or_insert_with(|| Default::default());
                    insert_symbol_definition(
                        &mut symbol_table,
                        var,
                        block_span.start..=block_span.end,
                        &SymbolLocation {
                            file,
                            start: var_span.start,
                            end: var_span.end,
                        },
                        iter_type,
                        SymbolType::Variable(VarSymbol {}),
                        false,
                    );
                }
                _ => {}
            }

            return analyze_block(file_id, file_version, block, files, scoped_generic_types);
        }
        Statement::VariableInit(_, (var_name, var_span), (value, _)) => {
            let var_type = match value {
                VariableInitType::Expression(exp) => analyze_exp(
                    file_id,
                    file_version,
                    exp,
                    DataType::Any,
                    files,
                    scoped_generic_types,
                ),
                VariableInitType::DataType((ty, _)) => ty.clone(),
                _ => DataType::Error,
            };

            let mut symbol_table = files
                .symbol_table
                .entry(file)
                .or_insert_with(|| Default::default());
            insert_symbol_definition(
                &mut symbol_table,
                var_name,
                span.end..=scope_end,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types.deref_type(&var_type),
                SymbolType::Variable(VarSymbol {}),
                false,
            );
        }
        Statement::Echo(_, exp) => {
            analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Any,
                files,
                scoped_generic_types,
            );
        }
        Statement::Expression(exp) => {
            analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Any,
                files,
                scoped_generic_types,
            );
        }
        Statement::Fail(_, exp) => {
            if let Some(exp) = exp {
                analyze_exp(
                    file_id,
                    file_version,
                    exp,
                    DataType::Number,
                    files,
                    scoped_generic_types,
                );
            }
        }
        Statement::Return(_, exp) => {
            if let Some(exp) = exp {
                let ty = analyze_exp(
                    file_id,
                    file_version,
                    exp,
                    DataType::Any,
                    files,
                    scoped_generic_types,
                );

                return Some(ty);
            }

            return Some(DataType::Null);
        }
        Statement::ShorthandAdd((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, &var, &file, var_span.start) {
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

            let exp_ty = analyze_exp(
                file_id,
                file_version,
                exp,
                var_ty.clone(),
                files,
                scoped_generic_types,
            );

            if !matches_type(&default_ty, &var_ty, scoped_generic_types)
                || !matches_type(&exp_ty, &var_ty, scoped_generic_types)
            {
                files.report_error(
                    &file,
                    &format!(
                        "Cannot add to variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    var_span.clone(),
                );
            }

            insert_symbol_reference(
                &var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
            );
        }
        Statement::ShorthandDiv((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, &var, &file, var_span.start) {
                Some(info) => info.data_type,
                None => DataType::Any,
            };

            if !matches_type(&DataType::Number, &var_ty, scoped_generic_types) {
                files.report_error(
                    &file,
                    &format!(
                        "Cannot divide variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    var_span.clone(),
                );
            }

            analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Number,
                files,
                scoped_generic_types,
            );

            insert_symbol_reference(
                &var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
            );
        }
        Statement::ShorthandModulo((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, &var, &file, var_span.start) {
                Some(info) => info.data_type,
                None => DataType::Any,
            };

            if !matches_type(&DataType::Number, &var_ty, scoped_generic_types) {
                files.report_error(
                    &file,
                    &format!(
                        "Cannot use modulo with variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    var_span.clone(),
                );
            }

            analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Number,
                files,
                scoped_generic_types,
            );

            insert_symbol_reference(
                &var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
            );
        }
        Statement::ShorthandMul((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, &var, &file, var_span.start) {
                Some(info) => info.data_type,
                None => DataType::Any,
            };

            if !matches_type(&DataType::Number, &var_ty, scoped_generic_types) {
                files.report_error(
                    &file,
                    &format!(
                        "Cannot use multiply with variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    var_span.clone(),
                );
            }

            analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Number,
                files,
                scoped_generic_types,
            );

            insert_symbol_reference(
                &var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
            );
        }
        Statement::ShorthandSub((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, &var, &file, var_span.start) {
                Some(info) => info.data_type,
                None => DataType::Any,
            };

            if !matches_type(&DataType::Number, &var_ty, scoped_generic_types) {
                files.report_error(
                    &file,
                    &format!(
                        "Cannot use subtract with variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    var_span.clone(),
                );
            }

            analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Number,
                files,
                scoped_generic_types,
            );

            insert_symbol_reference(
                &var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
            );
        }
        Statement::VariableSet((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, &var, &file, var_span.start) {
                Some(info) => info.data_type,
                None => DataType::Any,
            };

            analyze_exp(
                file_id,
                file_version,
                exp,
                var_ty,
                files,
                scoped_generic_types,
            );

            insert_symbol_reference(
                &var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
            );
        }
        _ => {}
    };

    None
}

pub fn analyze_block(
    file_id: FileId,
    file_version: FileVersion,
    (block, span): &Spanned<Block>,
    files: &Files,
    scoped_generic_types: &GenericsMap,
) -> Option<DataType> {
    let mut types: Vec<DataType> = vec![];
    if let Block::Block(_, stmnt) = block {
        for stmnt in stmnt.iter() {
            if let Some(ty) = analyze_stmnt(
                file_id,
                file_version,
                stmnt,
                files,
                span.end,
                scoped_generic_types,
            ) {
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
    file_id: FileId,
    file_version: FileVersion,
    (failure, span): &Spanned<FailureHandler>,
    files: &Files,
    scoped_generic_types: &GenericsMap,
) {
    match failure {
        FailureHandler::Handle(_, stmnts) => {
            stmnts.iter().for_each(|stmnt| {
                analyze_stmnt(
                    file_id,
                    file_version,
                    stmnt,
                    files,
                    span.end,
                    scoped_generic_types,
                );
            });
        }
        _ => {}
    }
}
