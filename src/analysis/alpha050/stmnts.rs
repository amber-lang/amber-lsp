use crate::{
    analysis::{
        get_symbol_definition_info, insert_symbol_definition, insert_symbol_reference,
        types::{make_union_type, matches_type, GenericsMap},
        BlockContext, Context, DataType, SymbolInfo, SymbolLocation, SymbolType, VariableSymbol,
    },
    files::{FileVersion, Files},
    grammar::{
        alpha050::{
            Block, Comment, ElseCondition, FailableHandler, FailureHandler, IfChainContent,
            IfCondition, IterLoopVars, Statement, VariableInitType,
        },
        CommandModifier, Spanned,
    },
    paths::FileId,
};

use super::exp::{analyze_exp, ExpAnalysisResult};

#[derive(Debug, Clone)]
pub struct StmntAnalysisResult {
    pub is_propagating_failure: bool,
    pub return_ty: Option<DataType>,
}

/// Analyze a statement.
///
/// Returns the data type of the return statement and a boolean indicating if the
/// statement is propagating a failure.
#[tracing::instrument(skip_all)]
pub fn analyze_stmnt(
    file_id: FileId,
    file_version: FileVersion,
    (stmnt, span): &Spanned<Statement>,
    files: &Files,
    scope_end: usize,
    scoped_generic_types: &GenericsMap,
    contexts: &mut Vec<Context>,
) -> StmntAnalysisResult {
    let file = (file_id, file_version);

    match stmnt {
        Statement::Block(block) => analyze_block(
            file_id,
            file_version,
            block,
            files,
            scoped_generic_types,
            contexts,
        ),
        Statement::IfChain(_, if_chain) => {
            let mut stmnts = vec![];
            let mut exps = vec![];

            for (if_chain_content, _) in if_chain.iter() {
                match if_chain_content {
                    IfChainContent::IfCondition((condition, _)) => match condition {
                        IfCondition::IfCondition(exp, block) => {
                            let exp = analyze_exp(
                                file_id,
                                file_version,
                                exp,
                                DataType::Boolean,
                                files,
                                scoped_generic_types,
                                contexts,
                            );
                            let stmnt = analyze_block(
                                file_id,
                                file_version,
                                block,
                                files,
                                scoped_generic_types,
                                contexts,
                            );

                            exps.push(exp);
                            stmnts.push(stmnt);
                        }
                        IfCondition::Error => {}
                        IfCondition::Comment((Comment::DocString(docs), _)) => {
                            let _ = handle_doc_strings(docs, contexts);
                        }
                        IfCondition::Comment(_) => {}
                    },
                    IfChainContent::Else((else_cond, _)) => match else_cond {
                        ElseCondition::Else(_, block) => {
                            let stmnt = analyze_block(
                                file_id,
                                file_version,
                                block,
                                files,
                                scoped_generic_types,
                                contexts,
                            );

                            stmnts.push(stmnt);
                        }
                    },
                    IfChainContent::Comment((Comment::DocString(docs), _)) => {
                        let _ = handle_doc_strings(docs, contexts);
                    }
                    IfChainContent::Comment(_) => {}
                }
            }

            get_stmnt_analysis_result(stmnts, exps)
        }
        Statement::IfCondition(_, if_cond, comments, else_cond) => {
            let mut stmnts = vec![];
            let mut exps = vec![];

            match &if_cond.0 {
                IfCondition::IfCondition(exp, block) => {
                    let exp = analyze_exp(
                        file_id,
                        file_version,
                        exp,
                        DataType::Boolean,
                        files,
                        scoped_generic_types,
                        contexts,
                    );
                    let block = analyze_block(
                        file_id,
                        file_version,
                        block,
                        files,
                        scoped_generic_types,
                        contexts,
                    );

                    stmnts.push(block);
                    exps.push(exp);
                }
                IfCondition::Comment((Comment::DocString(docs), _)) => {
                    let _ = handle_doc_strings(docs, contexts);
                }
                _ => {}
            }

            comments.iter().for_each(|(comment, _)| {
                if let Comment::DocString(docs) = comment {
                    let _ = handle_doc_strings(docs, contexts);
                }
            });

            if let Some(else_cond) = else_cond {
                match &else_cond.0 {
                    ElseCondition::Else(_, block) => {
                        let block = analyze_block(
                            file_id,
                            file_version,
                            block,
                            files,
                            scoped_generic_types,
                            contexts,
                        );

                        stmnts.push(block);
                    }
                }
            }

            get_stmnt_analysis_result(stmnts, exps)
        }
        Statement::InfiniteLoop(_, block) => {
            let mut new_contexts = contexts.clone();
            new_contexts.push(Context::Loop);

            analyze_block(
                file_id,
                file_version,
                block,
                files,
                scoped_generic_types,
                &new_contexts,
            )
        }
        Statement::WhileLoop(_, exp, block) => {
            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Boolean,
                files,
                scoped_generic_types,
                contexts,
            );

            let mut new_contexts = contexts.clone();
            new_contexts.push(Context::Loop);

            let block_analysis = analyze_block(
                file_id,
                file_version,
                block,
                files,
                scoped_generic_types,
                &new_contexts,
            );

            get_stmnt_analysis_result(vec![block_analysis], vec![exp_analysis])
        }
        Statement::IterLoop(_, (vars, _), _, exp, block) => {
            let block_span = block.1;

            let exp = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Array(Box::new(DataType::Any)),
                files,
                scoped_generic_types,
                contexts,
            );

            // TODO: Check if DataType is generic
            let iter_type = match exp.exp_ty.clone() {
                DataType::Array(ty) => *ty,
                DataType::Failable(ty) => {
                    if let DataType::Array(inner_ty) = *ty {
                        *inner_ty
                    } else {
                        DataType::Any
                    }
                }
                _ => DataType::Any,
            };

            match &vars {
                IterLoopVars::WithIndex((var1, var1_span), (var2, var2_span)) => {
                    let mut symbol_table = files.symbol_table.entry(file).or_default();
                    insert_symbol_definition(
                        &mut symbol_table,
                        &SymbolInfo {
                            name: var1.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                            data_type: DataType::Int,
                            is_definition: true,
                            undefined: false,
                            span: *var1_span,
                            contexts: contexts.clone(),
                        },
                        file,
                        block_span.start..=block_span.end,
                        false,
                    );

                    insert_symbol_definition(
                        &mut symbol_table,
                        &SymbolInfo {
                            name: var2.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                            data_type: iter_type,
                            is_definition: true,
                            undefined: false,
                            span: *var2_span,
                            contexts: contexts.clone(),
                        },
                        file,
                        block_span.start..=block_span.end,
                        false,
                    );
                }
                IterLoopVars::Single((var, var_span)) => {
                    let mut symbol_table = files.symbol_table.entry(file).or_default();
                    insert_symbol_definition(
                        &mut symbol_table,
                        &SymbolInfo {
                            name: var.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                            data_type: iter_type,
                            is_definition: true,
                            undefined: false,
                            span: *var_span,
                            contexts: contexts.clone(),
                        },
                        file,
                        block_span.start..=block_span.end,
                        false,
                    );
                }
                _ => {}
            }

            let mut new_contexts = contexts.clone();
            new_contexts.push(Context::Loop);

            let block = analyze_block(
                file_id,
                file_version,
                block,
                files,
                scoped_generic_types,
                &new_contexts,
            );

            get_stmnt_analysis_result(vec![block], vec![exp])
        }
        Statement::VariableInit(_, (var_name, var_span), (value, _)) => {
            let exp = match value {
                VariableInitType::Expression(exp) => analyze_exp(
                    file_id,
                    file_version,
                    exp,
                    DataType::Any,
                    files,
                    scoped_generic_types,
                    contexts,
                ),
                VariableInitType::DataType((ty, _)) => ExpAnalysisResult {
                    exp_ty: ty.clone(),
                    is_propagating_failure: false,
                    return_ty: None,
                },
                _ => ExpAnalysisResult {
                    exp_ty: DataType::Error,
                    is_propagating_failure: false,
                    return_ty: None,
                },
            };

            let mut symbol_table = files.symbol_table.entry(file).or_default();

            let var_type = match exp.exp_ty {
                DataType::Failable(ty) => scoped_generic_types.deref_type(&ty),
                ty => scoped_generic_types.deref_type(&ty),
            };

            insert_symbol_definition(
                &mut symbol_table,
                &SymbolInfo {
                    name: var_name.to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                    data_type: var_type,
                    is_definition: true,
                    undefined: false,
                    span: *var_span,
                    contexts: contexts.clone(),
                },
                file,
                span.end..=scope_end,
                false,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::ConstInit(_, (var_name, var_span), exp) => {
            let exp = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );

            let mut symbol_table = files.symbol_table.entry(file).or_default();

            let var_type = match exp.exp_ty {
                DataType::Failable(ty) => scoped_generic_types.deref_type(&ty),
                ty => scoped_generic_types.deref_type(&ty),
            };

            insert_symbol_definition(
                &mut symbol_table,
                &SymbolInfo {
                    name: var_name.to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbol { is_const: true }),
                    data_type: var_type,
                    is_definition: true,
                    undefined: false,
                    span: *var_span,
                    contexts: contexts.clone(),
                },
                file,
                span.end..=scope_end,
                false,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::Echo(_, exp) => {
            let exp = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::Expression(exp) => {
            let exp = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Any,
                files,
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::Fail(_, exp) => {
            if !contexts
                .iter()
                .any(|c| matches!(c, Context::Function(_) | Context::Main))
            {
                files.report_error(
                    &file,
                    "Fail statements can only be used inside of functions or the main block",
                    *span,
                );
            }

            if let Some(exp) = exp {
                let exp = analyze_exp(
                    file_id,
                    file_version,
                    exp,
                    DataType::Int,
                    files,
                    scoped_generic_types,
                    contexts,
                );

                return StmntAnalysisResult {
                    is_propagating_failure: exp.is_propagating_failure,
                    return_ty: exp.return_ty,
                };
            }

            StmntAnalysisResult {
                is_propagating_failure: true,
                return_ty: None,
            }
        }
        Statement::Return(_, exp) => {
            if !contexts.iter().any(|c| matches!(c, Context::Function(_))) {
                files.report_error(&file, "Return statement outside of function", *span);
            }

            if let Some(exp) = exp {
                let exp_analysis = analyze_exp(
                    file_id,
                    file_version,
                    exp,
                    DataType::Any,
                    files,
                    scoped_generic_types,
                    contexts,
                );

                if let Some(ty) = exp_analysis.return_ty {
                    return StmntAnalysisResult {
                        is_propagating_failure: exp_analysis.is_propagating_failure,
                        return_ty: Some(make_union_type(vec![ty, exp_analysis.exp_ty])),
                    };
                }

                return StmntAnalysisResult {
                    is_propagating_failure: exp_analysis.is_propagating_failure,
                    return_ty: Some(exp_analysis.exp_ty),
                };
            }

            StmntAnalysisResult {
                is_propagating_failure: false,
                return_ty: None,
            }
        }
        Statement::ShorthandAdd((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, var, &file, var_span.start) {
                Some(info) => {
                    match info.symbol_type {
                        SymbolType::Function(_) => {
                            files.report_error(&file, "Cannot assign to a function", *var_span);
                        }
                        SymbolType::Variable(var) if var.is_const => {
                            files.report_error(&file, "Cannot assign to a constant", *var_span);
                        }
                        _ => {}
                    }

                    info.data_type
                }
                None => DataType::Any,
            };

            let default_ty = DataType::Union(vec![
                DataType::Text,
                DataType::Number,
                DataType::Int,
                DataType::Array(Box::new(DataType::Union(vec![
                    DataType::Text,
                    DataType::Number,
                    DataType::Int,
                ]))),
            ]);

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                var_ty.clone(),
                files,
                scoped_generic_types,
                contexts,
            );

            if !matches_type(&default_ty, &var_ty, scoped_generic_types)
                || !matches_type(&exp_analysis.exp_ty, &var_ty, scoped_generic_types)
            {
                files.report_error(
                    &file,
                    &format!(
                        "Cannot add to variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    *var_span,
                );
            }

            if let DataType::Generic(id) = var_ty {
                scoped_generic_types.constrain_generic_type(id, exp_analysis.exp_ty);
            }

            insert_symbol_reference(
                var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
        }
        Statement::ShorthandDiv((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, var, &file, var_span.start) {
                Some(info) => {
                    match info.symbol_type {
                        SymbolType::Function(_) => {
                            files.report_error(&file, "Cannot assign to a function", *var_span);
                        }
                        SymbolType::Variable(var) if var.is_const => {
                            files.report_error(&file, "Cannot assign to a constant", *var_span);
                        }
                        _ => {}
                    }

                    info.data_type
                }
                None => DataType::Any,
            };

            if !matches_type(
                &DataType::Union([DataType::Number, DataType::Int].to_vec()),
                &var_ty,
                scoped_generic_types,
            ) {
                files.report_error(
                    &file,
                    &format!(
                        "Cannot divide variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    *var_span,
                );
            }

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
                files,
                scoped_generic_types,
                contexts,
            );

            if let DataType::Generic(id) = var_ty {
                scoped_generic_types.constrain_generic_type(id, exp_analysis.exp_ty);
            }

            insert_symbol_reference(
                var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
        }
        Statement::ShorthandModulo((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, var, &file, var_span.start) {
                Some(info) => {
                    match info.symbol_type {
                        SymbolType::Function(_) => {
                            files.report_error(&file, "Cannot assign to a function", *var_span);
                        }
                        SymbolType::Variable(var) if var.is_const => {
                            files.report_error(&file, "Cannot assign to a constant", *var_span);
                        }
                        _ => {}
                    }

                    info.data_type
                }
                None => DataType::Any,
            };

            if !matches_type(
                &DataType::Union([DataType::Number, DataType::Int].to_vec()),
                &var_ty,
                scoped_generic_types,
            ) {
                files.report_error(
                    &file,
                    &format!(
                        "Cannot use modulo with variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    *var_span,
                );
            }

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
                files,
                scoped_generic_types,
                contexts,
            );

            if let DataType::Generic(id) = var_ty {
                scoped_generic_types.constrain_generic_type(id, exp_analysis.exp_ty);
            }

            insert_symbol_reference(
                var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
        }
        Statement::ShorthandMul((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, var, &file, var_span.start) {
                Some(info) => {
                    match info.symbol_type {
                        SymbolType::Function(_) => {
                            files.report_error(&file, "Cannot assign to a function", *var_span);
                        }
                        SymbolType::Variable(var) if var.is_const => {
                            files.report_error(&file, "Cannot assign to a constant", *var_span);
                        }
                        _ => {}
                    }

                    info.data_type
                }
                None => DataType::Any,
            };

            if !matches_type(
                &DataType::Union([DataType::Number, DataType::Int].to_vec()),
                &var_ty,
                scoped_generic_types,
            ) {
                files.report_error(
                    &file,
                    &format!(
                        "Cannot use multiply with variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    *var_span,
                );
            }

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
                files,
                scoped_generic_types,
                contexts,
            );

            if let DataType::Generic(id) = var_ty {
                scoped_generic_types.constrain_generic_type(id, exp_analysis.exp_ty);
            }

            insert_symbol_reference(
                var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
        }
        Statement::ShorthandSub((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, var, &file, var_span.start) {
                Some(info) => {
                    match info.symbol_type {
                        SymbolType::Function(_) => {
                            files.report_error(&file, "Cannot assign to a function", *var_span);
                        }
                        SymbolType::Variable(var) if var.is_const => {
                            files.report_error(&file, "Cannot assign to a constant", *var_span);
                        }
                        _ => {}
                    }

                    info.data_type
                }
                None => DataType::Any,
            };

            if !matches_type(
                &DataType::Union([DataType::Number, DataType::Int].to_vec()),
                &var_ty,
                scoped_generic_types,
            ) {
                files.report_error(
                    &file,
                    &format!(
                        "Cannot use subtract with variable of type {}",
                        var_ty.to_string(scoped_generic_types)
                    ),
                    *var_span,
                );
            }

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Union([DataType::Number, DataType::Int].to_vec()),
                files,
                scoped_generic_types,
                contexts,
            );

            if let DataType::Generic(id) = var_ty {
                scoped_generic_types.constrain_generic_type(id, exp_analysis.exp_ty);
            }

            insert_symbol_reference(
                var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
        }
        Statement::VariableSet((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, var, &file, var_span.start) {
                Some(info) => {
                    match info.symbol_type {
                        SymbolType::Function(_) => {
                            files.report_error(&file, "Cannot assign to a function", *var_span);
                        }
                        SymbolType::Variable(var) if var.is_const => {
                            files.report_error(&file, "Cannot assign to a constant", *var_span);
                        }
                        _ => {}
                    }

                    info.data_type
                }
                None => DataType::Any,
            };

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                var_ty,
                files,
                scoped_generic_types,
                contexts,
            );

            insert_symbol_reference(
                var,
                files,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
        }
        Statement::Break => {
            if !contexts.iter().any(|c| matches!(c, Context::Loop)) {
                files.report_error(&file, "Break statement outside of loop", *span);
            }

            StmntAnalysisResult {
                is_propagating_failure: false,
                return_ty: None,
            }
        }
        Statement::Continue => {
            if !contexts.iter().any(|c| matches!(c, Context::Loop)) {
                files.report_error(&file, "Continue statement outside of loop", *span);
            }

            StmntAnalysisResult {
                is_propagating_failure: false,
                return_ty: None,
            }
        }
        Statement::Cd(_, exp) => {
            let exp = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Text,
                files,
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::MoveFiles(modifiers, _, from_exp, to_exp, failable_handlers) => {
            let exp1 = analyze_exp(
                file_id,
                file_version,
                from_exp,
                DataType::Text,
                files,
                scoped_generic_types,
                contexts,
            );
            let exp2 = analyze_exp(
                file_id,
                file_version,
                to_exp,
                DataType::Text,
                files,
                scoped_generic_types,
                contexts,
            );

            let stmnt = analyze_failable_handlers(
                file_id,
                file_version,
                failable_handlers,
                files,
                scoped_generic_types,
                contexts,
            );

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
                files.report_error(&file, "Command must have a failure handler", *span);
            }

            get_stmnt_analysis_result(vec![stmnt], vec![exp1, exp2])
        }
        Statement::Comment((Comment::DocString(docs), _)) => handle_doc_strings(docs, contexts),
        Statement::Comment(_) | Statement::Shebang(_) | Statement::Error => StmntAnalysisResult {
            is_propagating_failure: false,
            return_ty: None,
        },
    }
}

fn handle_doc_strings(docs: &String, contexts: &mut Vec<Context>) -> StmntAnalysisResult {
    match contexts.last() {
        Some(Context::DocString(doc_string)) => {
            let new_doc_string = format!("{doc_string}\n{docs}");
            *contexts.last_mut().unwrap() = Context::DocString(new_doc_string);
        }
        _ => {
            contexts.push(Context::DocString(docs.clone()));
        }
    }

    StmntAnalysisResult {
        is_propagating_failure: false,
        return_ty: None,
    }
}

pub fn analyze_block(
    file_id: FileId,
    file_version: FileVersion,
    (block, span): &Spanned<Block>,
    files: &Files,
    scoped_generic_types: &GenericsMap,
    contexts: &[Context],
) -> StmntAnalysisResult {
    let mut types: Vec<DataType> = vec![];

    let mut is_propagating = false;

    match block {
        Block::Block(modifiers, stmnt) => {
            let mut new_contexts = contexts.to_owned();
            new_contexts.push(Context::Block(BlockContext {
                modifiers: modifiers.iter().map(|(m, _)| m.clone()).collect(),
            }));

            for stmnt in stmnt.iter() {
                let StmntAnalysisResult {
                    return_ty,
                    is_propagating_failure,
                } = analyze_stmnt(
                    file_id,
                    file_version,
                    stmnt,
                    files,
                    span.end,
                    scoped_generic_types,
                    &mut new_contexts,
                );

                if let Some(ty) = return_ty {
                    types.push(ty);
                }

                is_propagating |= is_propagating_failure;
            }
        }
        Block::Singleline(stmnt) => {
            let mut working_contexts = contexts.to_owned();
            let StmntAnalysisResult {
                return_ty,
                is_propagating_failure,
            } = analyze_stmnt(
                file_id,
                file_version,
                stmnt,
                files,
                span.end,
                scoped_generic_types,
                &mut working_contexts,
            );

            if let Some(ty) = return_ty {
                types.push(ty);
            }

            is_propagating |= is_propagating_failure;
        }
        Block::Error => {}
    }

    if types.is_empty() {
        StmntAnalysisResult {
            is_propagating_failure: is_propagating,
            return_ty: None,
        }
    } else {
        StmntAnalysisResult {
            is_propagating_failure: is_propagating,
            return_ty: Some(make_union_type(types)),
        }
    }
}

pub fn analyze_failable_handlers(
    file_id: FileId,
    file_version: FileVersion,
    failable_handlers: &[Spanned<FailableHandler>],
    files: &Files,
    scoped_generic_types: &GenericsMap,
    contexts: &[Context],
) -> StmntAnalysisResult {
    let mut types: Vec<DataType> = vec![];
    let mut is_propagating = false;
    let mut contexts = contexts.to_owned();

    failable_handlers
        .iter()
        .for_each(|(handler, _)| match handler {
            FailableHandler::Failure((failure, span)) => {
                match failure {
                    FailureHandler::Handle(_, exit_code, block) => {
                        if let Some((code_var, code_var_span)) = exit_code {
                            let mut symbol_table = files
                                .symbol_table
                                .entry((file_id, file_version))
                                .or_default();

                            insert_symbol_definition(
                                &mut symbol_table,
                                &SymbolInfo {
                                    name: code_var.to_string(),
                                    symbol_type: SymbolType::Variable(VariableSymbol {
                                        is_const: false,
                                    }),
                                    data_type: DataType::Array(Box::new(DataType::Text)),
                                    is_definition: true,
                                    undefined: false,
                                    span: *code_var_span,
                                    contexts: vec![],
                                },
                                (file_id, file_version),
                                code_var_span.end..=span.end,
                                false,
                            );
                        }

                        let StmntAnalysisResult {
                            return_ty,
                            is_propagating_failure,
                        } = analyze_stmnt(
                            file_id,
                            file_version,
                            &(Statement::Block(*block.clone()), block.1),
                            files,
                            span.end,
                            scoped_generic_types,
                            &mut contexts,
                        );

                        types.extend(return_ty);
                        is_propagating |= is_propagating_failure;
                    }
                    FailureHandler::Propagate => {
                        if !contexts
                            .iter()
                            .any(|c| *c == Context::Main || matches!(c, Context::Function(_)))
                        {
                            files.report_error(
                                &(file_id, file_version),
                                "Propagate can only be used inside of main block or function",
                                *span,
                            );
                        }

                        is_propagating = true;
                    }
                };
            }
            FailableHandler::Succeeded(_, block) => {
                let StmntAnalysisResult {
                    return_ty,
                    is_propagating_failure,
                } = analyze_stmnt(
                    file_id,
                    file_version,
                    &(Statement::Block(*block.clone()), block.1),
                    files,
                    block.1.end,
                    scoped_generic_types,
                    &mut contexts,
                );

                types.extend(return_ty);
                is_propagating |= is_propagating_failure;
            }
            FailableHandler::Then(_, (code_var, code_var_span), block) => {
                let mut symbol_table = files
                    .symbol_table
                    .entry((file_id, file_version))
                    .or_default();

                insert_symbol_definition(
                    &mut symbol_table,
                    &SymbolInfo {
                        name: code_var.to_string(),
                        symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                        data_type: DataType::Array(Box::new(DataType::Text)),
                        is_definition: true,
                        undefined: false,
                        span: *code_var_span,
                        contexts: vec![],
                    },
                    (file_id, file_version),
                    code_var_span.end..=block.1.end,
                    false,
                );

                let StmntAnalysisResult {
                    return_ty,
                    is_propagating_failure,
                } = analyze_stmnt(
                    file_id,
                    file_version,
                    &(Statement::Block(*block.clone()), block.1),
                    files,
                    block.1.end,
                    scoped_generic_types,
                    &mut contexts,
                );

                types.extend(return_ty);
                is_propagating |= is_propagating_failure;
            }
        });

    if types.is_empty() {
        StmntAnalysisResult {
            is_propagating_failure: is_propagating,
            return_ty: None,
        }
    } else {
        StmntAnalysisResult {
            is_propagating_failure: is_propagating,
            return_ty: Some(make_union_type(types)),
        }
    }
}

fn get_stmnt_analysis_result(
    stmnt_analysis: Vec<StmntAnalysisResult>,
    exp_analysis: Vec<ExpAnalysisResult>,
) -> StmntAnalysisResult {
    let mut is_propagating_failure = false;
    let mut return_ty = vec![];

    for stmnt in stmnt_analysis {
        if stmnt.is_propagating_failure {
            is_propagating_failure = true;
        }
        if let Some(ty) = stmnt.return_ty {
            return_ty.push(ty);
        }
    }

    for exp in exp_analysis {
        if exp.is_propagating_failure {
            is_propagating_failure = true;
        }
        if let Some(ty) = exp.return_ty {
            return_ty.push(ty);
        }
    }

    StmntAnalysisResult {
        is_propagating_failure,
        return_ty: if !return_ty.is_empty() {
            Some(make_union_type(return_ty))
        } else {
            None
        },
    }
}
