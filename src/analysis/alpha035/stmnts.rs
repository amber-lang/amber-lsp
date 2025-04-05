use crate::{
    analysis::{
        get_symbol_definition_info, insert_symbol_definition, insert_symbol_reference,
        types::{make_union_type, matches_type, GenericsMap},
        BlockContext, Context, DataType, SymbolLocation, SymbolType,
    },
    files::{FileVersion, Files},
    grammar::{
        alpha035::{
            Block, ElseCondition, FailureHandler, IfChainContent, IfCondition, IterLoopVars,
            Statement, VariableInitType,
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
        Statement::Block(block) => {
            return analyze_block(
                file_id,
                file_version,
                block,
                files,
                scoped_generic_types,
                contexts,
            );
        }
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
                        IfCondition::InlineIfCondition(exp, boxed_stmnt) => {
                            let exp = analyze_exp(
                                file_id,
                                file_version,
                                exp,
                                DataType::Boolean,
                                files,
                                scoped_generic_types,
                                contexts,
                            );
                            let stmnt = analyze_stmnt(
                                file_id,
                                file_version,
                                boxed_stmnt,
                                files,
                                boxed_stmnt.1.end,
                                scoped_generic_types,
                                contexts,
                            );

                            exps.push(exp);
                            stmnts.push(stmnt);
                        }
                        IfCondition::Error => {}
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
                        ElseCondition::InlineElse(_, stmnt) => {
                            let stmnt = analyze_stmnt(
                                file_id,
                                file_version,
                                stmnt,
                                files,
                                stmnt.1.end,
                                scoped_generic_types,
                                contexts,
                            );

                            stmnts.push(stmnt);
                        }
                    },
                }
            }

            get_stmnt_analysis_result(stmnts, exps)
        }
        Statement::IfCondition(_, if_cond, else_cond) => {
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
                IfCondition::InlineIfCondition(exp, boxed_stmnt) => {
                    let exp = analyze_exp(
                        file_id,
                        file_version,
                        exp,
                        DataType::Boolean,
                        files,
                        scoped_generic_types,
                        contexts,
                    );
                    let stmnt = analyze_stmnt(
                        file_id,
                        file_version,
                        boxed_stmnt,
                        files,
                        boxed_stmnt.1.end,
                        scoped_generic_types,
                        contexts,
                    );

                    stmnts.push(stmnt);
                    exps.push(exp);
                }
                _ => {}
            }

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
                    ElseCondition::InlineElse(_, stmnt) => {
                        let stmnt = analyze_stmnt(
                            file_id,
                            file_version,
                            stmnt,
                            files,
                            stmnt.1.end,
                            scoped_generic_types,
                            contexts,
                        );

                        stmnts.push(stmnt);
                    }
                }
            }

            return get_stmnt_analysis_result(stmnts, exps);
        }
        Statement::InfiniteLoop(_, block) => {
            let mut new_contexts = contexts.clone();
            new_contexts.push(Context::Loop);
            return analyze_block(
                file_id,
                file_version,
                block,
                files,
                scoped_generic_types,
                &new_contexts,
            );
        }
        Statement::IterLoop(_, (vars, _), _, exp, block) => {
            let block_span = block.1.clone();

            let exp = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Array(Box::new(DataType::Any)),
                files,
                scoped_generic_types,
                contexts,
            );

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
                        SymbolType::Variable,
                        false,
                        contexts,
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
                        SymbolType::Variable,
                        false,
                        contexts,
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
                        SymbolType::Variable,
                        false,
                        contexts,
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

            let mut symbol_table = files
                .symbol_table
                .entry(file)
                .or_insert_with(|| Default::default());

            let var_type = match exp.exp_ty {
                DataType::Failable(ty) => scoped_generic_types.deref_type(&ty),
                ty => scoped_generic_types.deref_type(&ty),
            };

            insert_symbol_definition(
                &mut symbol_table,
                var_name,
                span.end..=scope_end,
                &SymbolLocation {
                    file,
                    start: var_span.start,
                    end: var_span.end,
                },
                var_type,
                SymbolType::Variable,
                false,
                contexts,
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
                    span.clone(),
                );
            }

            if let Some(exp) = exp {
                let exp = analyze_exp(
                    file_id,
                    file_version,
                    exp,
                    DataType::Number,
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
                files.report_error(&file, "Return statement outside of function", span.clone());
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
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
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

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
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
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
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

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
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
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
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

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
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
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
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

            let exp_analysis = analyze_exp(
                file_id,
                file_version,
                exp,
                DataType::Number,
                files,
                scoped_generic_types,
                contexts,
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
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
        }
        Statement::VariableSet((var, var_span), exp) => {
            let var_ty = match get_symbol_definition_info(files, &var, &file, var_span.start) {
                Some(info) => info.data_type,
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
                &var,
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
                files.report_error(&file, "Break statement outside of loop", span.clone());
            }

            StmntAnalysisResult {
                is_propagating_failure: false,
                return_ty: None,
            }
        }
        Statement::Continue => {
            if !contexts.iter().any(|c| matches!(c, Context::Loop)) {
                files.report_error(&file, "Continue statement outside of loop", span.clone());
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
        Statement::MoveFiles(modifiers, _, from_exp, to_exp, handler) => {
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

            if let Some(handler) = handler {
                let stmnt = analyze_failure_handler(
                    file_id,
                    file_version,
                    handler,
                    files,
                    scoped_generic_types,
                    contexts,
                );

                return get_stmnt_analysis_result(vec![stmnt], vec![exp1, exp2]);
            } else if !modifiers
                .iter()
                .any(|(modifier, _)| *modifier == CommandModifier::Unsafe)
            {
                files.report_error(&file, "Command must have a failure handler", *span);
            }

            return get_stmnt_analysis_result(vec![], vec![exp1, exp2]);
        }
        Statement::DocString(docs) => match contexts.last() {
            Some(Context::DocString(doc_string)) => {
                let new_doc_string = format!("{}\n{}", doc_string, docs);
                *contexts.last_mut().unwrap() = Context::DocString(new_doc_string);

                StmntAnalysisResult {
                    is_propagating_failure: false,
                    return_ty: None,
                }
            }
            _ => {
                contexts.push(Context::DocString(docs.clone()));

                StmntAnalysisResult {
                    is_propagating_failure: false,
                    return_ty: None,
                }
            }
        },
        Statement::Comment(_) | Statement::Shebang(_) | Statement::Error => StmntAnalysisResult {
            is_propagating_failure: false,
            return_ty: None,
        },
    }
}

pub fn analyze_block(
    file_id: FileId,
    file_version: FileVersion,
    (block, span): &Spanned<Block>,
    files: &Files,
    scoped_generic_types: &GenericsMap,
    contexts: &Vec<Context>,
) -> StmntAnalysisResult {
    let mut types: Vec<DataType> = vec![];

    let mut is_propagating = false;

    if let Block::Block(modifiers, stmnt) = block {
        let mut new_contexts = contexts.clone();
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

pub fn analyze_failure_handler(
    file_id: FileId,
    file_version: FileVersion,
    (failure, span): &Spanned<FailureHandler>,
    files: &Files,
    scoped_generic_types: &GenericsMap,
    contexts: &Vec<Context>,
) -> StmntAnalysisResult {
    let mut types: Vec<DataType> = vec![];
    let mut is_propagating = false;
    let mut contexts = contexts.clone();

    match failure {
        FailureHandler::Handle(_, stmnts) => {
            stmnts.iter().for_each(|stmnt| {
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
                    &mut contexts,
                );

                types.extend(return_ty);
                is_propagating |= is_propagating_failure;
            });
        }
        FailureHandler::Propagate => {
            if !contexts
                .iter()
                .any(|c| *c == Context::Main || matches!(c, Context::Function(_)))
            {
                files.report_error(
                    &(file_id, file_version),
                    "Propagate can only be used inside of main block or function",
                    span.clone(),
                );
            }

            is_propagating = true;
        }
    };

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
        return_ty: if return_ty.len() > 0 {
            Some(make_union_type(return_ty))
        } else {
            None
        },
    }
}
