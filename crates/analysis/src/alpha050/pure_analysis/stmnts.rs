//! Statement-level analysis: blocks, loops, variable operations, and failable handlers.

use amber_grammar::alpha050::{
    Block,
    Comment,
    ElseCondition,
    Expression,
    FailableHandler,
    FailureHandler,
    IfChainContent,
    IfCondition,
    IterLoopVars,
    Statement,
    VariableInitType,
};
use amber_grammar::{
    CommandModifier,
    Span,
    Spanned,
};
use amber_types::{
    DataType,
    PureGenericsMap,
};

use amber_types::matches_type_pure;

use crate::types::make_union_type;
use crate::{
    insert_symbol_definition,
    BlockContext,
    Context,
    SymbolInfo,
    SymbolType,
    VariableSymbol,
};

use super::context::PureContext;
use super::exp::analyze_exp_pure;
use super::utils::{
    get_stmnt_analysis_result,
    handle_doc_strings,
};
use super::{
    ExpAnalysisResult,
    StmntAnalysisResult,
};

// ─── Statement Analysis ───────────────────────────────────────────────────────

pub fn analyze_stmnt_pure(
    (stmnt, span): &Spanned<Statement>,
    ctx: &mut PureContext,
    scope_end: usize,
    scoped_generic_types: &mut PureGenericsMap,
    contexts: &mut Vec<Context>,
) -> StmntAnalysisResult {
    match stmnt {
        Statement::Block(block) => analyze_block_pure(block, ctx, scoped_generic_types, contexts),
        Statement::IfChain(_, if_chain) => {
            let mut stmnts = vec![];
            let mut exps = vec![];

            for (if_chain_content, _) in if_chain.iter() {
                match if_chain_content {
                    IfChainContent::IfCondition((condition, _)) => match condition {
                        IfCondition::IfCondition(exp, block) => {
                            let exp = analyze_exp_pure(
                                exp,
                                DataType::Boolean,
                                ctx,
                                scoped_generic_types,
                                contexts,
                            );
                            let stmnt =
                                analyze_block_pure(block, ctx, scoped_generic_types, contexts);
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
                            let stmnt =
                                analyze_block_pure(block, ctx, scoped_generic_types, contexts);
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
                    let exp = analyze_exp_pure(
                        exp,
                        DataType::Boolean,
                        ctx,
                        scoped_generic_types,
                        contexts,
                    );
                    let block = analyze_block_pure(block, ctx, scoped_generic_types, contexts);
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
                        let block = analyze_block_pure(block, ctx, scoped_generic_types, contexts);
                        stmnts.push(block);
                    }
                }
            }

            get_stmnt_analysis_result(stmnts, exps)
        }
        Statement::InfiniteLoop(_, block) => {
            let mut new_contexts = contexts.clone();
            new_contexts.push(Context::Loop);
            analyze_block_pure(block, ctx, scoped_generic_types, &new_contexts)
        }
        Statement::WhileLoop(_, exp, block) => {
            let exp_analysis =
                analyze_exp_pure(exp, DataType::Boolean, ctx, scoped_generic_types, contexts);
            let mut new_contexts = contexts.clone();
            new_contexts.push(Context::Loop);
            let block_analysis =
                analyze_block_pure(block, ctx, scoped_generic_types, &new_contexts);
            get_stmnt_analysis_result(vec![block_analysis], vec![exp_analysis])
        }
        Statement::IterLoop(_, (vars, _), _, exp, block) => {
            let block_span = block.1;
            let exp_result = analyze_exp_pure(
                exp,
                DataType::Array(Box::new(DataType::Any)),
                ctx,
                scoped_generic_types,
                contexts,
            );

            let iter_type = match exp_result.exp_ty.clone() {
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

            match vars {
                IterLoopVars::WithIndex((var1, var1_span), (var2, var2_span)) => {
                    insert_symbol_definition(
                        &mut ctx.symbol_table,
                        &SymbolInfo {
                            name: var1.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                            data_type: DataType::Int,
                            is_definition: true,
                            undefined: false,
                            span: *var1_span,
                            contexts: contexts.clone(),
                        },
                        ctx.file_key,
                        block_span.start..=block_span.end,
                        false,
                    );
                    insert_symbol_definition(
                        &mut ctx.symbol_table,
                        &SymbolInfo {
                            name: var2.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                            data_type: iter_type,
                            is_definition: true,
                            undefined: false,
                            span: *var2_span,
                            contexts: contexts.clone(),
                        },
                        ctx.file_key,
                        block_span.start..=block_span.end,
                        false,
                    );
                }
                IterLoopVars::Single((var, var_span)) => {
                    insert_symbol_definition(
                        &mut ctx.symbol_table,
                        &SymbolInfo {
                            name: var.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                            data_type: iter_type,
                            is_definition: true,
                            undefined: false,
                            span: *var_span,
                            contexts: contexts.clone(),
                        },
                        ctx.file_key,
                        block_span.start..=block_span.end,
                        false,
                    );
                }
                _ => {}
            }

            let mut new_contexts = contexts.clone();
            new_contexts.push(Context::Loop);
            let block_result = analyze_block_pure(block, ctx, scoped_generic_types, &new_contexts);
            get_stmnt_analysis_result(vec![block_result], vec![exp_result])
        }
        Statement::VariableInit(_, (var_name, var_span), (value, _)) => {
            let exp = match value {
                VariableInitType::Expression(exp) => {
                    analyze_exp_pure(exp, DataType::Any, ctx, scoped_generic_types, contexts)
                }
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

            let var_type = match exp.exp_ty {
                DataType::Failable(ref ty) => scoped_generic_types.deref_type(ty),
                ref ty => scoped_generic_types.deref_type(ty),
            };

            insert_symbol_definition(
                &mut ctx.symbol_table,
                &SymbolInfo {
                    name: var_name.to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                    data_type: var_type,
                    is_definition: true,
                    undefined: false,
                    span: *var_span,
                    contexts: contexts.clone(),
                },
                ctx.file_key,
                span.end..=scope_end,
                false,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::ConstInit(_, (var_name, var_span), exp) => {
            let exp = analyze_exp_pure(exp, DataType::Any, ctx, scoped_generic_types, contexts);

            let var_type = match exp.exp_ty {
                DataType::Failable(ref ty) => scoped_generic_types.deref_type(ty),
                ref ty => scoped_generic_types.deref_type(ty),
            };

            insert_symbol_definition(
                &mut ctx.symbol_table,
                &SymbolInfo {
                    name: var_name.to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbol { is_const: true }),
                    data_type: var_type,
                    is_definition: true,
                    undefined: false,
                    span: *var_span,
                    contexts: contexts.clone(),
                },
                ctx.file_key,
                span.end..=scope_end,
                false,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::Echo(_, exp) => {
            let exp = analyze_exp_pure(exp, DataType::Any, ctx, scoped_generic_types, contexts);
            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::Expression(exp) => {
            let exp = analyze_exp_pure(exp, DataType::Any, ctx, scoped_generic_types, contexts);
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
                ctx.report_error(
                    "Fail statements can only be used inside of functions or the main block",
                    *span,
                );
            }
            if let Some(exp) = exp {
                let exp = analyze_exp_pure(exp, DataType::Int, ctx, scoped_generic_types, contexts);
                return StmntAnalysisResult {
                    is_propagating_failure: true,
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
                ctx.report_error("Return statement outside of function", *span);
            }
            if let Some(exp) = exp {
                let exp_analysis =
                    analyze_exp_pure(exp, DataType::Any, ctx, scoped_generic_types, contexts);
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
        Statement::ShorthandAdd((var, var_span), exp) => analyze_shorthand_op(
            ctx,
            scoped_generic_types,
            contexts,
            var,
            var_span,
            exp,
            ShorthandOp::Add,
        ),
        Statement::ShorthandSub((var, var_span), exp) => analyze_shorthand_op(
            ctx,
            scoped_generic_types,
            contexts,
            var,
            var_span,
            exp,
            ShorthandOp::Sub,
        ),
        Statement::ShorthandMul((var, var_span), exp) => analyze_shorthand_op(
            ctx,
            scoped_generic_types,
            contexts,
            var,
            var_span,
            exp,
            ShorthandOp::Mul,
        ),
        Statement::ShorthandDiv((var, var_span), exp) => analyze_shorthand_op(
            ctx,
            scoped_generic_types,
            contexts,
            var,
            var_span,
            exp,
            ShorthandOp::Div,
        ),
        Statement::ShorthandModulo((var, var_span), exp) => analyze_shorthand_op(
            ctx,
            scoped_generic_types,
            contexts,
            var,
            var_span,
            exp,
            ShorthandOp::Modulo,
        ),
        Statement::VariableSet((var, var_span), exp) => {
            let var_ty = match ctx.get_definition_info(var, var_span.start) {
                Some(info) => {
                    match info.symbol_type {
                        SymbolType::Function(_) => {
                            ctx.report_error("Cannot assign to a function", *var_span);
                        }
                        SymbolType::Variable(ref v) if v.is_const => {
                            ctx.report_error("Cannot assign to a constant", *var_span);
                        }
                        _ => {}
                    }
                    info.data_type
                }
                None => DataType::Any,
            };

            let exp_analysis = analyze_exp_pure(exp, var_ty, ctx, scoped_generic_types, contexts);

            ctx.insert_reference(
                var,
                var_span.start,
                var_span.end,
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
                ctx.report_error("Break statement outside of loop", *span);
            }
            StmntAnalysisResult {
                is_propagating_failure: false,
                return_ty: None,
            }
        }
        Statement::Continue => {
            if !contexts.iter().any(|c| matches!(c, Context::Loop)) {
                ctx.report_error("Continue statement outside of loop", *span);
            }
            StmntAnalysisResult {
                is_propagating_failure: false,
                return_ty: None,
            }
        }
        Statement::Cd(_, exp) => {
            let exp = analyze_exp_pure(exp, DataType::Text, ctx, scoped_generic_types, contexts);
            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::MoveFiles(modifiers, _, from_exp, to_exp, failable_handlers) => {
            let exp1 = analyze_exp_pure(
                from_exp,
                DataType::Text,
                ctx,
                scoped_generic_types,
                contexts,
            );
            let exp2 =
                analyze_exp_pure(to_exp, DataType::Text, ctx, scoped_generic_types, contexts);
            let stmnt = analyze_failable_handlers_pure(
                failable_handlers,
                ctx,
                scoped_generic_types,
                contexts,
            );

            let has_failure_handler = failable_handlers.iter().any(|(modifier, _)| {
                matches!(modifier, FailableHandler::Failure(_))
                    || matches!(modifier, FailableHandler::Exited(_, _, _))
            });

            if !has_failure_handler
                && !modifiers.iter().any(|(modifier, _)| {
                    *modifier == CommandModifier::Unsafe || *modifier == CommandModifier::Trust
                })
                && !contexts.iter().any(|c| match c {
                    Context::Block(BlockContext { modifiers }) => modifiers
                        .iter()
                        .any(|m| *m == CommandModifier::Unsafe || *m == CommandModifier::Trust),
                    _ => false,
                })
            {
                ctx.report_error("Command must have a failure handler", *span);
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

// ─── Shorthand Ops ────────────────────────────────────────────────────────────

#[derive(Clone, Copy)]
enum ShorthandOp {
    Add,
    Sub,
    Mul,
    Div,
    Modulo,
}

fn analyze_shorthand_op(
    ctx: &mut PureContext,
    scoped_generic_types: &mut PureGenericsMap,
    contexts: &[Context],
    var: &str,
    var_span: &Span,
    exp: &Spanned<Expression>,
    op: ShorthandOp,
) -> StmntAnalysisResult {
    let var_ty = match ctx.get_definition_info(var, var_span.start) {
        Some(info) => {
            match info.symbol_type {
                SymbolType::Function(_) => {
                    ctx.report_error("Cannot assign to a function", *var_span);
                }
                SymbolType::Variable(ref v) if v.is_const => {
                    ctx.report_error("Cannot assign to a constant", *var_span);
                }
                _ => {}
            }
            info.data_type
        }
        None => DataType::Any,
    };

    let (valid_types, expected_exp_ty, error_verb) = match op {
        ShorthandOp::Add => {
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
            (default_ty, var_ty.clone(), "add to")
        }
        ShorthandOp::Sub => (
            DataType::Union(vec![DataType::Number, DataType::Int]),
            DataType::Union(vec![DataType::Number, DataType::Int]),
            "subtract with",
        ),
        ShorthandOp::Mul => (
            DataType::Union(vec![DataType::Number, DataType::Int]),
            DataType::Union(vec![DataType::Number, DataType::Int]),
            "multiply with",
        ),
        ShorthandOp::Div => (
            DataType::Union(vec![DataType::Number, DataType::Int]),
            DataType::Union(vec![DataType::Number, DataType::Int]),
            "divide",
        ),
        ShorthandOp::Modulo => (
            DataType::Union(vec![DataType::Number, DataType::Int]),
            DataType::Union(vec![DataType::Number, DataType::Int]),
            "use modulo with",
        ),
    };

    let exp_analysis = analyze_exp_pure(
        exp,
        match op {
            ShorthandOp::Add => var_ty.clone(),
            _ => expected_exp_ty,
        },
        ctx,
        scoped_generic_types,
        contexts,
    );

    match op {
        ShorthandOp::Add => {
            if !matches_type_pure(&valid_types, &var_ty, scoped_generic_types)
                || !matches_type_pure(&exp_analysis.exp_ty, &var_ty, scoped_generic_types)
            {
                ctx.report_error(
                    &format!(
                        "Cannot {} variable of type {}",
                        error_verb,
                        scoped_generic_types.to_display_string(&var_ty)
                    ),
                    *var_span,
                );
            }
        }
        _ => {
            if !matches_type_pure(&valid_types, &var_ty, scoped_generic_types) {
                ctx.report_error(
                    &format!(
                        "Cannot {} variable of type {}",
                        error_verb,
                        scoped_generic_types.to_display_string(&var_ty)
                    ),
                    *var_span,
                );
            }
        }
    }

    if let DataType::Generic(id) = var_ty {
        scoped_generic_types.constrain_generic_type(id, exp_analysis.exp_ty.clone());
    }

    ctx.insert_reference(
        var,
        var_span.start,
        var_span.end,
        scoped_generic_types,
        contexts,
    );

    StmntAnalysisResult {
        is_propagating_failure: exp_analysis.is_propagating_failure,
        return_ty: exp_analysis.return_ty,
    }
}

// ─── Block Analysis ───────────────────────────────────────────────────────────

pub fn analyze_block_pure(
    (block, span): &Spanned<Block>,
    ctx: &mut PureContext,
    scoped_generic_types: &mut PureGenericsMap,
    contexts: &[Context],
) -> StmntAnalysisResult {
    let mut types: Vec<DataType> = vec![];
    let mut is_propagating = false;

    match block {
        Block::Block(modifiers, stmnt_list) => {
            let mut new_contexts = contexts.to_owned();
            new_contexts.push(Context::Block(BlockContext {
                modifiers: modifiers.iter().map(|(m, _)| m.clone()).collect(),
            }));

            for stmnt in stmnt_list.iter() {
                let result = analyze_stmnt_pure(
                    stmnt,
                    ctx,
                    span.end,
                    scoped_generic_types,
                    &mut new_contexts,
                );
                if let Some(ty) = result.return_ty {
                    types.push(ty);
                }
                is_propagating |= result.is_propagating_failure;
            }
        }
        Block::Singleline(stmnt) => {
            let mut working_contexts = contexts.to_owned();
            let result = analyze_stmnt_pure(
                stmnt,
                ctx,
                span.end,
                scoped_generic_types,
                &mut working_contexts,
            );
            if let Some(ty) = result.return_ty {
                types.push(ty);
            }
            is_propagating |= result.is_propagating_failure;
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

// ─── Failable Handlers ───────────────────────────────────────────────────────

pub fn analyze_failable_handlers_pure(
    failable_handlers: &[Spanned<FailableHandler>],
    ctx: &mut PureContext,
    scoped_generic_types: &mut PureGenericsMap,
    contexts: &[Context],
) -> StmntAnalysisResult {
    let mut types: Vec<DataType> = vec![];
    let mut is_propagating = false;
    let mut contexts = contexts.to_owned();

    let mut has_exited_handler = false;
    let mut has_status_handler = false;

    failable_handlers
        .iter()
        .for_each(|(handler, _)| match handler {
            FailableHandler::Failure((failure, handler_span)) => {
                let mut keyword_span = *handler_span;
                match failure {
                    FailureHandler::Handle((_, name_span), exit_code, block) => {
                        keyword_span = *name_span;

                        if let Some((code_var, code_var_span)) = exit_code {
                            insert_symbol_definition(
                                &mut ctx.symbol_table,
                                &SymbolInfo {
                                    name: code_var.to_string(),
                                    symbol_type: SymbolType::Variable(VariableSymbol {
                                        is_const: false,
                                    }),
                                    data_type: DataType::Int,
                                    is_definition: true,
                                    undefined: false,
                                    span: *code_var_span,
                                    contexts: vec![],
                                },
                                ctx.file_key,
                                code_var_span.end..=handler_span.end,
                                false,
                            );
                        }

                        let result = analyze_stmnt_pure(
                            &(Statement::Block(*block.clone()), block.1),
                            ctx,
                            handler_span.end,
                            scoped_generic_types,
                            &mut contexts,
                        );
                        types.extend(result.return_ty);
                        is_propagating |= result.is_propagating_failure;
                    }
                    FailureHandler::Propagate => {
                        if !contexts
                            .iter()
                            .any(|c| *c == Context::Main || matches!(c, Context::Function(_)))
                        {
                            ctx.report_error(
                                "Propagate can only be used inside of main block or function",
                                *handler_span,
                            );
                        }
                        is_propagating = true;
                    }
                };

                has_status_handler = true;
                if has_exited_handler {
                    ctx.report_error(
                        "Exited handler should be used without any other handlers. Remove this handler or `exited` handler",
                        keyword_span,
                    );
                }
            }
            FailableHandler::Succeeded((_, handler_span), block) => {
                let result = analyze_stmnt_pure(
                    &(Statement::Block(*block.clone()), block.1),
                    ctx,
                    block.1.end,
                    scoped_generic_types,
                    &mut contexts,
                );

                has_status_handler = true;
                if has_exited_handler {
                    ctx.report_error(
                        "Exited handler should be used without any other handlers. Remove this handler or `exited` handler",
                        *handler_span,
                    );
                }

                types.extend(result.return_ty);
                is_propagating |= result.is_propagating_failure;
            }
            FailableHandler::Exited((_, name_span), (code_var, code_var_span), block) => {
                insert_symbol_definition(
                    &mut ctx.symbol_table,
                    &SymbolInfo {
                        name: code_var.to_string(),
                        symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                        data_type: DataType::Int,
                        is_definition: true,
                        undefined: false,
                        span: *code_var_span,
                        contexts: vec![],
                    },
                    ctx.file_key,
                    code_var_span.end..=block.1.end,
                    false,
                );

                let result = analyze_stmnt_pure(
                    &(Statement::Block(*block.clone()), block.1),
                    ctx,
                    block.1.end,
                    scoped_generic_types,
                    &mut contexts,
                );

                has_exited_handler = true;
                if has_status_handler {
                    ctx.report_error(
                        "Exited handler should be used without any other handlers",
                        *name_span,
                    );
                }

                types.extend(result.return_ty);
                is_propagating |= result.is_propagating_failure;
            }
            FailableHandler::Comment(_) => {}
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
