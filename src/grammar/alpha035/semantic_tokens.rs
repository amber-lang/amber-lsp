use chumsky::span::SimpleSpan;
use tower_lsp_server::lsp_types::SemanticTokenType;

use crate::grammar::SpannedSemanticToken;

use super::*;

const ESCAPE_SEQUENCE: SemanticTokenType = SemanticTokenType::new("escapeSequence");
const CONSTANT: SemanticTokenType = SemanticTokenType::new("constant");

pub const LEGEND_TYPE: [SemanticTokenType; 13] = [
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::COMMENT,
    SemanticTokenType::NUMBER,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::TYPE,
    SemanticTokenType::MODIFIER,
    SemanticTokenType::DECORATOR,
    ESCAPE_SEQUENCE,
    CONSTANT,
];

fn hash_semantic_token_type(token_type: SemanticTokenType) -> usize {
    LEGEND_TYPE.iter().position(|x| *x == token_type).unwrap()
}

#[tracing::instrument(skip_all)]
pub fn semantic_tokens_from_ast(
    ast: Option<&Vec<Spanned<GlobalStatement>>>,
) -> Vec<SpannedSemanticToken> {
    ast.map_or(vec![], |ast| {
        let mut tokens = vec![];

        for (statement, _) in ast {
            match statement {
                GlobalStatement::Import(
                    (is_pub, is_pub_span),
                    (_, import_keyword_span),
                    import_content,
                    (_, from_keyword_span),
                    (_, path_span),
                ) => {
                    if *is_pub {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::MODIFIER),
                            *is_pub_span,
                        ));
                    }

                    tokens.push((
                        hash_semantic_token_type(SemanticTokenType::KEYWORD),
                        *import_keyword_span,
                    ));

                    match import_content {
                        (ImportContent::ImportAll, span) => {
                            tokens.push((
                                hash_semantic_token_type(SemanticTokenType::VARIABLE),
                                *span,
                            ));
                        }
                        (ImportContent::ImportSpecific(vars), _) => {
                            vars.iter().for_each(|(_, span)| {
                                tokens.push((
                                    hash_semantic_token_type(SemanticTokenType::VARIABLE),
                                    *span,
                                ));
                            })
                        }
                    }

                    tokens.push((
                        hash_semantic_token_type(SemanticTokenType::KEYWORD),
                        *from_keyword_span,
                    ));

                    tokens.push((
                        hash_semantic_token_type(SemanticTokenType::STRING),
                        *path_span,
                    ));
                }
                GlobalStatement::FunctionDefinition(
                    compiler_flags,
                    is_pub,
                    fun,
                    (_, name_span),
                    args,
                    ty,
                    body,
                ) => {
                    compiler_flags.iter().for_each(|(_, span)| {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::DECORATOR),
                            *span,
                        ));
                    });

                    if is_pub.0 {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::MODIFIER),
                            is_pub.1,
                        ));
                    }

                    tokens.push((hash_semantic_token_type(SemanticTokenType::KEYWORD), fun.1));

                    tokens.push((
                        hash_semantic_token_type(SemanticTokenType::FUNCTION),
                        *name_span,
                    ));

                    args.iter().for_each(|(arg, _)| match arg {
                        FunctionArgument::Typed(
                            (is_ref, is_ref_span),
                            (_, arg_span),
                            (_, ty_span),
                        ) => {
                            if *is_ref {
                                tokens.push((
                                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                                    *is_ref_span,
                                ));
                            }

                            tokens.push((
                                hash_semantic_token_type(SemanticTokenType::PARAMETER),
                                *arg_span,
                            ));
                            tokens.push((
                                hash_semantic_token_type(SemanticTokenType::TYPE),
                                *ty_span,
                            ));
                        }
                        FunctionArgument::Generic((is_ref, is_ref_span), (_, arg_span)) => {
                            if *is_ref {
                                tokens.push((
                                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                                    *is_ref_span,
                                ));
                            }

                            tokens.push((
                                hash_semantic_token_type(SemanticTokenType::PARAMETER),
                                *arg_span,
                            ));
                        }
                        FunctionArgument::Optional(
                            (is_ref, is_ref_span),
                            (_, arg_span),
                            ty,
                            exp,
                        ) => {
                            if *is_ref {
                                tokens.push((
                                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                                    *is_ref_span,
                                ));
                            }

                            tokens.push((
                                hash_semantic_token_type(SemanticTokenType::PARAMETER),
                                *arg_span,
                            ));

                            if let Some((_, ty_span)) = ty {
                                tokens.push((
                                    hash_semantic_token_type(SemanticTokenType::TYPE),
                                    *ty_span,
                                ));
                            }

                            tokens.extend(semantic_tokens_from_expr(exp));
                        }
                        FunctionArgument::Error => {}
                    });

                    if let Some((_, ty_span)) = ty {
                        tokens.push((hash_semantic_token_type(SemanticTokenType::TYPE), *ty_span));
                    }

                    tokens.extend(semantic_tokens_from_stmnts(body));
                }
                GlobalStatement::Main((_, main_span), args, stmnts) => {
                    tokens.push((
                        hash_semantic_token_type(SemanticTokenType::KEYWORD),
                        *main_span,
                    ));

                    if let Some((_, args_span)) = args {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::VARIABLE),
                            *args_span,
                        ));
                    }

                    tokens.extend(semantic_tokens_from_stmnts(stmnts));
                }
                GlobalStatement::Statement(stmnt) => {
                    tokens.extend(semantic_tokens_from_stmnts(&vec![*stmnt.clone()]));
                }
            }
        }

        tokens
    })
}

fn semantic_tokens_from_stmnts(stmnts: &[Spanned<Statement>]) -> Vec<SpannedSemanticToken> {
    stmnts
        .iter()
        .flat_map(|(stmnt, span)| match stmnt {
            Statement::Block((block, _)) => match block {
                Block::Block(modifiers, stmnts) => {
                    let mut tokens = vec![];

                    modifiers.iter().for_each(|(_, span)| {
                        tokens.push((hash_semantic_token_type(SemanticTokenType::KEYWORD), *span));
                    });

                    tokens.extend(semantic_tokens_from_stmnts(stmnts));

                    tokens
                }
                Block::Error => vec![],
            },
            Statement::Break => vec![(hash_semantic_token_type(SemanticTokenType::KEYWORD), *span)],
            Statement::Comment(_) => {
                vec![(hash_semantic_token_type(SemanticTokenType::COMMENT), *span)]
            }
            Statement::Shebang(_) => {
                vec![(hash_semantic_token_type(SemanticTokenType::COMMENT), *span)]
            }
            Statement::Continue => {
                vec![(hash_semantic_token_type(SemanticTokenType::KEYWORD), *span)]
            }
            Statement::Echo((_, echo_span), expr) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                    *echo_span,
                )];

                tokens.extend(semantic_tokens_from_expr(expr));

                tokens
            }
            Statement::Cd((_, cd_span), expr) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                    *cd_span,
                )];

                tokens.extend(semantic_tokens_from_expr(expr));

                tokens
            }
            Statement::MoveFiles(command_modifiers, (_, mv_span), src, dest, failure) => {
                let mut tokens = vec![];

                command_modifiers.iter().for_each(|(_, span)| {
                    tokens.push((hash_semantic_token_type(SemanticTokenType::MODIFIER), *span));
                });

                tokens.push((
                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                    *mv_span,
                ));

                tokens.extend(semantic_tokens_from_expr(src));
                tokens.extend(semantic_tokens_from_expr(dest));

                if let Some((failure, failure_span)) = failure {
                    match failure {
                        FailureHandler::Handle((_, failed_span), stmnts) => {
                            tokens.push((
                                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                                *failed_span,
                            ));

                            tokens.extend(semantic_tokens_from_stmnts(stmnts));
                        }
                        FailureHandler::Propagate => {
                            tokens.push((
                                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                                *failure_span,
                            ));
                        }
                    }
                }

                tokens
            }
            Statement::Expression(expr) => semantic_tokens_from_expr(expr),
            Statement::Fail((_, fail_span), expr) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                    *fail_span,
                )];

                if let Some(expr) = expr {
                    tokens.extend(semantic_tokens_from_expr(expr));
                }

                tokens
            }
            Statement::IfChain((_, if_span), if_chain) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                    *if_span,
                )];

                if_chain
                    .iter()
                    .for_each(|(chain_cond, _)| match chain_cond {
                        IfChainContent::IfCondition((if_cond, _)) => match if_cond {
                            IfCondition::IfCondition(expr, block) => {
                                tokens.extend(semantic_tokens_from_expr(expr));
                                tokens.extend(semantic_tokens_from_stmnts(&vec![(
                                    Statement::Block(block.clone()),
                                    block.1,
                                )]));
                            }
                            IfCondition::InlineIfCondition(expr, stmnt) => {
                                tokens.extend(semantic_tokens_from_expr(expr));
                                tokens.extend(semantic_tokens_from_stmnts(&vec![*stmnt.clone()]));
                            }
                            IfCondition::Comment((_, span)) => {
                                tokens.push((
                                    hash_semantic_token_type(SemanticTokenType::COMMENT),
                                    *span,
                                ));
                            }
                            IfCondition::Error => {}
                        },
                        IfChainContent::Else((else_cond, _)) => match else_cond {
                            ElseCondition::Else((_, else_span), block) => {
                                tokens.push((
                                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                                    *else_span,
                                ));

                                tokens.extend(semantic_tokens_from_stmnts(&vec![(
                                    Statement::Block(block.clone()),
                                    block.1,
                                )]));
                            }
                            ElseCondition::InlineElse((_, else_span), stmnt) => {
                                tokens.push((
                                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                                    *else_span,
                                ));

                                tokens.extend(semantic_tokens_from_stmnts(&vec![*stmnt.clone()]));
                            }
                        },
                        IfChainContent::Comment((_, span)) => {
                            tokens.push((
                                hash_semantic_token_type(SemanticTokenType::COMMENT),
                                *span,
                            ));
                        }
                    });

                tokens
            }
            Statement::IfCondition((_, if_span), (if_cond, _), comments, else_cond) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                    *if_span,
                )];

                match if_cond {
                    IfCondition::IfCondition(expr, block) => {
                        tokens.extend(semantic_tokens_from_expr(expr));
                        tokens.extend(semantic_tokens_from_stmnts(&vec![(
                            Statement::Block(block.clone()),
                            block.1,
                        )]));
                    }
                    IfCondition::InlineIfCondition(expr, stmnt) => {
                        tokens.extend(semantic_tokens_from_expr(expr));
                        tokens.extend(semantic_tokens_from_stmnts(&vec![*stmnt.clone()]));
                    }
                    IfCondition::Comment((_, span)) => {
                        tokens.push((hash_semantic_token_type(SemanticTokenType::COMMENT), *span));
                    }
                    IfCondition::Error => {}
                }

                comments.iter().for_each(|(_, span)| {
                    tokens.push((hash_semantic_token_type(SemanticTokenType::COMMENT), *span));
                });

                if let Some((else_cond, _)) = else_cond {
                    match else_cond {
                        ElseCondition::Else((_, else_span), block) => {
                            tokens.push((
                                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                                *else_span,
                            ));

                            tokens.extend(semantic_tokens_from_stmnts(&vec![(
                                Statement::Block(block.clone()),
                                block.1,
                            )]));
                        }
                        ElseCondition::InlineElse((_, else_span), stmnt) => {
                            tokens.push((
                                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                                *else_span,
                            ));

                            tokens.extend(semantic_tokens_from_stmnts(&vec![*stmnt.clone()]));
                        }
                    }
                }

                tokens
            }
            Statement::InfiniteLoop((_, loop_span), block) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                    *loop_span,
                )];

                tokens.extend(semantic_tokens_from_stmnts(&vec![(
                    Statement::Block(block.clone()),
                    block.1,
                )]));

                tokens
            }
            Statement::IterLoop((_, if_span), (vars, _), (_, in_span), expr, block) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                    *if_span,
                )];

                match vars {
                    IterLoopVars::Single((_, span)) => {
                        tokens.push((hash_semantic_token_type(SemanticTokenType::VARIABLE), *span));
                    }
                    IterLoopVars::WithIndex((_, span1), (_, span2)) => {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::VARIABLE),
                            *span1,
                        ));
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::VARIABLE),
                            *span2,
                        ));
                    }
                    IterLoopVars::Error => {}
                }

                tokens.push((
                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                    *in_span,
                ));

                tokens.extend(semantic_tokens_from_expr(expr));
                tokens.extend(semantic_tokens_from_stmnts(&vec![(
                    Statement::Block(block.clone()),
                    block.1,
                )]));

                tokens
            }
            Statement::Return((_, return_span), expr) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                    *return_span,
                )];

                if let Some(expr) = expr {
                    tokens.extend(semantic_tokens_from_expr(expr));
                }

                tokens
            }
            Statement::ShorthandAdd((_, var_span), expr) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::VARIABLE),
                    *var_span,
                )];

                tokens.extend(semantic_tokens_from_expr(expr));

                tokens
            }
            Statement::ShorthandDiv((_, var_span), expr) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::VARIABLE),
                    *var_span,
                )];

                tokens.extend(semantic_tokens_from_expr(expr));

                tokens
            }
            Statement::ShorthandMul((_, var_span), expr) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::VARIABLE),
                    *var_span,
                )];

                tokens.extend(semantic_tokens_from_expr(expr));

                tokens
            }
            Statement::ShorthandModulo((_, var_span), expr) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::VARIABLE),
                    *var_span,
                )];

                tokens.extend(semantic_tokens_from_expr(expr));

                tokens
            }
            Statement::ShorthandSub((_, var_span), expr) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::VARIABLE),
                    *var_span,
                )];

                tokens.extend(semantic_tokens_from_expr(expr));

                tokens
            }
            Statement::ConstInit((_, const_span), (_, var_span), exp) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                    *const_span,
                )];

                tokens.push((
                    hash_semantic_token_type(SemanticTokenType::VARIABLE),
                    *var_span,
                ));

                tokens.extend(semantic_tokens_from_expr(exp));

                tokens
            }
            Statement::VariableInit((_, let_span), (_, var_span), (val, _)) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                    *let_span,
                )];

                tokens.push((
                    hash_semantic_token_type(SemanticTokenType::VARIABLE),
                    *var_span,
                ));

                match val {
                    VariableInitType::Expression(expr) => {
                        tokens.extend(semantic_tokens_from_expr(expr));
                    }
                    VariableInitType::DataType((_, ty_span)) => {
                        tokens.push((hash_semantic_token_type(SemanticTokenType::TYPE), *ty_span));
                    }
                    &VariableInitType::Error => {}
                }

                tokens
            }
            Statement::VariableSet((_, var_span), expr) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::VARIABLE),
                    *var_span,
                )];

                tokens.extend(semantic_tokens_from_expr(expr));

                tokens
            }
            Statement::Error => vec![],
        })
        .collect()
}

fn semantic_tokens_from_expr((expr, span): &Spanned<Expression>) -> Vec<SpannedSemanticToken> {
    match expr {
        Expression::Add(lhs, rhs) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(lhs));
            tokens.extend(semantic_tokens_from_expr(rhs));

            tokens
        }
        Expression::And(lhs, (_, and_span), rhs) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(lhs));
            tokens.push((
                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                *and_span,
            ));
            tokens.extend(semantic_tokens_from_expr(rhs));

            tokens
        }
        Expression::Array(elements) => elements
            .iter()
            .flat_map(semantic_tokens_from_expr)
            .collect(),
        Expression::Boolean(_) => vec![(hash_semantic_token_type(CONSTANT), *span)],
        Expression::Cast(expr, (_, as_span), (_, ty_span)) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(expr));
            tokens.push((
                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                *as_span,
            ));
            tokens.push((hash_semantic_token_type(SemanticTokenType::TYPE), *ty_span));

            tokens
        }
        Expression::Command(modifiers, cmd, failure_handler) => {
            let mut tokens = vec![];

            modifiers.iter().for_each(|(_, span)| {
                tokens.push((hash_semantic_token_type(SemanticTokenType::KEYWORD), *span));
            });

            cmd.iter().for_each(|(inter_cmd, span)| match inter_cmd {
                InterpolatedCommand::Text(_) => {
                    tokens.push((hash_semantic_token_type(SemanticTokenType::STRING), *span));
                }
                InterpolatedCommand::Expression(expr) => {
                    tokens.extend(semantic_tokens_from_expr(expr));
                }
                InterpolatedCommand::CommandOption(_) => {
                    tokens.push((hash_semantic_token_type(CONSTANT), *span));
                }
                InterpolatedCommand::Escape(_) => {
                    tokens.push((hash_semantic_token_type(ESCAPE_SEQUENCE), *span));
                }
            });

            if let Some((failure_handler, failure_span)) = failure_handler {
                match failure_handler {
                    FailureHandler::Handle((_, failed_span), stmnts) => {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::KEYWORD),
                            *failed_span,
                        ));

                        tokens.extend(semantic_tokens_from_stmnts(stmnts));
                    }
                    FailureHandler::Propagate => {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::KEYWORD),
                            *failure_span,
                        ));
                    }
                }
            }

            tokens
        }
        Expression::Divide(lhs, rhs) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(lhs));
            tokens.extend(semantic_tokens_from_expr(rhs));

            tokens
        }
        Expression::Eq(lhs, rhs) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(lhs));
            tokens.extend(semantic_tokens_from_expr(rhs));

            tokens
        }
        Expression::FunctionInvocation(modifiers, (_, name_span), args, failure_handler) => {
            let mut tokens = vec![];

            modifiers.iter().for_each(|(_, span)| {
                tokens.push((hash_semantic_token_type(SemanticTokenType::KEYWORD), *span));
            });

            tokens.push((
                hash_semantic_token_type(SemanticTokenType::FUNCTION),
                *name_span,
            ));

            args.iter().for_each(|expr| {
                tokens.extend(semantic_tokens_from_expr(expr));
            });

            if let Some((failure_handler, failure_span)) = failure_handler {
                match failure_handler {
                    FailureHandler::Handle((_, failed_span), stmnts) => {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::KEYWORD),
                            *failed_span,
                        ));

                        tokens.extend(semantic_tokens_from_stmnts(stmnts));
                    }
                    FailureHandler::Propagate => {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::KEYWORD),
                            *failure_span,
                        ));
                    }
                }
            }

            tokens
        }
        Expression::Ge(lhs, rhs) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(lhs));
            tokens.extend(semantic_tokens_from_expr(rhs));

            tokens
        }
        Expression::Gt(lhs, rhs) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(lhs));
            tokens.extend(semantic_tokens_from_expr(rhs));

            tokens
        }
        Expression::Is(lhs, (_, is_span), (_, ty_span)) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(lhs));
            tokens.push((
                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                *is_span,
            ));
            tokens.push((hash_semantic_token_type(SemanticTokenType::TYPE), *ty_span));

            tokens
        }
        Expression::Le(lhs, rhs) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(lhs));
            tokens.extend(semantic_tokens_from_expr(rhs));

            tokens
        }
        Expression::Lt(lhs, rhs) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(lhs));
            tokens.extend(semantic_tokens_from_expr(rhs));

            tokens
        }
        Expression::Modulo(lhs, rhs) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(lhs));
            tokens.extend(semantic_tokens_from_expr(rhs));

            tokens
        }
        Expression::Multiply(lhs, rhs) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(lhs));
            tokens.extend(semantic_tokens_from_expr(rhs));

            tokens
        }
        Expression::Nameof((_, nameof_span), expr) => {
            let mut tokens = vec![(
                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                *nameof_span,
            )];

            tokens.extend(semantic_tokens_from_expr(expr));

            tokens
        }
        Expression::Neg((_, op_span), expr) => {
            let mut tokens = vec![(
                hash_semantic_token_type(SemanticTokenType::OPERATOR),
                *op_span,
            )];

            tokens.extend(semantic_tokens_from_expr(expr));

            tokens
        }
        Expression::Neq(lhs, rhs) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(lhs));
            tokens.extend(semantic_tokens_from_expr(rhs));

            tokens
        }
        Expression::Not((_, not_span), expr) => {
            let mut tokens = vec![(
                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                *not_span,
            )];

            tokens.extend(semantic_tokens_from_expr(expr));

            tokens
        }
        Expression::Null => vec![(hash_semantic_token_type(CONSTANT), *span)],
        Expression::Number(_) => vec![(hash_semantic_token_type(SemanticTokenType::NUMBER), *span)],
        Expression::Or(lhs, (_, or_span), rhs) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(lhs));

            tokens.push((
                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                *or_span,
            ));

            tokens.extend(semantic_tokens_from_expr(rhs));

            tokens
        }
        Expression::Parentheses(expr) => semantic_tokens_from_expr(expr),
        Expression::Range(lhs, rhs) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(lhs));
            tokens.extend(semantic_tokens_from_expr(rhs));

            tokens
        }
        Expression::Status => vec![(hash_semantic_token_type(SemanticTokenType::KEYWORD), *span)],
        Expression::Subtract(lhs, rhs) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(lhs));
            tokens.extend(semantic_tokens_from_expr(rhs));

            tokens
        }
        Expression::Ternary(cond, (_, then_span), if_true, (_, else_span), if_false) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(cond));
            tokens.push((
                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                *then_span,
            ));
            tokens.extend(semantic_tokens_from_expr(if_true));
            tokens.push((
                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                *else_span,
            ));
            tokens.extend(semantic_tokens_from_expr(if_false));

            tokens
        }
        Expression::Text(inter_text) => {
            let mut tokens = vec![];

            tokens.push((
                hash_semantic_token_type(SemanticTokenType::STRING),
                SimpleSpan::new(span.start, span.start + 1),
            ));
            tokens.extend(
                inter_text
                    .iter()
                    .flat_map(|(text, span)| match text {
                        &InterpolatedText::Text(_) => {
                            vec![(hash_semantic_token_type(SemanticTokenType::STRING), *span)]
                        }
                        InterpolatedText::Expression(expr) => semantic_tokens_from_expr(expr),
                        InterpolatedText::Escape(_) => {
                            vec![(hash_semantic_token_type(SemanticTokenType::STRING), *span)]
                        }
                    })
                    .collect::<Vec<Spanned<usize>>>(),
            );

            tokens.push((
                hash_semantic_token_type(SemanticTokenType::STRING),
                SimpleSpan::new(span.end - 1, span.end),
            ));

            tokens
        }
        Expression::Var(_) => vec![(hash_semantic_token_type(SemanticTokenType::VARIABLE), *span)],
        Expression::Error => vec![],
    }
}
