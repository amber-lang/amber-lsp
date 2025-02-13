use chumsky::span::SimpleSpan;
use tower_lsp::lsp_types::SemanticTokenType;

use crate::grammar::SpannedSemanticToken;

use super::*;

pub const LEGEND_TYPE: [SemanticTokenType; 11] = [
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
];

fn hash_semantic_token_type(token_type: SemanticTokenType) -> usize {
    LEGEND_TYPE.iter().position(|x| *x == token_type).unwrap()
}

pub fn semantic_tokens_from_ast(
    ast: Option<&Vec<Spanned<GlobalStatement>>>,
) -> Vec<SpannedSemanticToken> {
    ast.map_or(vec![], |ast| {
        let mut tokens = vec![];

        for (statement, _) in ast {
            match statement {
                GlobalStatement::Import(is_pub, imp, import_content, from, path) => {
                    if is_pub.0 {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::MODIFIER),
                            is_pub.1.clone(),
                        ));
                    }

                    tokens.push((
                        hash_semantic_token_type(SemanticTokenType::KEYWORD),
                        imp.1.clone(),
                    ));

                    match import_content {
                        (ImportContent::ImportAll, span) => {
                            tokens.push((
                                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                                span.clone(),
                            ));
                        }
                        (ImportContent::ImportSpecific(vars), _) => {
                            vars.iter().for_each(|(_, span)| {
                                tokens.push((
                                    hash_semantic_token_type(SemanticTokenType::VARIABLE),
                                    span.clone(),
                                ));
                            })
                        }
                    }

                    tokens.push((
                        hash_semantic_token_type(SemanticTokenType::KEYWORD),
                        from.1.clone(),
                    ));

                    tokens.push((
                        hash_semantic_token_type(SemanticTokenType::STRING),
                        path.1.clone(),
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
                            span.clone(),
                        ));
                    });

                    if is_pub.0 {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::MODIFIER),
                            is_pub.1.clone(),
                        ));
                    }

                    tokens.push((
                        hash_semantic_token_type(SemanticTokenType::KEYWORD),
                        fun.1.clone(),
                    ));

                    tokens.push((
                        hash_semantic_token_type(SemanticTokenType::FUNCTION),
                        name_span.clone(),
                    ));

                    args.iter().for_each(|(arg, _)| match arg {
                        FunctionArgument::Typed((_, arg_span), (_, ty_span)) => {
                            tokens.push((
                                hash_semantic_token_type(SemanticTokenType::PARAMETER),
                                arg_span.clone(),
                            ));
                            tokens.push((
                                hash_semantic_token_type(SemanticTokenType::TYPE),
                                ty_span.clone(),
                            ));
                        }
                        FunctionArgument::Generic((_, arg_span)) => {
                            tokens.push((
                                hash_semantic_token_type(SemanticTokenType::PARAMETER),
                                arg_span.clone(),
                            ));
                        }
                        FunctionArgument::Error => {}
                    });

                    if let Some((_, ty_span)) = ty {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::TYPE),
                            ty_span.clone(),
                        ));
                    }

                    tokens.extend(semantic_tokens_from_stmnts(body));
                }
                GlobalStatement::Main((_, main_span), stmnts) => {
                    tokens.push((
                        hash_semantic_token_type(SemanticTokenType::KEYWORD),
                        *main_span,
                    ));
                    tokens.extend(semantic_tokens_from_stmnts(stmnts));
                }
                GlobalStatement::Statement(stmnt) => {
                    tokens.extend(semantic_tokens_from_stmnts(&vec![stmnt.clone()]));
                }
            }
        }

        tokens
    })
}

fn semantic_tokens_from_stmnts(stmnts: &Vec<Spanned<Statement>>) -> Vec<SpannedSemanticToken> {
    stmnts
        .iter()
        .flat_map(|(stmnt, span)| match stmnt {
            Statement::Block((block, _)) => match block {
                Block::Block(modifiers, stmnts) => {
                    let mut tokens = vec![];

                    modifiers.iter().for_each(|(_, span)| {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::KEYWORD),
                            span.clone(),
                        ));
                    });

                    tokens.extend(semantic_tokens_from_stmnts(stmnts));

                    tokens
                }
                Block::Error => vec![],
            },
            Statement::Break => vec![(
                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                span.clone(),
            )],
            Statement::Comment(_) => vec![(
                hash_semantic_token_type(SemanticTokenType::COMMENT),
                span.clone(),
            )],
            Statement::Continue => vec![(
                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                span.clone(),
            )],
            Statement::Echo((_, echo_span), expr) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                    *echo_span,
                )];

                tokens.extend(semantic_tokens_from_expr(expr));

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
                                    block.1.clone(),
                                )]));
                            }
                            IfCondition::InlineIfCondition(expr, stmnt) => {
                                tokens.extend(semantic_tokens_from_expr(expr));
                                tokens.extend(semantic_tokens_from_stmnts(&vec![*stmnt.clone()]));
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
                                    block.1.clone(),
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
                    });

                tokens
            }
            Statement::IfCondition((_, if_span), (if_cond, _), else_cond) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                    *if_span,
                )];

                match if_cond {
                    IfCondition::IfCondition(expr, block) => {
                        tokens.extend(semantic_tokens_from_expr(expr));
                        tokens.extend(semantic_tokens_from_stmnts(&vec![(
                            Statement::Block(block.clone()),
                            block.1.clone(),
                        )]));
                    }
                    IfCondition::InlineIfCondition(expr, stmnt) => {
                        tokens.extend(semantic_tokens_from_expr(expr));
                        tokens.extend(semantic_tokens_from_stmnts(&vec![*stmnt.clone()]));
                    }
                    IfCondition::Error => {}
                }

                if let Some((else_cond, _)) = else_cond {
                    match else_cond {
                        ElseCondition::Else((_, else_span), block) => {
                            tokens.push((
                                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                                *else_span,
                            ));

                            tokens.extend(semantic_tokens_from_stmnts(&vec![(
                                Statement::Block(block.clone()),
                                block.1.clone(),
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
                    block.1.clone(),
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
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::VARIABLE),
                            span.clone(),
                        ));
                    }
                    IterLoopVars::WithIndex((_, span1), (_, span2)) => {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::VARIABLE),
                            span1.clone(),
                        ));
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::VARIABLE),
                            span2.clone(),
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
                    block.1.clone(),
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
                    var_span.clone(),
                )];

                tokens.extend(semantic_tokens_from_expr(expr));

                tokens
            }
            Statement::ShorthandDiv((_, var_span), expr) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::VARIABLE),
                    var_span.clone(),
                )];

                tokens.extend(semantic_tokens_from_expr(expr));

                tokens
            }
            Statement::ShorthandMul((_, var_span), expr) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::VARIABLE),
                    var_span.clone(),
                )];

                tokens.extend(semantic_tokens_from_expr(expr));

                tokens
            }
            Statement::ShorthandModulo((_, var_span), expr) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::VARIABLE),
                    var_span.clone(),
                )];

                tokens.extend(semantic_tokens_from_expr(expr));

                tokens
            }
            Statement::ShorthandSub((_, var_span), expr) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::VARIABLE),
                    var_span.clone(),
                )];

                tokens.extend(semantic_tokens_from_expr(expr));

                tokens
            }
            Statement::VariableInit((_, let_span), (_, var_span), (val, _)) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                    let_span.clone(),
                )];

                tokens.push((
                    hash_semantic_token_type(SemanticTokenType::VARIABLE),
                    var_span.clone(),
                ));

                match val {
                    VariableInitType::Expression(expr) => {
                        tokens.extend(semantic_tokens_from_expr(expr));
                    }
                    VariableInitType::DataType((_, ty_span)) => {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::TYPE),
                            ty_span.clone(),
                        ));
                    }
                    &VariableInitType::Error => {}
                }

                tokens
            }
            Statement::VariableSet((_, var_span), expr) => {
                let mut tokens = vec![(
                    hash_semantic_token_type(SemanticTokenType::VARIABLE),
                    var_span.clone(),
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
                and_span.clone(),
            ));
            tokens.extend(semantic_tokens_from_expr(rhs));

            tokens
        }
        Expression::Array(elements) => elements
            .iter()
            .flat_map(|expr| semantic_tokens_from_expr(expr))
            .collect(),
        Expression::Boolean(_) => vec![(
            hash_semantic_token_type(SemanticTokenType::KEYWORD),
            span.clone(),
        )],
        Expression::Cast(expr, (_, as_span), (_, ty_span)) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(expr));
            tokens.push((
                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                as_span.clone(),
            ));
            tokens.push((
                hash_semantic_token_type(SemanticTokenType::TYPE),
                ty_span.clone(),
            ));

            tokens
        }
        Expression::Command(modifiers, cmd, failure_handler) => {
            let mut tokens = vec![];

            modifiers.iter().for_each(|(_, span)| {
                tokens.push((
                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                    span.clone(),
                ));
            });

            cmd.iter().for_each(|(inter_cmd, span)| match inter_cmd {
                InterpolatedCommand::Text(_) => {
                    tokens.push((
                        hash_semantic_token_type(SemanticTokenType::STRING),
                        span.clone(),
                    ));
                }
                InterpolatedCommand::Expression(expr) => {
                    tokens.extend(semantic_tokens_from_expr(expr));
                }
                InterpolatedCommand::CommandOption(_) => {
                    tokens.push((
                        hash_semantic_token_type(SemanticTokenType::KEYWORD),
                        span.clone(),
                    ));
                }
                InterpolatedCommand::Escape(_) => {
                    tokens.push((
                        hash_semantic_token_type(SemanticTokenType::KEYWORD),
                        span.clone(),
                    ));
                }
            });

            if let Some((failure_handler, failure_span)) = failure_handler {
                match failure_handler {
                    FailureHandler::Handle((_, failed_span), stmnts) => {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::KEYWORD),
                            failed_span.clone(),
                        ));

                        tokens.extend(semantic_tokens_from_stmnts(stmnts));
                    }
                    FailureHandler::Propagate => {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::KEYWORD),
                            failure_span.clone(),
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
                tokens.push((
                    hash_semantic_token_type(SemanticTokenType::KEYWORD),
                    span.clone(),
                ));
            });

            tokens.push((
                hash_semantic_token_type(SemanticTokenType::FUNCTION),
                name_span.clone(),
            ));

            args.iter().for_each(|expr| {
                tokens.extend(semantic_tokens_from_expr(expr));
            });

            if let Some((failure_handler, failure_span)) = failure_handler {
                match failure_handler {
                    FailureHandler::Handle((_, failed_span), stmnts) => {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::KEYWORD),
                            failed_span.clone(),
                        ));

                        tokens.extend(semantic_tokens_from_stmnts(stmnts));
                    }
                    FailureHandler::Propagate => {
                        tokens.push((
                            hash_semantic_token_type(SemanticTokenType::KEYWORD),
                            failure_span.clone(),
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
                is_span.clone(),
            ));
            tokens.push((
                hash_semantic_token_type(SemanticTokenType::TYPE),
                ty_span.clone(),
            ));

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
                nameof_span.clone(),
            )];

            tokens.extend(semantic_tokens_from_expr(expr));

            tokens
        }
        Expression::Neg((_, op_span), expr) => {
            let mut tokens = vec![(
                hash_semantic_token_type(SemanticTokenType::OPERATOR),
                op_span.clone(),
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
                not_span.clone(),
            )];

            tokens.extend(semantic_tokens_from_expr(expr));

            tokens
        }
        Expression::Null => vec![(
            hash_semantic_token_type(SemanticTokenType::KEYWORD),
            span.clone(),
        )],
        Expression::Number(_) => vec![(
            hash_semantic_token_type(SemanticTokenType::NUMBER),
            span.clone(),
        )],
        Expression::Or(lhs, (_, or_span), rhs) => {
            let mut tokens = vec![];

            tokens.extend(semantic_tokens_from_expr(lhs));

            tokens.push((
                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                or_span.clone(),
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
        Expression::Status => vec![(
            hash_semantic_token_type(SemanticTokenType::KEYWORD),
            span.clone(),
        )],
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
                then_span.clone(),
            ));
            tokens.extend(semantic_tokens_from_expr(if_true));
            tokens.push((
                hash_semantic_token_type(SemanticTokenType::KEYWORD),
                else_span.clone(),
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
                        &InterpolatedText::Text(_) => vec![(
                            hash_semantic_token_type(SemanticTokenType::STRING),
                            span.clone(),
                        )],
                        InterpolatedText::Expression(expr) => semantic_tokens_from_expr(expr),
                        InterpolatedText::Escape(_) => vec![(
                            hash_semantic_token_type(SemanticTokenType::STRING),
                            span.clone(),
                        )],
                    })
                    .collect::<Vec<Spanned<usize>>>(),
            );

            tokens.push((
                hash_semantic_token_type(SemanticTokenType::STRING),
                SimpleSpan::new(span.end - 1, span.end),
            ));

            tokens
        }
        Expression::Var(_) => vec![(
            hash_semantic_token_type(SemanticTokenType::VARIABLE),
            span.clone(),
        )],
        Expression::Error => vec![],
    }
}
