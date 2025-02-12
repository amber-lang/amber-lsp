use chumsky::prelude::*;

use crate::{
    grammar::alpha034::{
        expressions::parse_expr, lexer::Token, parser::default_recovery, AmberParser,
        ElseCondition, IfChainContent, IfCondition, Spanned, Statement,
    },
    T,
};

use super::block::block_parser;

fn else_cond_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<ElseCondition>> {
    let else_condition = just(T!["else"])
        .map_with(|_, e| ("else".to_string(), e.span()))
        .then(block_parser(stmnts.clone()))
        .map_with(|(else_keyword, body), e| (ElseCondition::Else(else_keyword, body), e.span()));

    let else_inline = just(T!["else"])
        .map_with(|_, e| ("else".to_string(), e.span()))
        .then_ignore(just(T![":"]))
        .then(
            stmnts.clone().recover_with(via_parser(
                default_recovery()
                    .or_not()
                    .map_with(|_, e| (Statement::Error, e.span())),
            )),
        )
        .map_with(|(else_keyword, body), e| {
            (
                ElseCondition::InlineElse(else_keyword, Box::new(body)),
                e.span(),
            )
        });

    choice((else_condition, else_inline)).boxed()
}

fn cond_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<IfCondition>> {
    let inline_if = parse_expr(stmnts.clone())
        .then_ignore(just(T![":"]).boxed())
        .then(
            stmnts.clone().recover_with(via_parser(
                default_recovery()
                    .or_not()
                    .map_with(|_, e| (Statement::Error, e.span())),
            )),
        )
        .map_with(|(condition, body), e| {
            (
                IfCondition::InlineIfCondition(Box::new(condition), Box::new(body)),
                e.span(),
            )
        });

    let if_condition = parse_expr(stmnts.clone())
        .then(block_parser(stmnts))
        .map_with(|(cond, body), e| (IfCondition::IfCondition(Box::new(cond), body), e.span()));

    choice((inline_if, if_condition)).boxed()
}

pub fn if_cond_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<Statement>> {
    just(T!["if"])
        .map_with(|_, e| ("if".to_string(), e.span()))
        .then(cond_parser(stmnts.clone()))
        .then(else_cond_parser(stmnts).or_not())
        .map_with(|((if_keyword, if_cond), else_cond), e| {
            (
                Statement::IfCondition(if_keyword, if_cond, else_cond),
                e.span(),
            )
        })
        .boxed()
}

pub fn if_chain_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<Statement>> {
    just(T!["if"])
        .map_with(|_, e| ("if".to_string(), e.span()))
        .then_ignore(just(T!["{"]))
        .then(
            cond_parser(stmnts.clone())
                .recover_with(via_parser(
                    default_recovery().map_with(|_, e| (IfCondition::Error, e.span())),
                ))
                .repeated()
                .collect::<Vec<Spanned<IfCondition>>>(),
        )
        .then(else_cond_parser(stmnts).or_not())
        .then_ignore(
            just(T!["}"]).recover_with(via_parser(default_recovery().or_not().map(|_| T!["}"]))),
        )
        .map_with(|((if_keyword, if_conds), else_cond), e| {
            let mut if_chain: Vec<Spanned<IfChainContent>> = if_conds
                .into_iter()
                .map(|if_cond| (IfChainContent::IfCondition(if_cond.clone()), if_cond.1))
                .collect::<Vec<_>>();

            if let Some(else_cond) = else_cond {
                if_chain.push((IfChainContent::Else(else_cond.clone()), else_cond.1));
            }

            (Statement::IfChain(if_keyword, if_chain), e.span())
        })
        .boxed()
}
