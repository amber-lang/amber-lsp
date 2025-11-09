use chumsky::prelude::*;

use crate::grammar::alpha050::expressions::parse_expr;
use crate::grammar::alpha050::parser::default_recovery;
use crate::grammar::alpha050::statements::comment;
use crate::grammar::alpha050::{
    AmberParser,
    Comment,
    ElseCondition,
    IfChainContent,
    IfCondition,
    Spanned,
    Statement,
    Token,
};
use crate::T;

use super::block::block_parser;

fn else_cond_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<ElseCondition>> {
    let else_condition = just(T!["else"])
        .map_with(|_, e| ("else".to_string(), e.span()))
        .then(block_parser(stmnts.clone(), false, true))
        .map_with(|(else_keyword, body), e| (ElseCondition::Else(else_keyword, body), e.span()))
        .boxed();

    else_condition
}

fn cond_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<IfCondition>> {
    let if_condition = parse_expr(stmnts.clone())
        .then(block_parser(stmnts, false, true))
        .map_with(|(cond, body), e| (IfCondition::IfCondition(cond, body), e.span()));

    if_condition.boxed()
}

pub fn if_cond_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<Statement>> {
    just(T!["if"])
        .map_with(|_, e| ("if".to_string(), e.span()))
        .then(cond_parser(stmnts.clone()))
        .then(
            comment::comment_parser()
                .repeated()
                .collect::<Vec<Spanned<Comment>>>(),
        )
        .then(else_cond_parser(stmnts).or_not())
        .map_with(|(((if_keyword, if_cond), comments), else_cond), e| {
            (
                Statement::IfCondition(if_keyword, if_cond, comments, else_cond),
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
            choice((
                cond_parser(stmnts.clone()),
                comment::comment_parser().map_with(|c, e| (IfCondition::Comment(c), e.span())),
            ))
            .recover_with(via_parser(
                default_recovery().map_with(|_, e| (IfCondition::Error, e.span())),
            ))
            .repeated()
            .collect::<Vec<Spanned<IfCondition>>>(),
        )
        .then(else_cond_parser(stmnts).or_not())
        .then(
            comment::comment_parser()
                .repeated()
                .collect::<Vec<Spanned<Comment>>>(),
        )
        .then_ignore(
            just(T!["}"]).recover_with(via_parser(default_recovery().or_not().map(|_| T!["}"]))),
        )
        .map_with(|(((if_keyword, if_conds), else_cond), comments), e| {
            let mut if_chain: Vec<Spanned<IfChainContent>> = if_conds
                .into_iter()
                .map(|if_cond| (IfChainContent::IfCondition(if_cond.clone()), if_cond.1))
                .collect::<Vec<_>>();

            if let Some(else_cond) = else_cond {
                if_chain.push((IfChainContent::Else(else_cond.clone()), else_cond.1));
            }

            if_chain.extend(
                comments
                    .into_iter()
                    .map(|c| (IfChainContent::Comment(c.clone()), c.1)),
            );

            (Statement::IfChain(if_keyword, if_chain), e.span())
        })
        .boxed()
}
