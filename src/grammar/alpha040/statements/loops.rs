use chumsky::prelude::*;

use crate::grammar::alpha040::expressions::parse_expr;
use crate::grammar::alpha040::parser::{
    default_recovery,
    ident,
};
use crate::grammar::alpha040::{
    AmberParser,
    Block,
    Expression,
    IterLoopVars,
    Spanned,
    Statement,
};
use crate::grammar::Token;
use crate::T;

use super::block::block_parser;

pub fn inf_loop_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<Statement>> {
    just(T!["loop"])
        .map_with(|_, e| ("loop".to_string(), e.span()))
        .then(block_parser(stmnts, false))
        .map_with(|(loop_keyword, block), e| {
            (Statement::InfiniteLoop(loop_keyword, block), e.span())
        })
        .boxed()
}

pub fn iter_loop_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<Statement>> {
    let single_var = ident("variable".to_string())
        .map_with(|txt, e| (IterLoopVars::Single((txt, e.span())), e.span()));
    let with_index_var = ident("variable".to_string())
        .map_with(|txt, e| (txt, e.span()))
        .then_ignore(just(T![","]))
        .then(
            ident("variable".to_string())
                .recover_with(via_parser(
                    default_recovery().or_not().map(|_| "".to_string()),
                ))
                .map_with(|txt, e| (txt, e.span())),
        )
        .map_with(|(var, index), e| (IterLoopVars::WithIndex(var, index), e.span()));

    just(T!["for"])
        .map_with(|_, e| ("for".to_string(), e.span()))
        .then(
            choice((with_index_var, single_var)).recover_with(via_parser(
                default_recovery().map_with(|_, e| (IterLoopVars::Error, e.span())),
            )),
        )
        .then(
            just(T!["in"])
                .recover_with(via_parser(any().or_not().map(|_| T!["in"])))
                .map_with(|t, e| (t.to_string(), e.span())),
        )
        .then(
            parse_expr(stmnts.clone()).recover_with(via_parser(
                default_recovery()
                    .or_not()
                    .map_with(|_, e| (Expression::Error, e.span())),
            )),
        )
        .then(
            block_parser(stmnts, false).recover_with(via_parser(
                default_recovery()
                    .or_not()
                    .map_with(|_, e| (Block::Error, e.span())),
            )),
        )
        .map_with(|((((loop_keyword, vars), in_keyword), expr), body), e| {
            (
                Statement::IterLoop(loop_keyword, vars, in_keyword, Box::new(expr), body),
                e.span(),
            )
        })
        .boxed()
}
