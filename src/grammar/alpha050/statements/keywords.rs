use chumsky::prelude::*;

use crate::grammar::alpha050::expressions::parse_expr;
use crate::grammar::alpha050::parser::default_recovery;
use crate::grammar::alpha050::{
    AmberParser,
    Expression,
    Spanned,
    Statement,
    Token,
};
use crate::T;

pub fn keywords_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<Statement>> {
    choice((
        just(T!["break"]).map_with(|_, e| (Statement::Break, e.span())),
        just(T!["continue"]).map_with(|_, e| (Statement::Continue, e.span())),
        just(T!["return"])
            .map_with(|_, e| ("return".to_string(), e.span()))
            .then(parse_expr(stmnts.clone()).or_not())
            .map_with(|(return_keyword, expr), e| {
                (
                    Statement::Return(return_keyword, expr.map(Box::new)),
                    e.span(),
                )
            }),
        just(T!["fail"])
            .map_with(|_, e| ("fail".to_string(), e.span()))
            .then(parse_expr(stmnts.clone()).or_not())
            .map_with(|(fail_keyword, expr), e| {
                (Statement::Fail(fail_keyword, expr.map(Box::new)), e.span())
            }),
        just(T!["echo"])
            .map_with(|_, e| ("echo".to_string(), e.span()))
            .then(
                parse_expr(stmnts.clone()).recover_with(via_parser(
                    default_recovery()
                        .or_not()
                        .map_with(|_, e| (Expression::Error, e.span())),
                )),
            )
            .map_with(|(echo_keyword, expr), e| {
                (Statement::Echo(echo_keyword, Box::new(expr)), e.span())
            }),
        just(T!["cd"])
            .map_with(|_, e| ("cd".to_string(), e.span()))
            .then(
                parse_expr(stmnts).recover_with(via_parser(
                    default_recovery()
                        .or_not()
                        .map_with(|_, e| (Expression::Error, e.span())),
                )),
            )
            .map_with(|(echo_keyword, expr), e| {
                (Statement::Cd(echo_keyword, Box::new(expr)), e.span())
            }),
    ))
    .boxed()
}
