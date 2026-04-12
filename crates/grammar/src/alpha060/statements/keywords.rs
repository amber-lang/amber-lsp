use chumsky::prelude::*;

use crate::alpha060::expressions::parse_expr;
use crate::alpha060::{
    AmberParser,
    Spanned,
    Statement,
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
    ))
    .boxed()
    .labelled("keyword")
}
