use chumsky::prelude::*;

use crate::alpha060::parser::default_recovery;
use crate::alpha060::statements::comment::comment_parser;
use crate::alpha060::{
    AmberParser,
    Expression,
    Spanned,
};
use crate::T;

pub fn array_parser<'a>(
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    expr.recover_with(via_parser(
        default_recovery().map_with(|_, e| (Expression::Error, e.span())),
    ))
    .separated_by(
        comment_parser()
            .repeated()
            .then(just(T![","]))
            .then(comment_parser().repeated()),
    )
    .allow_trailing()
    .collect()
    .delimited_by(
        just(T!["["]).then(comment_parser().repeated()),
        just(T!["]"]).recover_with(via_parser(default_recovery().or_not().map(|_| T!["]"]))),
    )
    .map_with(move |arr, e| (Expression::Array(arr), e.span()))
    .boxed()
    .labelled("array")
}
