use chumsky::prelude::*;

use crate::alpha034::parser::default_recovery;
use crate::alpha034::{
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
        just(T![","]).recover_with(via_parser(default_recovery().rewind().map(|_| T![","]))),
    )
    .collect()
    .delimited_by(
        just(T!["["]),
        just(T!["]"]).recover_with(via_parser(default_recovery().or_not().map(|_| T!["]"]))),
    )
    .map_with(move |arr, e| (Expression::Array(arr), e.span()))
    .boxed()
}
