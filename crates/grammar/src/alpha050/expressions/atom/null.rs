use crate::alpha050::{
    AmberParser,
    Expression,
    Spanned,
};
use crate::T;
use chumsky::prelude::*;

pub fn null_parser<'a>() -> impl AmberParser<'a, Spanned<Expression>> {
    just(T!["null"])
        .map_with(|_, e| (Expression::Null, e.span()))
        .boxed()
}
