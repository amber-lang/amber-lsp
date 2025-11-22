use crate::grammar::alpha050::{
    AmberParser,
    Expression,
    Spanned,
    Token,
};
use crate::T;
use chumsky::prelude::*;

pub fn null_parser<'a>() -> impl AmberParser<'a, Spanned<Expression>> {
    just(T!["null"])
        .map_with(|_, e| (Expression::Null, e.span()))
        .boxed()
}
