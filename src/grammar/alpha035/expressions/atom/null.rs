use crate::grammar::alpha035::{
    AmberParser,
    Expression,
    Spanned,
};
use crate::grammar::Token;
use crate::T;
use chumsky::prelude::*;

pub fn null_parser<'a>() -> impl AmberParser<'a, Spanned<Expression>> {
    just(T!["null"])
        .map_with(|_, e| (Expression::Null, e.span()))
        .boxed()
}
