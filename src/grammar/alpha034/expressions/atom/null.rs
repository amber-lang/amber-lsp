use crate::{
    grammar::alpha034::{
        lexer::Token,
        AmberParser,
        Expression,
        Spanned,
    },
    T,
};
use chumsky::prelude::*;

pub fn null_parser<'a>() -> impl AmberParser<'a, Spanned<Expression>> {
    just(T!["null"])
        .map_with(|_, e| (Expression::Null, e.span()))
        .boxed()
}
