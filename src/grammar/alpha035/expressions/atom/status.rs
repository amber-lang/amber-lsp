use crate::grammar::alpha035::{
    AmberParser,
    Expression,
    Spanned,
};
use crate::grammar::Token;
use crate::T;
use chumsky::prelude::*;

pub fn status_var_parser<'a>() -> impl AmberParser<'a, Spanned<Expression>> {
    just(T!["status"])
        .map_with(|_, e| (Expression::Status, e.span()))
        .boxed()
}
