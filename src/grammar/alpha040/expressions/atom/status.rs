use crate::{
    grammar::alpha040::{lexer::Token, AmberParser, Expression, Spanned},
    T,
};
use chumsky::prelude::*;

pub fn status_var_parser<'a>() -> impl AmberParser<'a, Spanned<Expression>> {
    just(T!["status"])
        .map_with(|_, e| (Expression::Status, e.span()))
        .boxed()
}
