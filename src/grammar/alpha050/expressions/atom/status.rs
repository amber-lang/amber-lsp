use crate::{
    grammar::alpha050::{
        AmberParser,
        Expression,
        Spanned,
        Token,
    },
    T,
};
use chumsky::prelude::*;

pub fn status_var_parser<'a>() -> impl AmberParser<'a, Spanned<Expression>> {
    just(T!["status"])
        .map_with(|_, e| (Expression::Status, e.span()))
        .boxed()
}
