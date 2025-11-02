use crate::grammar::alpha050::{parser::ident, AmberParser, Spanned};

use super::Expression;
use chumsky::prelude::*;

pub fn var_parser<'a>() -> impl AmberParser<'a, Spanned<Expression>> {
    ident("variable".to_string())
        .map_with(|name, e| (Expression::Var((name, e.span())), e.span()))
        .boxed()
        .labelled("variable")
}
