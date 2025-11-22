use crate::grammar::alpha040::parser::ident;
use crate::grammar::alpha040::{
    AmberParser,
    Spanned,
};

use super::Expression;
use chumsky::prelude::*;

pub fn var_parser<'a>() -> impl AmberParser<'a, Spanned<Expression>> {
    ident("variable".to_string())
        .map_with(|name, e| (Expression::Var((name, e.span())), e.span()))
        .boxed()
        .labelled("variable")
}
