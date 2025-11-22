use crate::grammar::alpha035::parser::ident;
use crate::grammar::alpha035::{
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
