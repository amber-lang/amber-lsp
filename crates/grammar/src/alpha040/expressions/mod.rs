use chumsky::prelude::*;

use super::{
    AmberParser,
    Expression,
    Spanned,
    Statement,
};

mod and;
mod atom;
mod cast;
mod comparison;
mod is;
mod or;
mod product;
mod range;
mod sum;
mod ternary;
mod unary;

pub fn parse_expr<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    recursive(move |expr| ternary::ternary_parser(stmnts, expr).labelled("expression")).boxed()
}
