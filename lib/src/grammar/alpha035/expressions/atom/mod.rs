use crate::grammar::alpha035::{AmberParser, Spanned, Statement};

use super::super::Expression;
use chumsky::prelude::*;

mod array;
mod bool;
mod call;
mod command;
mod null;
mod number;
mod parentheses;
mod status;
mod text;
mod var;

pub fn atom_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    choice((
        bool::bool_parser(),
        null::null_parser(),
        status::status_var_parser(),
        call::function_call_parser(stmnts.clone(), expr.clone()),
        var::var_parser(),
        text::text_parser(expr.clone()),
        array::array_parser(expr.clone()),
        command::command_parser(stmnts, expr.clone()),
        number::number_parser(),
        parentheses::parentheses_parser(expr),
    ))
    .boxed()
}
