use crate::{
    grammar::{
        alpha040::parser::default_recovery,
        alpha040::{lexer::Token, AmberParser, Spanned, Statement},
    },
    T,
};

use super::super::Expression;
use chumsky::prelude::*;

mod array;
mod bool;
mod call;
mod command;
mod exit;
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
        exit::exit_parser(expr.clone()),
        parentheses::parentheses_parser(expr.clone()),
        bool::bool_parser(),
        null::null_parser(),
        status::status_var_parser(),
        call::function_call_parser(stmnts.clone(), expr.clone()),
        var::var_parser(),
        text::text_parser(expr.clone()),
        array::array_parser(expr.clone()),
        command::command_parser(stmnts.clone(), expr.clone()),
        number::number_parser(),
    ))
    .boxed()
}

pub fn array_index_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    atom_parser(stmnts.clone(), expr.clone())
        .foldl(
            just(T!['['])
                .ignore_then(
                    expr.recover_with(via_parser(
                        default_recovery()
                            .or_not()
                            .map_with(|_, e| (Expression::Error, e.span())),
                    )),
                )
                .then_ignore(
                    just(T![']'])
                        .recover_with(via_parser(default_recovery().or_not().map(|_| T![']']))),
                )
                .repeated(),
            |expr, index| {
                let span = SimpleSpan::new(expr.1.start, index.1.end);

                (
                    Expression::ArrayIndex(Box::new(expr), Box::new(index)),
                    span,
                )
            },
        )
        .boxed()
        .labelled("array index")
}
