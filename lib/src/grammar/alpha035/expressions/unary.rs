use chumsky::prelude::*;

use crate::{
    grammar::alpha035::{lexer::Token, AmberParser, Expression, Spanned, Statement},
    T,
};

use super::atom::atom_parser;

pub fn unary_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    choice((
        just(T!['-'])
            .map_with(|t, e| ((t.to_string(), e.span()), Expression::Neg as fn(_, _) -> _)),
        just(T!["not"])
            .map_with(|t, e| ((t.to_string(), e.span()), Expression::Not as fn(_, _) -> _)),
        just(T!["nameof"]).map_with(|t, e| {
            (
                (t.to_string(), e.span()),
                Expression::Nameof as fn(_, _) -> _,
            )
        }),
    ))
    .repeated()
    .foldr(atom_parser(stmnts, expr), |(op_string, op), expr| {
        let span = SimpleSpan::new(expr.1.start, expr.1.end);

        (op(op_string, Box::new(expr)), span)
    })
    .boxed()
}
