use chumsky::prelude::*;

use crate::{
    grammar::alpha034::{
        lexer::Token,
        parser::default_recovery,
        AmberParser,
        Expression,
        Spanned,
        Statement,
    },
    T,
};

use super::is::is_parser;

pub fn product_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    is_parser(stmnts.clone(), expr.clone())
        .foldl(
            choice((
                just(T!['*']).to(Expression::Multiply as fn(_, _) -> _),
                just(T!['/']).to(Expression::Divide as fn(_, _) -> _),
                just(T!['%']).to(Expression::Modulo as fn(_, _) -> _),
            ))
            .then(
                is_parser(stmnts, expr).recover_with(via_parser(
                    default_recovery()
                        .or_not()
                        .map_with(|_, e| (Expression::Error, e.span())),
                )),
            )
            .repeated(),
            |lhs, (op, rhs)| {
                let span = SimpleSpan::new(lhs.1.start, rhs.1.end);

                (op(Box::new(lhs), Box::new(rhs)), span)
            },
        )
        .boxed()
        .labelled("expression")
}
