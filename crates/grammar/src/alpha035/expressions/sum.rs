use chumsky::prelude::*;

use crate::alpha035::parser::default_recovery;
use crate::alpha035::{
    AmberParser,
    Expression,
    Spanned,
    Statement,
};
use crate::T;

use super::product::product_parser;

pub fn sum_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    product_parser(stmnts.clone(), expr.clone())
        .foldl(
            choice((
                just(T!['+']).to(Expression::Add as fn(_, _) -> _),
                just(T!['-']).to(Expression::Subtract as fn(_, _) -> _),
            ))
            .then(
                product_parser(stmnts, expr).recover_with(via_parser(
                    default_recovery()
                        .or_not()
                        .map_with(|_, e| (Expression::Error, e.span())),
                )),
            )
            .repeated(),
            |lhs, (op, rhs)| {
                let span = SimpleSpan::from(lhs.1.start..rhs.1.end);
                (op(Box::new(lhs), Box::new(rhs)), span)
            },
        )
        .boxed()
        .labelled("expression")
}
