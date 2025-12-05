use chumsky::prelude::*;

use crate::alpha034::parser::default_recovery;
use crate::alpha034::{
    AmberParser,
    Expression,
    Spanned,
    Statement,
};
use crate::T;

use super::or::or_parser;

pub fn range_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    or_parser(stmnts.clone(), expr.clone())
        .foldl(
            just(T![".."])
                .ignore_then(just(T!["="]).or_not())
                .ignore_then(
                    or_parser(stmnts, expr).recover_with(via_parser(
                        default_recovery()
                            .or_not()
                            .map_with(|_, e| (Expression::Error, e.span())),
                    )),
                )
                .repeated(),
            |start, end| {
                let span = SimpleSpan::from(start.1.start..end.1.end);

                (Expression::Range(Box::new(start), Box::new(end)), span)
            },
        )
        .boxed()
        .labelled("expression")
}
