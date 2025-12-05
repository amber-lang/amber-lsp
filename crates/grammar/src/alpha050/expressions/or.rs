use chumsky::prelude::*;

use crate::alpha050::parser::default_recovery;
use crate::alpha050::{
    AmberParser,
    Spanned,
    Statement,
};
use crate::T;

use super::and::and_parser;
use super::Expression;

pub fn or_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    and_parser(stmnts.clone(), expr.clone())
        .foldl(
            just(T!["or"])
                .map_with(|t, e| (t.to_string(), e.span()))
                .then(
                    and_parser(stmnts, expr).recover_with(via_parser(
                        default_recovery()
                            .or_not()
                            .map_with(|_, e| (Expression::Error, e.span())),
                    )),
                )
                .repeated(),
            |lhs, (or_keyword, rhs)| {
                let span = SimpleSpan::from(lhs.1.start..rhs.1.end);

                (
                    Expression::Or(Box::new(lhs), or_keyword, Box::new(rhs)),
                    span,
                )
            },
        )
        .boxed()
        .labelled("expression")
}
