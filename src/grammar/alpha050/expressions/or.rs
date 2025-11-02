use chumsky::prelude::*;

use crate::{
    grammar::alpha050::{lexer::Token, parser::default_recovery, AmberParser, Spanned, Statement},
    T,
};

use super::{and::and_parser, Expression};

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
                let span = SimpleSpan::new(lhs.1.start, rhs.1.end);

                (
                    Expression::Or(Box::new(lhs), or_keyword, Box::new(rhs)),
                    span,
                )
            },
        )
        .boxed()
        .labelled("expression")
}
