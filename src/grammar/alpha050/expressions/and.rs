use chumsky::prelude::*;

use crate::{
    grammar::alpha050::{
        lexer::Token, parser::default_recovery, AmberParser, Expression, Spanned, Statement,
    },
    T,
};

use super::comparison::comparison_parser;

pub fn and_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    comparison_parser(stmnts.clone(), expr.clone())
        .foldl(
            just(T!["and"])
                .map_with(|t, s| (t.to_string(), s.span()))
                .then(
                    comparison_parser(stmnts, expr).recover_with(via_parser(
                        default_recovery()
                            .or_not()
                            .map_with(|_, e| (Expression::Error, e.span())),
                    )),
                )
                .repeated(),
            |lhs, (and_keyword, rhs)| {
                let span = SimpleSpan::new(lhs.1.start, rhs.1.end);

                (
                    Expression::And(Box::new(lhs), and_keyword, Box::new(rhs)),
                    span,
                )
            },
        )
        .boxed()
        .labelled("expression")
}
