use chumsky::prelude::*;

use crate::grammar::alpha035::parser::default_recovery;
use crate::grammar::alpha035::{
    AmberParser,
    Spanned,
    Statement,
};
use crate::grammar::Token;
use crate::T;

use super::range::range_parser;
use super::Expression;

pub fn ternary_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    range_parser(stmnts, expr.clone())
        .foldl(
            just(T!["then"])
                .map_with(|t, e| (t.to_string(), e.span()))
                .then(
                    expr.clone().recover_with(via_parser(
                        default_recovery()
                            .or_not()
                            .map_with(|_, e| (Expression::Error, e.span())),
                    )),
                )
                .then(
                    just(T!["else"])
                        .map_with(|t, e| (t.to_string(), e.span()))
                        .recover_with(via_parser(
                            default_recovery()
                                .or_not()
                                .map_with(|_, e| ("".to_string(), e.span())),
                        )),
                )
                .then(
                    expr.recover_with(via_parser(
                        default_recovery()
                            .or_not()
                            .map_with(|_, e| (Expression::Error, e.span())),
                    )),
                )
                .repeated(),
            |cond, (((then_keyword, if_true), else_keyword), if_false)| {
                let span = SimpleSpan::new(cond.1.start, if_false.1.end);

                (
                    Expression::Ternary(
                        Box::new(cond),
                        then_keyword,
                        Box::new(if_true),
                        else_keyword,
                        Box::new(if_false),
                    ),
                    span,
                )
            },
        )
        .boxed()
        .labelled("expression")
}
