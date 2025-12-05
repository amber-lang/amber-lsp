use crate::grammar::alpha035::parser::default_recovery;
use crate::grammar::alpha035::{
    AmberParser,
    Expression,
    InterpolatedText,
    Spanned,
};
use crate::grammar::Token;
use crate::T;
use chumsky::prelude::*;

pub fn text_parser<'a>(
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    let interpolated = expr
        .recover_with(via_parser(
            default_recovery()
                .or_not()
                .map_with(|_, e| (Expression::Error, e.span())),
        ))
        .delimited_by(
            just(T!['{']),
            just(T!['}']).recover_with(via_parser(
                default_recovery()
                    .repeated()
                    .then(just(T!['}']))
                    .or_not()
                    .map(|_| T!['}']),
            )),
        )
        .map(|expr| InterpolatedText::Expression(Box::new(expr)))
        .boxed();

    just(T!['"'])
        .ignore_then(
            choice((
                any()
                    .filter(|c: &Token| *c != T!['"'] && *c != T!['{'] && *c != T!['\\'])
                    .map_with(|text, e| InterpolatedText::Text((text.to_string(), e.span())))
                    .labelled("text string"),
                interpolated,
            ))
            .map_with(|expr, e| (expr, e.span()))
            .repeated()
            .collect(),
        )
        .then_ignore(
            just(T!['"']).recover_with(via_parser(default_recovery().or_not().map(|_| T!['"']))),
        )
        .map_with(|expr, e| (Expression::Text(expr), e.span()))
        .boxed()
        .labelled("text")
}
