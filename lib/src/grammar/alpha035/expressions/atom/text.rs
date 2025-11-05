use crate::{
    grammar::alpha035::{
        lexer::Token, parser::default_recovery, AmberParser, Expression, InterpolatedText, Spanned,
    },
    T,
};
use chumsky::prelude::*;

pub fn text_parser<'a>(
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    let escaped = just(T!['\\'])
        .ignore_then(any())
        .map_with(|char: Token, e| InterpolatedText::Escape((char.to_string(), e.span())))
        .boxed();

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
                    .map_with(|text, e| InterpolatedText::Text((text.to_string(), e.span()))),
                escaped,
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
