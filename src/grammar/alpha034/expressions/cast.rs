use chumsky::prelude::*;

use crate::{
    grammar::alpha034::{
        global::type_parser, lexer::Token, AmberParser, Expression, Spanned, Statement,
    },
    T,
};

use super::unary::unary_parser;

pub fn cast_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    unary_parser(stmnts, expr)
        .foldl(
            just(T!["as"])
                .map_with(|t, e| (t.to_string(), e.span()))
                .then(type_parser())
                .repeated(),
            |expr, (as_keyword, cast)| {
                let span = SimpleSpan::new(expr.1.start, cast.1.end);

                (Expression::Cast(Box::new(expr), as_keyword, cast), span)
            },
        )
        .boxed()
}
