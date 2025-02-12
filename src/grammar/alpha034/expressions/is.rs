use chumsky::prelude::*;

use crate::{
    grammar::alpha034::{
        global::type_parser, lexer::Token, AmberParser, Expression, Spanned, Statement,
    },
    T,
};

use super::cast::cast_parser;

pub fn is_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    cast_parser(stmnts, expr.clone())
        .foldl(
            just(T!["is"])
                .map_with(|t, e| (t.to_string(), e.span()))
                .then(type_parser())
                .repeated(),
            |expr, (is_keyword, cast)| {
                let span = SimpleSpan::new(expr.1.start, cast.1.end);

                (Expression::Is(Box::new(expr), is_keyword, cast), span)
            },
        )
        .boxed()
}
