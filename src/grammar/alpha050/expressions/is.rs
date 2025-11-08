use chumsky::prelude::*;

use crate::{
    grammar::alpha050::{
        global::type_parser,
        parser::default_recovery,
        AmberParser,
        DataType,
        Expression,
        Spanned,
        Statement,
        Token,
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
                .then(
                    type_parser().recover_with(via_parser(
                        default_recovery()
                            .or_not()
                            .map_with(|_, e| (DataType::Error, e.span())),
                    )),
                )
                .repeated(),
            |expr, (is_keyword, cast)| {
                let span = SimpleSpan::new(expr.1.start, cast.1.end);

                (Expression::Is(Box::new(expr), is_keyword, cast), span)
            },
        )
        .boxed()
        .labelled("expression")
}
