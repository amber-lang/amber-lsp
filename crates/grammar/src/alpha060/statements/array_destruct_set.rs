use chumsky::prelude::*;

use crate::alpha060::expressions::parse_expr;
use crate::alpha060::parser::{
    default_recovery,
    ident,
};
use crate::alpha060::statements::comment::comment_parser;
use crate::alpha060::{
    AmberParser,
    Expression,
    Spanned,
    Statement,
};
use crate::T;

pub fn array_destruct_set_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<Statement>> {
    ident("variable".to_string())
        .map_with(|name, e| (name, e.span()))
        .separated_by(
            comment_parser()
                .repeated()
                .then(just(T![","]))
                .then(comment_parser().repeated()),
        )
        .at_least(1)
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(
            just(T!["["]).then(comment_parser().repeated()),
            just(T!["]"]),
        )
        .then_ignore(just(T!["="]))
        .then(
            parse_expr(stmnts).recover_with(via_parser(
                default_recovery()
                    .or_not()
                    .map_with(|_, e| (Expression::Error, e.span())),
            )),
        )
        .map_with(|(names, value), e| {
            (
                Statement::ArrayDestructSet(names, Box::new(value)),
                e.span(),
            )
        })
        .boxed()
        .labelled("array destructuring set")
}
