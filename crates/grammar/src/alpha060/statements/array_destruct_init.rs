use chumsky::prelude::*;

use crate::alpha060::expressions::parse_expr;
use crate::alpha060::parser::{
    default_recovery,
    ident,
};
use crate::alpha060::statements::comment::comment_parser;
use crate::alpha060::{
    AmberParser,
    Spanned,
    Statement,
    VariableInitType,
};
use crate::T;

pub fn array_destruct_init_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<Statement>> {
    just(T!["let"])
        .map_with(|_, e| ("let".to_string(), e.span()))
        .then(
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
                ),
        )
        .then_ignore(
            just(T!["="]).recover_with(via_parser(default_recovery().or_not().map(|_| T!["="]))),
        )
        .then(
            parse_expr(stmnts)
                .map(VariableInitType::Expression)
                .recover_with(via_parser(
                    default_recovery().or_not().map(|_| VariableInitType::Error),
                ))
                .map_with(|val, e| (val, e.span())),
        )
        .map_with(|((let_keyword, names), value), e| {
            (
                Statement::ArrayDestructInit(let_keyword, names, value),
                e.span(),
            )
        })
        .boxed()
        .labelled("array destructuring init")
}
