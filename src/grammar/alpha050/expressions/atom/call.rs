use crate::{
    grammar::alpha050::{
        lexer::Token,
        parser::{default_recovery, ident},
        statements::{failable_handlers::failable_handlers_parser, modifiers::modifier_parser},
        AmberParser, Expression, Spanned, Statement,
    },
    T,
};
use chumsky::prelude::*;

pub fn function_call_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    modifier_parser()
        .repeated()
        .collect()
        .then(ident("function".to_string()).map_with(|name, e| (name, e.span())))
        .then_ignore(just(T!["("]))
        .then(
            expr.recover_with(via_parser(
                default_recovery().map_with(|_, e| (Expression::Error, e.span())),
            ))
            .separated_by(
                just(T![","])
                    .recover_with(via_parser(default_recovery().rewind().map(|_| T![","]))),
            )
            .allow_trailing()
            .collect(),
        )
        .then_ignore(
            just(T![")"]).recover_with(via_parser(default_recovery().or_not().map(|_| T![")"]))),
        )
        .then(failable_handlers_parser(stmnts))
        .map_with(|(((modifier, name), args), failure), e| {
            (
                Expression::FunctionInvocation(modifier, name, args, failure),
                e.span(),
            )
        })
        .boxed()
}
