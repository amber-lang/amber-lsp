use crate::grammar::alpha034::parser::{
    default_recovery,
    ident,
};
use crate::grammar::alpha034::statements::failed::failure_parser;
use crate::grammar::alpha034::statements::modifiers::modifier_parser;
use crate::grammar::alpha034::{
    AmberParser,
    Expression,
    Spanned,
    Statement,
};
use crate::grammar::Token;
use crate::T;
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
            .collect(),
        )
        .then_ignore(
            just(T![")"]).recover_with(via_parser(default_recovery().or_not().map(|_| T![")"]))),
        )
        .then(failure_parser(stmnts).or_not())
        .map_with(|(((modifier, name), args), failure), e| {
            (
                Expression::FunctionInvocation(modifier, name, args, failure),
                e.span(),
            )
        })
        .boxed()
}
