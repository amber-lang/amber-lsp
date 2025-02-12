use crate::{
    grammar::alpha034::{lexer::Token, parser::default_recovery, AmberParser, Spanned},
    T,
};

use super::super::Expression;
use chumsky::prelude::*;

pub fn parentheses_parser<'a>(
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    expr.recover_with(via_parser(
        default_recovery()
            .or_not()
            .map_with(|_, e| (Expression::Error, e.span())),
    ))
    .delimited_by(
        just(T!['(']),
        just(T![')']).recover_with(via_parser(
            default_recovery()
                .repeated()
                .then(just(T![')']))
                .or_not()
                .map(|_| T![')']),
        )),
    )
    .map_with(|expr, e| (Expression::Parentheses(Box::new(expr)), e.span()))
    .boxed()
}
