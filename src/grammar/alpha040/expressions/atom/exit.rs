use chumsky::prelude::*;

use crate::{
    grammar::alpha040::{
        lexer::Token,
        AmberParser,
        Expression,
        Spanned,
    },
    T,
};

pub fn exit_parser<'a>(
    exp: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    just(T!["exit"])
        .map_with(|_, e| ("exit".to_string(), e.span()))
        .then(exp.or_not())
        .map_with(|(exit_name, exit_code), e| {
            (
                Expression::Exit(exit_name, exit_code.map(Box::new)),
                e.span(),
            )
        })
}
