use chumsky::prelude::*;

use crate::{
    grammar::alpha040::{lexer::Token, AmberParser, Spanned},
    T,
};

use super::Expression;

pub fn number_parser<'a>() -> impl AmberParser<'a, Spanned<Expression>> {
    let int = any().try_map(|token: Token, span| {
        let word = token.to_string();

        for char in word.chars() {
            if !char.is_ascii_digit() {
                return Err(Rich::custom(span, "int must contain only digits"));
            }
        }

        Ok(word)
    });

    choice((
        int.then(just(T!['.']).ignore_then(int))
            .map(|(int, float)| format!("{int}.{float}")),
        just(T!['.'])
            .ignore_then(int)
            .map(|float| format!("0.{float}")),
        int.map(|int| format!("{int}.0")),
    ))
    .from_str::<f32>()
    .unwrapped()
    .map_with(|num, e| (Expression::Number((num, e.span())), e.span()))
    .boxed()
    .labelled("number")
}
