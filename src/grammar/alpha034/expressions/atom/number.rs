use chumsky::prelude::*;

use crate::{
    grammar::alpha034::{lexer::Token, AmberParser, Spanned},
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

    int.then(just(T!['.']).ignore_then(int).or_not())
        .map(|(int, float)| {
            let float = float.unwrap_or('0'.to_string());

            format!("{}.{}", int, float)
        })
        .from_str::<f32>()
        .unwrapped()
        .map_with(|num, e| (Expression::Number((num, e.span())), e.span()))
        .boxed()
        .labelled("number")
}
