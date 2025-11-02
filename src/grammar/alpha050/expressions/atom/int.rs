use chumsky::prelude::*;

use crate::grammar::alpha050::{lexer::Token, AmberParser, Spanned};

use super::Expression;

pub fn int_parser<'a>() -> impl AmberParser<'a, Spanned<Expression>> {
    let int = any().try_map(|token: Token, span| {
        let word = token.to_string();

        for char in word.chars() {
            if !char.is_ascii_digit() {
                return Err(Rich::custom(span, "int must contain only digits"));
            }
        }

        Ok(word)
    });

    int.from_str::<u32>()
        .unwrapped()
        .map_with(|int, e| (Expression::Int((int, e.span())), e.span()))
        .boxed()
        .labelled("int")
}
