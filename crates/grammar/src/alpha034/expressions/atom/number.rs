use chumsky::prelude::*;

use crate::alpha034::{
    AmberParser,
    Expression,
};
use crate::{
    Spanned,
    Token,
};

pub fn number_parser<'a>() -> impl AmberParser<'a, Spanned<Expression>> {
    any()
        .try_map(|token: Token, span| {
            let word = token.to_string();

            let num_str = if word.starts_with('.') {
                // For numbers like ".4", prepend "0" to make "0.4"
                format!("0{}", word)
            } else {
                word
            };

            // Parse as f32
            num_str
                .parse::<f32>()
                .map_err(|_| Rich::custom(span, format!("invalid number format: {}", num_str)))
        })
        .map_with(|num, e| (Expression::Number((num, e.span())), e.span()))
        .boxed()
        .labelled("number")
}
