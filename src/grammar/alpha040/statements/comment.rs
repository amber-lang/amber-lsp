use chumsky::prelude::*;

use crate::grammar::alpha040::{lexer::Token, AmberParser, Spanned, Statement};

pub fn comment_parser<'a>() -> impl AmberParser<'a, Spanned<Statement>> {
    any()
        .filter(|t: &Token| t.to_string().starts_with("//"))
        .map_with(|com, e| {
            (
                Statement::Comment(com.to_string()[2..].trim().to_string()),
                e.span(),
            )
        })
        .boxed()
}
