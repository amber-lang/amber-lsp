use chumsky::prelude::*;

use crate::grammar::alpha040::{lexer::Token, AmberParser, Spanned, Statement};

pub fn doc_string_parser<'a>() -> impl AmberParser<'a, Spanned<Statement>> {
    any()
        .filter(|t: &Token| t.to_string().starts_with("///"))
        .map_with(|doc, e| {
            (
                Statement::DocString(doc.to_string()[3..].trim().to_string()),
                e.span(),
            )
        })
        .boxed()
}
