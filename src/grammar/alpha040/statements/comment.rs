use chumsky::prelude::*;

use crate::grammar::alpha040::{
    lexer::Token,
    AmberParser,
    Comment,
    Spanned,
};

pub fn comment_parser<'a>() -> impl AmberParser<'a, Spanned<Comment>> {
    choice((doc_string_parser(), single_line_comment_parser())).boxed()
}

fn single_line_comment_parser<'a>() -> impl AmberParser<'a, Spanned<Comment>> {
    any()
        .filter(|t: &Token| t.to_string().starts_with("//"))
        .map_with(|com, e| {
            (
                Comment::Comment(com.to_string()[2..].trim().to_string()),
                e.span(),
            )
        })
        .boxed()
}

fn doc_string_parser<'a>() -> impl AmberParser<'a, Spanned<Comment>> {
    any()
        .filter(|t: &Token| t.to_string().starts_with("///"))
        .map_with(|doc, e| {
            (
                Comment::DocString(doc.to_string()[3..].trim().to_string()),
                e.span(),
            )
        })
        .boxed()
}
