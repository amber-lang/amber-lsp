use chumsky::prelude::*;

use crate::alpha034::{
    AmberParser,
    Comment,
    Spanned,
};
use amber_types::Token;

pub fn comment_parser<'a>() -> impl AmberParser<'a, Spanned<Comment>> {
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
