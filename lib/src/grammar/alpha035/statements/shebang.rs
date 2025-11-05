use chumsky::prelude::*;

use crate::grammar::alpha035::{lexer::Token, AmberParser, Spanned, Statement};

pub fn shebang_parser<'a>() -> impl AmberParser<'a, Spanned<Statement>> {
    any()
        .filter(|t: &Token| t.to_string().starts_with("#!"))
        .map_with(|shebang, e| (Statement::Shebang(shebang.to_string()), e.span()))
        .boxed()
}
