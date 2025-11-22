use chumsky::prelude::*;

use crate::grammar::alpha040::{
    AmberParser,
    CommandModifier,
    Spanned,
};
use crate::grammar::Token;
use crate::T;

pub fn modifier_parser<'a>() -> impl AmberParser<'a, Spanned<CommandModifier>> {
    choice((
        just(T!["trust"]).to(CommandModifier::Trust),
        just(T!["unsafe"]).to(CommandModifier::Unsafe),
        just(T!["silent"]).to(CommandModifier::Silent),
    ))
    .map_with(|modifier, e| (modifier, e.span()))
    .labelled("modifier")
    .boxed()
}
