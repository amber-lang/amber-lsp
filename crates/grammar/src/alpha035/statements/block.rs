use chumsky::prelude::*;

use crate::alpha035::parser::default_recovery;
use crate::alpha035::{
    AmberParser,
    Block,
    Spanned,
    Statement,
};
use crate::T;

use super::modifiers::modifier_parser;

pub fn block_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
    with_modifiers: bool,
) -> impl AmberParser<'a, Spanned<Block>> {
    let body_parser = stmnts
        .recover_with(via_parser(
            default_recovery().map_with(|_, e| (Statement::Error, e.span())),
        ))
        .repeated()
        .collect()
        .delimited_by(
            just(T!['{']),
            just(T!['}']).recover_with(via_parser(default_recovery().or_not().map(|_| T!['}']))),
        )
        .boxed();

    if !with_modifiers {
        return body_parser
            .map_with(|body, e| (Block::Block(vec![], body), e.span()))
            .boxed();
    }

    modifier_parser()
        .repeated()
        .collect()
        .then(body_parser)
        .map_with(move |(modifier, body), e| (Block::Block(modifier, body), e.span()))
        .boxed()
}

pub fn block_parser_statement<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<Statement>> {
    block_parser(stmnts, true)
        .map_with(|block, e| (Statement::Block(block), e.span()))
        .boxed()
}
