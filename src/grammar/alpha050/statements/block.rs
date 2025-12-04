use chumsky::prelude::*;

use crate::grammar::alpha050::parser::default_recovery;
use crate::grammar::alpha050::{
    AmberParser,
    Block,
    Spanned,
    Statement,
    Token,
};
use crate::T;

use super::modifiers::modifier_parser;

pub fn block_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
    with_modifiers: bool,
    with_singleline: bool,
) -> impl AmberParser<'a, Spanned<Block>> {
    let body_parser = stmnts
        .clone()
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

    let body_with_modifiers_parser = modifier_parser()
        .repeated()
        .collect()
        .then(body_parser.clone())
        .map_with(move |(modifier, body), e| (Block::Block(modifier, body), e.span()))
        .boxed();

    let singleline_parser = just(T![':'])
        .ignore_then(stmnts.recover_with(via_parser(
            default_recovery().map_with(|_, e| (Statement::Error, e.span())),
        )))
        .map_with(|body, e| (Block::Singleline(Box::new(body)), e.span()))
        .boxed();

    if with_singleline {
        if !with_modifiers {
            return choice((
                singleline_parser,
                body_parser
                    .map_with(|body, e| (Block::Block(vec![], body), e.span()))
                    .boxed(),
            ))
            .boxed();
        }

        return choice((singleline_parser, body_with_modifiers_parser)).boxed();
    }

    if !with_modifiers {
        return body_parser
            .map_with(|body, e| (Block::Block(vec![], body), e.span()))
            .boxed();
    }

    body_with_modifiers_parser
}

pub fn block_parser_statement<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<Statement>> {
    block_parser(stmnts, true, false)
        .map_with(|block, e| (Statement::Block(block), e.span()))
        .boxed()
        .labelled("block")
}
