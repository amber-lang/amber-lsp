use chumsky::prelude::*;

use crate::grammar::alpha050::parser::{
    default_recovery,
    ident,
};
use crate::grammar::alpha050::statements::block::block_parser;
use crate::grammar::alpha050::statements::comment::comment_parser;
use crate::grammar::alpha050::{
    AmberParser,
    Block,
    FailableHandler,
    FailureHandler,
    Spanned,
    Statement,
    Token,
};
use crate::T;

fn failure_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<FailableHandler>> {
    let handle_parser = just(T!["failed"])
        .map_with(|t, e| (t.to_string(), e.span()))
        .then(
            just(T!['('])
                .ignore_then(
                    ident("failure code argument".to_string())
                        .recover_with(via_parser(default_recovery().map(|_| "".to_string())))
                        .map_with(|arg, e| (arg, e.span())),
                )
                .then_ignore(
                    just(T![')']).recover_with(via_parser(default_recovery().map(|_| T![')']))),
                )
                .or_not(),
        )
        .then(block_parser(stmnts, false, true).recover_with(via_parser(
            default_recovery().map_with(|_, e| (Block::Error, e.span())),
        )))
        .map(|((failed_keyword, failure_code), block)| {
            FailureHandler::Handle(failed_keyword, failure_code, Box::new(block))
        })
        .boxed();

    let prop_parser = just(T!['?']).map(|_| FailureHandler::Propagate).boxed();

    choice((handle_parser, prop_parser))
        .map_with(|handler, e| (handler, e.span()))
        .boxed()
        .map_with(|stmnt, e| (FailableHandler::Failure(stmnt), e.span()))
        .labelled("failure handler")
}

fn succeeded_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<FailableHandler>> {
    just(T!["succeeded"])
        .map_with(|t, e| (t.to_string(), e.span()))
        .then(block_parser(stmnts, false, true).recover_with(via_parser(
            default_recovery().map_with(|_, e| (Block::Error, e.span())),
        )))
        .map_with(|(succeeded_keyword, block), e| {
            (
                FailableHandler::Succeeded(succeeded_keyword, Box::new(block)),
                e.span(),
            )
        })
        .boxed()
        .labelled("succeeded handler")
}

fn exited_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<FailableHandler>> {
    just(T!["exited"])
        .map_with(|t, e| (t.to_string(), e.span()))
        .then(
            just(T!['('])
                .recover_with(via_parser(default_recovery().map(|_| T!['('])))
                .ignore_then(
                    ident("status code argument".to_string())
                        .recover_with(via_parser(default_recovery().map(|_| "".to_string())))
                        .map_with(|arg, e| (arg, e.span())),
                )
                .then_ignore(
                    just(T![')']).recover_with(via_parser(default_recovery().map(|_| T![')']))),
                ),
        )
        .then(block_parser(stmnts, false, true).recover_with(via_parser(
            default_recovery().map_with(|_, e| (Block::Error, e.span())),
        )))
        .map_with(|((succeeded_keyword, status_code), block), e| {
            (
                FailableHandler::Exited(succeeded_keyword, status_code, Box::new(block)),
                e.span(),
            )
        })
        .boxed()
        .labelled("exited handler")
}

fn comment_parser_in_failable<'a>() -> impl AmberParser<'a, Spanned<FailableHandler>> {
    comment_parser()
        .map_with(|comment, e| (FailableHandler::Comment(comment), e.span()))
        .boxed()
}

pub fn failable_handlers_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Vec<Spanned<FailableHandler>>> {
    choice((
        failure_parser(stmnts.clone()),
        succeeded_parser(stmnts.clone()),
        exited_parser(stmnts),
        comment_parser_in_failable(),
    ))
    .repeated()
    .collect()
    .boxed()
    .labelled("failure handler (?, failed, exited, succeeded)")
}
