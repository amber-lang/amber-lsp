use chumsky::prelude::*;

use crate::{
    grammar::alpha035::{
        lexer::Token,
        parser::default_recovery,
        AmberParser,
        FailureHandler,
        Spanned,
        Statement,
    },
    T,
};

pub fn failure_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<FailureHandler>> {
    let handle_parser = just(T!["failed"])
        .map_with(|t, e| (t.to_string(), e.span()))
        .then_ignore(
            just(T!["{"]).recover_with(via_parser(default_recovery().or_not().map(|_| T!["{"]))),
        )
        .then(
            stmnts
                .recover_with(via_parser(
                    default_recovery().map_with(|_, e| (Statement::Error, e.span())),
                ))
                .repeated()
                .collect(),
        )
        .then_ignore(
            just(T!["}"]).recover_with(via_parser(default_recovery().or_not().map(|_| T!["}"]))),
        )
        .map(|(failed_keyword, block)| FailureHandler::Handle(failed_keyword, block))
        .boxed();

    let prop_parser = just(T!['?']).map(|_| FailureHandler::Propagate).boxed();

    choice((handle_parser, prop_parser))
        .map_with(|handler, e| (handler, e.span()))
        .boxed()
}
