use chumsky::prelude::*;

use crate::{
    grammar::alpha050::{
        expressions::parse_expr,
        parser::default_recovery,
        AmberParser,
        Expression,
        Spanned,
        Statement,
        Token,
    },
    T,
};

use super::{
    failable_handlers::failable_handlers_parser,
    modifiers::modifier_parser,
};

pub fn move_files_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<Statement>> {
    modifier_parser()
        .repeated()
        .collect()
        .then(just(T!["mv"]).map_with(|modif, e| (modif.to_string(), e.span())))
        .then(
            parse_expr(stmnts.clone()).recover_with(via_parser(
                default_recovery()
                    .or_not()
                    .map_with(|_, e| (Expression::Error, e.span())),
            )),
        )
        .then(
            parse_expr(stmnts.clone()).recover_with(via_parser(
                default_recovery()
                    .or_not()
                    .map_with(|_, e| (Expression::Error, e.span())),
            )),
        )
        .then(failable_handlers_parser(stmnts.clone()))
        .map_with(|((((modif, mv), src), dest), fail), e| {
            (
                Statement::MoveFiles(modif, mv, Box::new(src), Box::new(dest), fail),
                e.span(),
            )
        })
}
