use chumsky::prelude::*;

use crate::alpha050::expressions::parse_expr;
use crate::alpha050::parser::{
    default_recovery,
    ident,
};
use crate::alpha050::{
    AmberParser,
    Expression,
    Spanned,
    Statement,
};
use crate::T;

pub fn array_index_set_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<Statement>> {
    ident("variable".to_string())
        .map_with(|name, e| (name, e.span()))
        .then_ignore(just(T!['[']))
        .then(
            parse_expr(stmnts.clone()).recover_with(via_parser(
                default_recovery()
                    .or_not()
                    .map_with(|_, e| (Expression::Error, e.span())),
            )),
        )
        .then_ignore(
            just(T![']']).recover_with(via_parser(default_recovery().or_not().map(|_| T![']']))),
        )
        .then_ignore(just(T!["="]))
        .then(
            parse_expr(stmnts).recover_with(via_parser(
                default_recovery()
                    .or_not()
                    .map_with(|_, e| (Expression::Error, e.span())),
            )),
        )
        .map_with(|((name, index), value), e| {
            (
                Statement::ArrayIndexSet(name, Box::new(index), Box::new(value)),
                e.span(),
            )
        })
        .boxed()
        .labelled("array index set")
}
