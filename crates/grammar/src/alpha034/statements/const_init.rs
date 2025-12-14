use chumsky::prelude::*;

use crate::alpha034::expressions::parse_expr;
use crate::alpha034::parser::{
    default_recovery,
    ident,
};
use crate::alpha034::{
    AmberParser,
    Expression,
    Spanned,
    Statement,
};
use crate::T;

pub fn const_init_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<Statement>> {
    just(T!["const"])
        .map_with(|_, e| ("const".to_string(), e.span()))
        .then(
            ident("variable".to_string())
                .recover_with(via_parser(
                    default_recovery().or_not().map(|_| "".to_string()),
                ))
                .map_with(|name, e| (name, e.span())),
        )
        .then_ignore(
            just(T!["="]).recover_with(via_parser(default_recovery().or_not().map(|_| T!["="]))),
        )
        .then(
            parse_expr(stmnts).recover_with(via_parser(
                default_recovery()
                    .or_not()
                    .map_with(|_, e| (Expression::Error, e.span())),
            )),
        )
        .map_with(|((const_keyword, name), value), e| {
            (
                Statement::ConstInit(const_keyword, name, Box::new(value)),
                e.span(),
            )
        })
        .boxed()
}
