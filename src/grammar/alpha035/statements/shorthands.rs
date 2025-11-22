use chumsky::prelude::*;

use crate::grammar::alpha035::expressions::parse_expr;
use crate::grammar::alpha035::parser::{
    default_recovery,
    ident,
};
use crate::grammar::alpha035::{
    AmberParser,
    Expression,
    Spanned,
    Statement,
};
use crate::grammar::Token;
use crate::T;

pub fn shorthand_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<Statement>> {
    ident("variable".to_string())
        .map_with(|name, e| (name, e.span()))
        .then(choice((
            just(T!["+="]).to(Statement::ShorthandAdd as fn(_, _) -> _),
            just(T!["-="]).to(Statement::ShorthandSub as fn(_, _) -> _),
            just(T!["*="]).to(Statement::ShorthandMul as fn(_, _) -> _),
            just(T!["/="]).to(Statement::ShorthandDiv as fn(_, _) -> _),
            just(T!["%="]).to(Statement::ShorthandModulo as fn(_, _) -> _),
        )))
        .then(
            parse_expr(stmnts).recover_with(via_parser(
                default_recovery()
                    .or_not()
                    .map_with(|_, e| (Expression::Error, e.span())),
            )),
        )
        .map_with(|((name, op), value), e| (op(name, Box::new(value)), e.span()))
        .boxed()
}
