use chumsky::prelude::*;

use crate::{
    grammar::alpha040::{
        expressions::parse_expr,
        global::type_parser,
        lexer::Token,
        parser::{
            default_recovery,
            ident,
        },
        AmberParser,
        Spanned,
        Statement,
        VariableInitType,
    },
    T,
};

pub fn var_init_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
) -> impl AmberParser<'a, Spanned<Statement>> {
    just(T!["let"])
        .map_with(|_, e| ("let".to_string(), e.span()))
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
            choice((
                type_parser().map(VariableInitType::DataType),
                parse_expr(stmnts).map(VariableInitType::Expression),
            ))
            .recover_with(via_parser(
                default_recovery().or_not().map(|_| VariableInitType::Error),
            ))
            .map_with(|val, e| (val, e.span())),
        )
        .map_with(|((let_keyword, name), value), e| {
            (Statement::VariableInit(let_keyword, name, value), e.span())
        })
        .boxed()
}
