use amber_types::Spanned;
use chumsky::prelude::*;

use crate::alpha060::parser::default_recovery;
use crate::alpha060::{
    AmberParser,
    Statement,
};
use crate::{
    CompilerFlag,
    T,
};

pub fn compiler_flag_parser<'a>() -> impl AmberParser<'a, Spanned<Statement>> {
    just(T!["#["])
        .ignore_then(
            choice((
                just(T!["allow_nested_if_else"]).to(CompilerFlag::AllowNestedIfElse),
                just(T!["allow_generic_return"]).to(CompilerFlag::AllowGenericReturn),
                just(T!["allow_absurd_cast"]).to(CompilerFlag::AllowAbsurdCast),
                just(T!["allow_public_mutable"]).to(CompilerFlag::AllowPublicMutable),
            ))
            .recover_with(via_parser(
                default_recovery().or_not().map(|_| CompilerFlag::Error),
            )),
        )
        .then_ignore(
            just(T!["]"]).recover_with(via_parser(default_recovery().or_not().map(|_| T!["]"]))),
        )
        .map_with(|flag, e| (Statement::CompilerFlag((flag, e.span())), e.span()))
        .labelled("compiler flag")
        .boxed()
}
