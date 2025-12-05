use chumsky::prelude::*;

use crate::T;
use amber_types::{
    DataType,
    Token,
};

use super::parser::{
    default_recovery,
    ident,
};
use super::statements::statement_parser;
use super::{
    AmberParser,
    CompilerFlag,
    FunctionArgument,
    GlobalStatement,
    ImportContent,
    Spanned,
    Statement,
};

pub fn import_parser<'a>() -> impl AmberParser<'a, Spanned<GlobalStatement>> {
    let import_all_parser = just(T!["*"]).map_with(|_, e| (ImportContent::ImportAll, e.span()));

    let import_specific_parser = just(T!["{"])
        .ignore_then(
            ident("variable".to_string())
                .recover_with(via_parser(default_recovery().map(|_| "".to_string())))
                .map_with(|name, e| (name, e.span()))
                .separated_by(
                    just(T![","])
                        .recover_with(via_parser(default_recovery().rewind().map(|_| T![","]))),
                )
                .collect(),
        )
        .then_ignore(
            just(T!["}"]).recover_with(via_parser(default_recovery().or_not().map(|_| T!["}"]))),
        )
        .map_with(|vars, e| (ImportContent::ImportSpecific(vars), e.span()))
        .boxed();

    let path_parser = just(T!['"'])
        .then(
            any()
                .filter(|c| *c != T!['"'])
                .repeated()
                .collect::<Vec<Token>>()
                .map(|name| name.iter().map(|t| t.to_string()).collect::<String>()),
        )
        .then(
            just(T!['"']).recover_with(via_parser(
                default_recovery()
                    .repeated()
                    .then(just(T!['"']))
                    .or_not()
                    .map(|_| T!['"']),
            )),
        )
        .map_with(|((_, path), _): ((Token, String), Token), e| (path, e.span()))
        .boxed();

    choice((
        just(T!["pub"])
            .map_with(|_, e| (true, e.span()))
            .then(just(T!["import"]).map_with(|_, e| ("import".to_string(), e.span()))),
        just(T!["import"]).map_with(|_, e| ((false, e.span()), ("import".to_string(), e.span()))),
    ))
    .then(
        import_all_parser
            .or(import_specific_parser)
            .recover_with(via_parser(
                default_recovery()
                    .or_not()
                    .map_with(|_, e| (ImportContent::ImportAll, e.span())),
            )),
    )
    .then(
        just(T!["from"])
            .recover_with(via_parser(default_recovery().or_not().map(|_| T!["from"])))
            .map_with(|_, e| ("from".to_string(), e.span())),
    )
    .then(
        path_parser.recover_with(via_parser(
            default_recovery()
                .or_not()
                .map_with(|_, e| ("".to_string(), e.span())),
        )),
    )
    .map_with(|((((is_pub, imp), vars), from), path), e| {
        (
            GlobalStatement::Import(is_pub, imp, vars, from, path),
            e.span(),
        )
    })
    .boxed()
}

pub fn type_parser<'a>() -> impl AmberParser<'a, Spanned<DataType>> {
    let literal_type = choice((
        just(T!["Text"]).to(DataType::Text),
        just(T!["Num"]).to(DataType::Number),
        just(T!["Bool"]).to(DataType::Boolean),
        just(T!["Null"]).to(DataType::Null),
    ))
    .boxed();

    literal_type
        .clone()
        .or(just(T!["["])
            .ignore_then(literal_type)
            .then_ignore(just(T!["]"]))
            .map(|ty| DataType::Array(Box::new(ty))))
        .map_with(|ty, e| (ty, e.span()))
        .labelled("type")
        .boxed()
}

fn compiler_flag_parser<'a>() -> impl AmberParser<'a, Spanned<CompilerFlag>> {
    just(T!["#["])
        .ignore_then(
            choice((
                just(T!["allow_nested_if_else"]).to(CompilerFlag::AllowNestedIfElse),
                just(T!["allow_generic_return"]).to(CompilerFlag::AllowGenericReturn),
                just(T!["allow_absurd_cast"]).to(CompilerFlag::AllowAbsurdCast),
            ))
            .recover_with(via_parser(
                default_recovery().or_not().map(|_| CompilerFlag::Error),
            )),
        )
        .then_ignore(
            just(T!["]"]).recover_with(via_parser(default_recovery().or_not().map(|_| T!["]"]))),
        )
        .map_with(|flag, e| (flag, e.span()))
        .labelled("compiler flag")
        .boxed()
}

pub fn function_parser<'a>() -> impl AmberParser<'a, Spanned<GlobalStatement>> {
    let generic_arg_parser = just(T!["ref"])
        .or_not()
        .map_with(|is_ref, e| (is_ref.is_some(), e.span()))
        .then(ident("argument".to_string()))
        .map_with(|(is_ref, name), e| FunctionArgument::Generic(is_ref, (name, e.span())))
        .boxed();

    let typed_arg_parser = just(T!["ref"])
        .or_not()
        .map_with(|is_ref, e| (is_ref.is_some(), e.span()))
        .then(ident("argument".to_string()).map_with(|name, e| (name, e.span())))
        .then(
            just(T![":"]).ignore_then(
                type_parser().recover_with(via_parser(
                    default_recovery()
                        .or_not()
                        .map_with(|_, e| (DataType::Error, e.span())),
                )),
            ),
        )
        .map(|((is_ref, name), ty)| FunctionArgument::Typed(is_ref, name, ty))
        .boxed();

    let arg_parser = choice((typed_arg_parser, generic_arg_parser))
        .map_with(|arg, e| (arg, e.span()))
        .labelled("argument")
        .boxed();

    let args_parser = arg_parser
        .recover_with(via_parser(
            default_recovery().map_with(|_, e| (FunctionArgument::Error, e.span())),
        ))
        .separated_by(
            just(T![","]).recover_with(via_parser(default_recovery().rewind().map(|_| T![","]))),
        )
        .collect()
        .delimited_by(
            just(T!['(']),
            just(T![')']).recover_with(via_parser(default_recovery().or_not().map(|_| T![')']))),
        )
        .boxed();

    let ret_parser = just(T![":"])
        .ignore_then(
            type_parser().recover_with(via_parser(
                default_recovery()
                    .or_not()
                    .map_with(|_, e| (DataType::Error, e.span())),
            )),
        )
        .or_not()
        .then(
            just(T!["{"])
                .ignore_then(
                    statement_parser()
                        .recover_with(via_parser(
                            default_recovery().map_with(|_, e| (Statement::Error, e.span())),
                        ))
                        .repeated()
                        .collect(),
                )
                .then_ignore(
                    just(T!["}"])
                        .recover_with(via_parser(default_recovery().or_not().map(|_| T!["}"]))),
                ),
        )
        .boxed();

    compiler_flag_parser()
        .repeated()
        .collect()
        .then(
            just(T!["pub"])
                .or_not()
                .map_with(|is_pub, e| (is_pub.is_some(), e.span())),
        )
        .then(just(T!["fun"]).map_with(|_, e| ("fun".to_string(), e.span())))
        .then(
            ident("function".to_string())
                .map_err(|err| Rich::custom(*err.span(), "Expected function name"))
                .recover_with(via_parser(
                    default_recovery().or_not().map(|_| "".to_string()),
                ))
                .map_with(|name, e| (name, e.span())),
        )
        .then(args_parser.recover_with(via_parser(default_recovery().or_not().map(|_| vec![]))))
        .then(ret_parser.recover_with(via_parser(
            default_recovery().or_not().map(|_| (None, vec![])),
        )))
        .map_with(
            |(((((compiler_flags, is_pub), fun), name), args), (ty, body)), e| {
                (
                    GlobalStatement::FunctionDefinition(
                        compiler_flags,
                        is_pub,
                        fun,
                        name,
                        args,
                        ty,
                        body,
                    ),
                    e.span(),
                )
            },
        )
        .labelled("function")
        .boxed()
}

pub fn main_parser<'a>() -> impl AmberParser<'a, Spanned<GlobalStatement>> {
    just(T!["main"])
        .map_with(|_, e| ("main".to_string(), e.span()))
        .then(
            just(T!['('])
                .ignore_then(
                    ident("argument".to_string())
                        .recover_with(via_parser(default_recovery().map(|_| "".to_string()))),
                )
                .then_ignore(
                    just(T![')'])
                        .recover_with(via_parser(default_recovery().or_not().map(|_| T![')']))),
                )
                .map_with(|name, e| (name, e.span()))
                .or_not(),
        )
        .then(
            just(T!["{"])
                .recover_with(via_parser(
                    default_recovery()
                        .repeated()
                        .then(just(T!['{']))
                        .or_not()
                        .map(|_| T!["{"]),
                ))
                .ignore_then(
                    statement_parser()
                        .recover_with(via_parser(
                            default_recovery().map_with(|_, e| (Statement::Error, e.span())),
                        ))
                        .repeated()
                        .collect(),
                )
                .then_ignore(
                    just(T!["}"])
                        .recover_with(via_parser(default_recovery().or_not().map(|_| T!["}"]))),
                ),
        )
        .map_with(|((main, args), body), e| (GlobalStatement::Main(main, args, body), e.span()))
        .boxed()
}

pub fn global_statement_parser<'a>() -> impl AmberParser<'a, Vec<Spanned<GlobalStatement>>> {
    let statement = statement_parser()
        .map(|stmnt| (GlobalStatement::Statement(stmnt.clone()), stmnt.1))
        .boxed();

    choice((import_parser(), function_parser(), main_parser(), statement))
        .recover_with(via_parser(any().map_with(|_, e| {
            (
                GlobalStatement::Statement((Statement::Error, e.span())),
                e.span(),
            )
        })))
        .repeated()
        .collect()
        .then_ignore(end())
        .boxed()
}
