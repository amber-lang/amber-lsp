use chumsky::prelude::*;

use crate::T;

use super::{
    lexer::Token,
    parser::{default_recovery, ident},
    statements::statement_parser,
    AmberParser, CompilerFlag, DataType, FunctionArgument, GlobalStatement, ImportContent, Spanned,
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
        .ignore_then(
            any()
                .filter(|c| *c != T!['"'])
                .repeated()
                .collect::<Vec<Token>>()
                .map_with(|name, e| {
                    (
                        name.iter().map(|t| t.to_string()).collect::<String>(),
                        SimpleSpan::new(
                            (e.span() as SimpleSpan).start - 1,
                            (e.span() as SimpleSpan).end + 1,
                        ),
                    )
                }),
        )
        .then_ignore(
            just(T!['"']).recover_with(via_parser(
                default_recovery()
                    .repeated()
                    .then(just(T!['"']))
                    .or_not()
                    .map(|_| T!['"']),
            )),
        )
        .boxed();

    just(T!["pub"])
        .or_not()
        .map_with(|is_pub, e| (is_pub.is_some(), e.span()))
        .then(just(T!["import"]).map_with(|_, e| ("import".to_string(), e.span())))
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
    just(T!["#"])
        .ignore_then(just(T!["["]))
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
    let generic_arg_parser = ident("argument".to_string())
        .map_with(|name, e| (FunctionArgument::Generic((name, e.span())), e.span()))
        .boxed();

    let typed_arg_parser = ident("argument".to_string())
        .map_with(|name, e| (name, e.span()))
        .then(
            just(T![":"]).ignore_then(
                type_parser().recover_with(via_parser(
                    default_recovery()
                        .or_not()
                        .map_with(|_, e| (DataType::Error, e.span())),
                )),
            ),
        )
        .map_with(|(name, ty), e| (FunctionArgument::Typed(name, ty), e.span()))
        .boxed();

    let arg_parser = choice((typed_arg_parser, generic_arg_parser))
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
                .map_err(|err| Rich::custom(err.span().clone(), "Expected function name"))
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
        .map_with(|(main, body), e| (GlobalStatement::Main(main, body), e.span()))
        .boxed()
}

pub fn global_statement_parser<'a>() -> impl AmberParser<'a, Vec<Spanned<GlobalStatement>>> {
    let statement = statement_parser()
        .map(|stmnt| (GlobalStatement::Statement(stmnt.clone()), stmnt.1))
        .boxed();

    choice((import_parser(), function_parser(), main_parser(), statement))
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
        .then_ignore(just(T![';']).or_not())
        .then_ignore(end())
        .boxed()
}
