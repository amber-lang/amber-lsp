use chumsky::prelude::*;

use crate::T;

use super::expressions::parse_expr;
use super::parser::{
    default_recovery,
    ident,
};
use super::statements::statement_parser;
use super::{
    AmberParser,
    CompilerFlag,
    DataType,
    Expression,
    FunctionArgument,
    GlobalStatement,
    ImportContent,
    Spanned,
    Statement,
};
use crate::grammar::Token;

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
                .allow_trailing()
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
        .labelled("import statement")
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
            .ignore_then(literal_type.or_not())
            .then_ignore(just(T!["]"]))
            .map(|ty| DataType::Array(Box::new(ty.unwrap_or(DataType::Any)))))
        .then(
            just(T!["?"])
                .or_not()
                .map(|is_optional| is_optional.is_some()),
        )
        .map_with(|(ty, is_optional), e| {
            (
                match is_optional {
                    true => DataType::Failable(Box::new(ty)),
                    _ => ty,
                },
                e.span(),
            )
        })
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
        .then(
            just(T!['='])
                .ignore_then(
                    parse_expr(statement_parser()).recover_with(via_parser(
                        default_recovery()
                            .or_not()
                            .map_with(|_, e| (Expression::Error, e.span())),
                    )),
                )
                .or_not(),
        )
        .map_with(|(arg, default), e| {
            (
                match (arg.clone(), default) {
                    (FunctionArgument::Typed(is_ref, name, ty), Some(expr)) => {
                        FunctionArgument::Optional(is_ref, name, Some(ty), expr)
                    }
                    (FunctionArgument::Generic(is_ref, name), Some(expr)) => {
                        FunctionArgument::Optional(is_ref, name, None, expr)
                    }
                    _ => arg,
                },
                e.span(),
            )
        })
        .labelled("argument")
        .boxed();

    let args_parser = arg_parser
        .recover_with(via_parser(
            default_recovery().map_with(|_, e| (FunctionArgument::Error, e.span())),
        ))
        .separated_by(
            just(T![","]).recover_with(via_parser(default_recovery().rewind().map(|_| T![","]))),
        )
        .allow_trailing()
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
        .then(ret_parser.or_not())
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
                )
                .recover_with(via_parser(
                    default_recovery()
                        .repeated()
                        .then(just(T!["}"]))
                        .or_not()
                        .map(|_| vec![]),
                )),
        )
        .map_with(
            |((((((compiler_flags, is_pub), fun), name), args), ty), body), e| {
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
        .boxed()
        .labelled("function definition")
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
        .labelled("main body")
}

pub fn global_statement_parser<'a>() -> impl AmberParser<'a, Vec<Spanned<GlobalStatement>>> {
    let statement = statement_parser()
        .map(|stmnt| (GlobalStatement::Statement(Box::new(stmnt.clone())), stmnt.1))
        .boxed();

    choice((import_parser(), function_parser(), main_parser(), statement))
        .recover_with(via_parser(any().map_with(|_, e| {
            (
                GlobalStatement::Statement(Box::new((Statement::Error, e.span()))),
                e.span(),
            )
        })))
        .repeated()
        .collect()
        .then_ignore(end())
        .boxed()
}
