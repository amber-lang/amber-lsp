use chumsky::prelude::*;

use crate::{
    grammar::alpha034::{
        lexer::Token, statements::failed::failure_parser, AmberParser, Expression,
        InterpolatedCommand, Spanned, Statement,
    },
    T,
};

pub fn command_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    let escape = just(T!['\\'])
        .ignore_then(any())
        .map(|token: Token| InterpolatedCommand::Escape(token.to_string()))
        .boxed();

    let command_option = just(T!["-"])
        .then(just(T!["-"]).or_not())
        .then(any().or_not())
        .map(|((_, second_dash), option)| {
            let dashes = if second_dash.is_some() { "--" } else { "-" };

            InterpolatedCommand::CommandOption(format!(
                "{}{}",
                dashes,
                option.unwrap_or(T![""]).to_string()
            ))
        })
        .boxed();

    let interpolated = expr
        .recover_with(via_parser(
            any()
                .or_not()
                .map_with(|_, e| (Expression::Error, e.span())),
        ))
        .delimited_by(
            just(T!['{']),
            just(T!['}']).recover_with(via_parser(
                none_of(T!["}"])
                    .repeated()
                    .then(just(T!['}']))
                    .or_not()
                    .map(|_| T!['}']),
            )),
        )
        .map(|expr| InterpolatedCommand::Expression(Box::new(expr)))
        .boxed();

    just(T!['$'])
        .ignore_then(
            choice((
                any()
                    .filter(|c: &Token| {
                        *c != T!["$"] && *c != T!["{"] && *c != T!["\\"] && *c != T!["-"]
                    })
                    .map(|text| InterpolatedCommand::Text(text.to_string())),
                escape,
                command_option,
                interpolated,
            ))
            .map_with(|cmd, e| (cmd, e.span()))
            .repeated()
            .collect(),
        )
        .then_ignore(just(T!['$']).recover_with(via_parser(any().or_not().map(|_| T!['$']))))
        .then(failure_parser(stmnts).or_not())
        .map_with(|(expr, handler), e| (Expression::Command(expr, handler), e.span()))
        .boxed()
}
