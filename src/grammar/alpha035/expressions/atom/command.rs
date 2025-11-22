use chumsky::prelude::*;

use crate::grammar::alpha035::parser::default_recovery;
use crate::grammar::alpha035::statements::failed::failure_parser;
use crate::grammar::alpha035::statements::modifiers::modifier_parser;
use crate::grammar::alpha035::{
    AmberParser,
    Expression,
    InterpolatedCommand,
    Spanned,
    Statement,
};
use crate::grammar::Token;
use crate::T;

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
        .then(none_of([T!["{"], T!["$"], T!["\\"], T!["-"]]).or_not())
        .map(|((_, second_dash), option)| {
            let dashes = if second_dash.is_some() { "--" } else { "-" };

            InterpolatedCommand::CommandOption(format!("{}{}", dashes, option.unwrap_or(T![""])))
        })
        .boxed();

    let interpolated = expr
        .recover_with(via_parser(
            default_recovery()
                .or_not()
                .map_with(|_, e| (Expression::Error, e.span())),
        ))
        .delimited_by(
            just(T!['{']),
            just(T!['}']).recover_with(via_parser(
                default_recovery()
                    .repeated()
                    .then(just(T!['}']))
                    .or_not()
                    .map(|_| T!['}']),
            )),
        )
        .map(|expr| InterpolatedCommand::Expression(Box::new(expr)))
        .boxed();

    modifier_parser()
        .repeated()
        .collect()
        .then(just(T!['$']).map_with(|_, e| (InterpolatedCommand::Text("$".to_string()), e.span())))
        .then(
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
            .collect::<Vec<Spanned<InterpolatedCommand>>>(),
        )
        .then(
            just(T!['$'])
                .recover_with(via_parser(default_recovery().or_not().map(|_| T!['$'])))
                .map_with(|_, e| (InterpolatedCommand::Text("$".to_string()), e.span())),
        )
        .then(failure_parser(stmnts).or_not())
        .map_with(|((((modifier, begin), content), end), handler), e| {
            let mut content_with_bounds = vec![begin];
            content_with_bounds.extend(content);
            content_with_bounds.push(end);

            (
                Expression::Command(modifier, content_with_bounds, handler),
                e.span(),
            )
        })
        .boxed()
        .labelled("command")
}
