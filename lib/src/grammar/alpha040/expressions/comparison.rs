use chumsky::prelude::*;

use crate::{
    grammar::alpha040::{
        lexer::Token, parser::default_recovery, AmberParser, Expression, Spanned, Statement,
    },
    T,
};

use super::sum::sum_parser;

pub fn comparison_parser<'a>(
    stmnts: impl AmberParser<'a, Spanned<Statement>>,
    expr: impl AmberParser<'a, Spanned<Expression>>,
) -> impl AmberParser<'a, Spanned<Expression>> {
    sum_parser(stmnts.clone(), expr.clone())
        .foldl(
            choice((
                just(T![">="]).to(Expression::Ge as fn(_, _) -> _),
                just(T![">"]).to(Expression::Gt as fn(_, _) -> _),
                just(T!["<="]).to(Expression::Le as fn(_, _) -> _),
                just(T!["<"]).to(Expression::Lt as fn(_, _) -> _),
                just(T!["=="]).to(Expression::Eq as fn(_, _) -> _),
                just(T!["!="]).to(Expression::Neq as fn(_, _) -> _),
            ))
            .then(
                sum_parser(stmnts.clone(), expr.clone()).recover_with(via_parser(
                    default_recovery()
                        .or_not()
                        .map_with(|_, e| (Expression::Error, e.span())),
                )),
            )
            .repeated(),
            |lhs, (op, rhs)| {
                let span = SimpleSpan::new(lhs.1.start, rhs.1.end);

                (op(Box::new(lhs), Box::new(rhs)), span)
            },
        )
        .boxed()
        .labelled("expression")
}
