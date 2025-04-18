use chumsky::prelude::*;

use super::{expressions::parse_expr, AmberParser, Spanned, Statement};

pub mod block;
pub mod comment;
pub mod failed;
pub mod if_cond;
pub mod keywords;
pub mod loops;
pub mod modifiers;
pub mod shorthands;
pub mod var_init;
pub mod var_set;
pub mod shebang;
pub mod move_files;
pub mod doc_string;
pub mod const_init;

pub fn statement_parser<'a>() -> impl AmberParser<'a, Spanned<Statement>> {
    recursive(|stmnt| {
        choice((
            doc_string::doc_string_parser(),
            comment::comment_parser(),
            shebang::shebang_parser(),
            var_init::var_init_parser(stmnt.clone()),
            var_set::var_set_parser(stmnt.clone()),
            block::block_parser_statement(stmnt.clone()),
            if_cond::if_chain_parser(stmnt.clone()),
            if_cond::if_cond_parser(stmnt.clone()),
            shorthands::shorthand_parser(stmnt.clone()),
            loops::inf_loop_parser(stmnt.clone()),
            loops::iter_loop_parser(stmnt.clone()),
            keywords::keywords_parser(stmnt.clone()),
            move_files::move_files_parser(stmnt.clone()),
            const_init::const_init_parser(stmnt.clone()),
            parse_expr(stmnt).map_with(|expr, e| (Statement::Expression(Box::new(expr)), e.span())),
        ))
        .boxed()
        .labelled("statement")
    })
    .boxed()
}
