use std::fs::read_to_string;

use chumsky::error::Rich;
use insta::assert_debug_snapshot;

use amber_lsp::grammar::{
    alpha040::{lexer::Token, AmberCompiler, GlobalStatement, Spanned},
    LSPAnalysis, ParserResponse,
};

fn tokenize(input: &str) -> Vec<Spanned<Token>> {
    AmberCompiler::new().tokenize(input)
}

fn parse<'a>(
    tokens: &'a Vec<Spanned<Token>>,
) -> (Option<Vec<Spanned<GlobalStatement>>>, Vec<Rich<'a, String>>) {
    let ParserResponse {
        ast,
        errors,
        semantic_tokens: _,
    } = AmberCompiler::new().parse(tokens);

    let ast = match ast {
        amber_lsp::grammar::Grammar::Alpha040(ast) => ast,
        _ => panic!("Unexpected AST"),
    };

    (ast, errors)
}

fn parse_unwrap<'a>(tokens: &'a Vec<Spanned<Token>>) -> Vec<Spanned<GlobalStatement>> {
    let (ast, errors) = parse(tokens);
    if !errors.is_empty() {
        panic!("Errors: {:?}", errors);
    }
    ast.unwrap()
}

#[test]
fn test_stdlib_array() {
    let stdlib = read_to_string("resources/alpha040/std/array.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_date() {
    let stdlib = read_to_string("resources/alpha040/std/date.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_env() {
    let stdlib = read_to_string("resources/alpha040/std/env.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_fs() {
    let stdlib = read_to_string("resources/alpha040/std/fs.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_http() {
    let stdlib = read_to_string("resources/alpha040/std/http.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_math() {
    let stdlib = read_to_string("resources/alpha040/std/math.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_text() {
    let stdlib = read_to_string("resources/alpha040/std/text.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}
