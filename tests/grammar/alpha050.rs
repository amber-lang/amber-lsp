use std::fs::read_to_string;

use chumsky::error::Rich;
use insta::assert_debug_snapshot;

use amber_lsp::grammar::alpha050::{
    AmberCompiler,
    GlobalStatement,
    Spanned,
    Token,
};
use amber_lsp::grammar::{
    LSPAnalysis,
    ParserResponse,
};

fn tokenize(input: &str) -> Vec<Spanned<Token>> {
    AmberCompiler::new().tokenize(input)
}

fn parse(
    tokens: &[Spanned<Token>],
) -> (Option<Vec<Spanned<GlobalStatement>>>, Vec<Rich<'_, String>>) {
    let ParserResponse {
        ast,
        errors,
        semantic_tokens: _,
    } = AmberCompiler::new().parse(tokens);

    let ast = match ast {
        amber_lsp::grammar::Grammar::Alpha050(ast) => ast,
        _ => panic!("Unexpected AST"),
    };

    (ast, errors)
}

fn parse_unwrap(tokens: &[Spanned<Token>]) -> Vec<Spanned<GlobalStatement>> {
    let (ast, errors) = parse(tokens);
    if !errors.is_empty() {
        panic!("Errors: {errors:?}");
    }
    ast.unwrap()
}

#[test]
fn test_numbers() {
    let input = r#"
    2
    2.4
    .2
    "#;

    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_stdlib_array() {
    let stdlib = read_to_string("resources/alpha050/std/array.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_date() {
    let stdlib = read_to_string("resources/alpha050/std/date.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_env() {
    let stdlib = read_to_string("resources/alpha050/std/env.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_fs() {
    let stdlib = read_to_string("resources/alpha050/std/fs.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_http() {
    let stdlib = read_to_string("resources/alpha050/std/http.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_math() {
    let stdlib = read_to_string("resources/alpha050/std/math.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_text() {
    let stdlib = read_to_string("resources/alpha050/std/text.ab").unwrap();

    let tokens = tokenize(&stdlib);

    assert_debug_snapshot!(tokens);

    assert_debug_snapshot!(parse_unwrap(&tokens));
}

#[test]
fn test_unfinished_function_call() {
    let input = "
    import { array_contains } from \"std/array\"

    let x = [1, 2, 3]
    let y = 2

    echo array_contains(x, y)

    let line = 213

    for idx, line in lines(\"text.txt\") {
      echo line
    }

    // fun foo(x: Num, y: Text) {

    // }

    fun foo(x: Num, y: Text) {
    }

    // fun abc() {}

    array_contains([1, 2, 3],)
    ";

    let tokens = tokenize(input);
    assert_debug_snapshot!(tokens);

    let result = parse(&tokens);

    assert_debug_snapshot!(result);
}

#[test]
fn test_comments_in_ifs() {
    let input = r#"
    if {
        1 == 2: echo "x" // test comment
        // another comment
        2 == 2 {
            echo "y"
        }
        // another
        else: echo "z" // comment
        // super comment
        /// doc comment
    }

    if age >= 16: echo "Welcome" // comment
    // comment in between
    else: echo "Entry not allowed" // another comment
"#;

    assert_debug_snapshot!(parse(&tokenize(input)));
}

#[test]
fn test_block_singleline() {
    let input = r#"
    main {
    echo 4
    }

    main(args): echo args

    $$ failed {
        echo "failed"
    }
    $$ failed: echo "failed"

"#;

    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_failable_handlers() {
    let input = r#"
    $$ succeeded(code): echo code // should fail
    $$ succeeded: echo "success" // should succeed

    $$ failed(code): echo code // should succeed
    $$ failed: echo "failed" // should succeed

    $$ then(code): echo code // should succeed
    $$ then: echo "then" // should fail

"#;

    assert_debug_snapshot!(parse(&tokenize(input)));
}

#[test]
fn test_mv_files() {
    let input = r#"
    mv "/tmp/a" "/tmp/b"

    unsafe mv "/tmp/a" "/tmp/b"

    mv "/tmp/a" "/tmp/b" failed {
        echo "Error"
    }

    unsafe {
        mv "/tmp/a" "/tmp/b"
    }
"#;

    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}
