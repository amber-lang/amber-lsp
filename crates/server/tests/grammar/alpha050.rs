use std::fs::read_to_string;

use chumsky::error::Rich;
use insta::assert_debug_snapshot;

use amber_grammar::alpha050::{
    AmberCompiler,
    GlobalStatement,
    Token,
};
use amber_grammar::{
    LSPAnalysis,
    ParserResponse,
    Spanned,
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
        amber_grammar::Grammar::Alpha050(ast) => ast,
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
    let stdlib = read_to_string("../../resources/alpha050/std/array.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_date() {
    let stdlib = read_to_string("../../resources/alpha050/std/date.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_env() {
    let stdlib = read_to_string("../../resources/alpha050/std/env.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_fs() {
    let stdlib = read_to_string("../../resources/alpha050/std/fs.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_http() {
    let stdlib = read_to_string("../../resources/alpha050/std/http.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_math() {
    let stdlib = read_to_string("../../resources/alpha050/std/math.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_text() {
    let stdlib = read_to_string("../../resources/alpha050/std/text.ab").unwrap();

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

    $$ exited(code): echo code // should succeed
    $$ exited: echo "then" // should fail

    $$
    // succeeded
    succeeded: echo "success"
    // failed
    failed(code): echo "failed with code: {code}"


    $$?
"#;
    println!("{:?}", tokenize(input));
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

#[test]
fn test_lexer_double_dollar_edge_cases() {
    let compiler = AmberCompiler::new();

    assert_debug_snapshot!("double_dollar_whitespace", compiler.tokenize("$$ \n"));
    assert_debug_snapshot!("double_dollar_end", compiler.tokenize("$$"));
    assert_debug_snapshot!("double_dollar_no_space", compiler.tokenize("$$failed"));
    assert_debug_snapshot!("double_dollar_brace", compiler.tokenize("$$ {var}$"));
}

#[test]
fn test_lexer_string_escapes() {
    let compiler = AmberCompiler::new();

    assert_debug_snapshot!(
        "string_escapes",
        compiler.tokenize(r#""hello\nworld\t\"quote\"""#)
    );
    assert_debug_snapshot!("string_trailing_backslash", compiler.tokenize(r#""test\""#));
    assert_debug_snapshot!("empty_string", compiler.tokenize(r#""""#));
}

#[test]
fn test_lexer_command_escapes() {
    let compiler = AmberCompiler::new();

    assert_debug_snapshot!(
        "command_escapes",
        compiler.tokenize(r#"$echo\ with\ spaces$"#)
    );
    assert_debug_snapshot!("command_backslash", compiler.tokenize(r#"$test\n$"#));
}

#[test]
fn test_lexer_interpolation_edge_cases() {
    let compiler = AmberCompiler::new();

    assert_debug_snapshot!(
        "string_nested_braces",
        compiler.tokenize(r#""text {if true { "inner" } else { "other" }} end""#)
    );
    assert_debug_snapshot!(
        "string_multi_interpolation",
        compiler.tokenize(r#""{a} and {b} and {c}""#)
    );
    assert_debug_snapshot!(
        "command_nested_braces",
        compiler.tokenize(r#"$echo {if true { 1 } else { 0 }}$"#)
    );
}

#[test]
fn test_lexer_dollar_at_end() {
    let compiler = AmberCompiler::new();

    assert_debug_snapshot!("dollar_at_end", compiler.tokenize("text$"));
    assert_debug_snapshot!("just_dollar", compiler.tokenize("$"));
}

#[test]
fn test_lexer_dollar_followed_by_dollar() {
    let compiler = AmberCompiler::new();

    assert_debug_snapshot!("dollar_dollar", compiler.tokenize("$$"));
    assert_debug_snapshot!("triple_dollar", compiler.tokenize("$$$"));
}

#[test]
fn test_lexer_dollar_prev_is_dollar() {
    let compiler = AmberCompiler::new();

    assert_debug_snapshot!("dollar_dollar_space", compiler.tokenize("$$ "));
    assert_debug_snapshot!("dollar_dollar_char", compiler.tokenize("$$x"));
}

#[test]
fn test_lexer_context_transitions() {
    let compiler = AmberCompiler::new();

    assert_debug_snapshot!(
        "context_string_to_main",
        compiler.tokenize(r#""start {expr} end""#)
    );
    assert_debug_snapshot!("context_command_to_main", compiler.tokenize("$cmd {expr}$"));
    assert_debug_snapshot!(
        "context_multiple_switches",
        compiler.tokenize(r#""a {$b$} c""#)
    );
}

#[test]
fn test_lexer_error_recovery() {
    let compiler = AmberCompiler::new();

    assert_debug_snapshot!("unclosed_string", compiler.tokenize(r#""unclosed"#));
    assert_debug_snapshot!("unclosed_command", compiler.tokenize("$echo test"));
    assert_debug_snapshot!("mismatched_braces", compiler.tokenize(r#""{{{""#));
}

#[test]
fn test_lexer_edge_cases() {
    let compiler = AmberCompiler::new();

    assert_debug_snapshot!("empty_input", compiler.tokenize(""));
    assert_debug_snapshot!("single_char", compiler.tokenize("x"));
    assert_debug_snapshot!("single_dollar_cmd", compiler.tokenize("$echo$"));
}

#[test]
fn test_modifiers() {
    let input = r#"
    sudo $$
    trust $$
    silent $$
    unsafe $$
"#;

    assert_debug_snapshot!(parse(&tokenize(input)));
}
