use std::fs::read_to_string;

use chumsky::error::Rich;
use insta::assert_debug_snapshot;

use amber_grammar::alpha060::{
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
        amber_grammar::Grammar::Alpha060(ast) => ast,
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
    let stdlib = read_to_string("../../resources/alpha060/std/array.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_date() {
    let stdlib = read_to_string("../../resources/alpha060/std/date.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_env() {
    let stdlib = read_to_string("../../resources/alpha060/std/env.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_fs() {
    let stdlib = read_to_string("../../resources/alpha060/std/fs.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_http() {
    let stdlib = read_to_string("../../resources/alpha060/std/http.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_math() {
    let stdlib = read_to_string("../../resources/alpha060/std/math.ab").unwrap();

    assert_debug_snapshot!(parse_unwrap(&tokenize(&stdlib)));
}

#[test]
fn test_stdlib_text() {
    let stdlib = read_to_string("../../resources/alpha060/std/text.ab").unwrap();

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

    echo(array_contains)(x, y)

    let line = 213

    for idx, line in lines(\"text.txt\") {
      echo(line)
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
        1 == 2: echo("x") // test comment
        // another comment
        2 == 2 {
            echo("y")
        }
        // another
        else: echo("z") // comment
        // super comment
        /// doc comment
    }

    if age >= 16: echo("Welcome") // comment
    // comment in between
    else: echo("Entry not allowed") // another comment
"#;

    assert_debug_snapshot!(parse(&tokenize(input)));
}

#[test]
fn test_block_singleline() {
    let input = r#"
    main {
    echo(4)
    }

    main(args): echo(args)

    $$ failed {
        echo("failed")
    }
    $$ failed: echo("failed")

"#;

    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_failable_handlers() {
    let input = r#"
    $$ succeeded(code): echo(code) // should fail
    $$ succeeded: echo("success") // should succeed

    $$ failed(code): echo(code) // should succeed
    $$ failed: echo("failed") // should succeed

    $$ exited(code): echo(code) // should succeed
    $$ exited: echo("then") // should fail

    $$
    // succeeded
    succeeded: echo("success")
    // failed
    failed(code): echo("failed) with code: {code}"


    $$?
"#;
    println!("{:?}", tokenize(input));
    assert_debug_snapshot!(parse(&tokenize(input)));
}

#[test]
fn test_mv_files() {
    let input = r#"
    mv("/tmp/a", "/tmp/b")

    unsafe mv("/tmp/a", "/tmp/b")

    mv ("/tmp/a", "/tmp/b") failed {
        echo("Error")
    }

    unsafe {
        mv ("/tmp/a", "/tmp/b")
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
    assert_debug_snapshot!(
        "command_new_line",
        parse(&compiler.tokenize(
            r#"
    $ MY_VAR=1 \
     my_command $
    "#
        ))
    );
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
    assert_debug_snapshot!("unclosed_command", compiler.tokenize("$echo(test)"));
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

#[test]
fn test_array_index_set() {
    let input = r#"
    let arr = [1, 2, 3]
    arr[0] = 10
"#;

    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_array_index_set_expression_index() {
    let input = r#"
    let arr = [1, 2, 3]
    let i = 0
    arr[i + 1] = 42
"#;

    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_array_destruct_init() {
    let input = r#"
    let [a, b] = [1, 2]
"#;

    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_array_destruct_init_three_vars() {
    let input = r#"
    let [x, y, z] = [1, 2, 3]
"#;

    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_array_destruct_set_simple() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("[a, b] = [1, 2]")));
}

#[test]
fn test_array_destruct_set() {
    let input = r#"
    let a = 0;
    let b = 0;
    [a, b] = [1, 2]
"#;

    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_array_destruct_init_and_set() {
    let input = r#"
    let array_to_destruct = [1, 2, 3]

    let [a, b] = array_to_destruct

    [a, b] = array_to_destruct
"#;

    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_array_destruct_init_with_comments_and_trailing() {
    let input = r#"
    let array_to_destruct = [1, 2, 3]

    let [
        // comment
        // 2nd comment
        a,
        // 3rd comment
        // 4th comment
        b,
        // 5th comment
        // 6th comment
    ] = array_to_destruct

    [
        // comment
        // 2nd comment
        a,
        // 3rd comment
        // 4th comment
        b,
        // 5th comment
        // 6th comment
    ] = array_to_destruct
"#;

    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

// ==================== Union Type Tests ====================

#[test]
fn test_union_type_simple() {
    let input = r#"
    fun foo(x: Int | Text) {
    }
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_three_types() {
    let input = r#"
    fun foo(x: Int | Text | Bool) {
    }
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_with_null() {
    let input = r#"
    fun foo(x: Int | Null) {
    }
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_with_failable() {
    let input = r#"
    fun foo(x: Int? | Text) {
    }
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_failable_at_end() {
    let input = r#"
    fun foo(x: Int | Text?) {
    }
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_with_array() {
    let input = r#"
    fun foo(x: [Int] | Text) {
    }
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_array_with_union_inside() {
    let input = r#"
    fun foo(x: [Int | Text]) {
    }
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_complex() {
    let input = r#"
    fun foo(x: Int | [Num? | Int] | Text | Null?) {
    }
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_return_type() {
    let input = r#"
    fun foo(): Int | Text {
    }
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_return_type_with_failable() {
    let input = r#"
    fun foo(): Int | Text? {
    }
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_multiple_args() {
    let input = r#"
    fun foo(a: Int | Text, b: Bool | Null) {
    }
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_cast() {
    let input = r#"
    let x = 42 as Int | Text
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_is_check() {
    let input = r#"
    let x = 42
    let y = x is Int | Text
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_array_failable() {
    let input = r#"
    fun foo(x: [Int]? | Text) {
    }
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_all_primitives() {
    let input = r#"
    fun foo(x: Int | Num | Text | Bool | Null) {
    }
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_nested_array_with_failable() {
    let input = r#"
    fun foo(x: [Num? | Int]) {
    }
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_empty_array_in_union() {
    let input = r#"
    fun foo(x: [] | Text) {
    }
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_optional_arg_with_union() {
    let input = r#"
    fun foo(x: Int | Text = 42) {
    }
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_union_type_lexer_pipe_token() {
    let input = "Int | Text";
    let tokens = tokenize(input);
    assert_eq!(tokens[1].0 .0, "|");
}

#[test]
fn test_pub_const() {
    let input = r#"
    pub const MAX_SIZE = 100
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_pub_const_with_expression() {
    let input = r#"
    pub const GREETING = "Hello, World!"
    pub const LIMIT = 10 + 5
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_pub_let_with_flag() {
    let input = r#"
    #[allow_public_mutable]
    pub let counter = 0
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_pub_let_without_flag() {
    let input = r#"
    pub let counter = 0
"#;
    // Should parse successfully (error reported during analysis, not grammar)
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}

#[test]
fn test_pub_const_and_pub_fun() {
    let input = r#"
    pub const VERSION = "1.0"

    pub fun get_version() {
        return VERSION
    }
"#;
    assert_debug_snapshot!(parse_unwrap(&tokenize(input)));
}
