use std::fs::read_to_string;

use chumsky::error::Rich;
use insta::assert_debug_snapshot;

use amber_lsp::grammar::alpha034::semantic_tokens::semantic_tokens_from_ast;
use amber_lsp::grammar::alpha034::{
    AmberCompiler,
    Spanned,
};
use amber_lsp::grammar::{
    LSPAnalysis,
    ParserResponse,
    Token,
};

fn tokenize(input: &str) -> Vec<Spanned<Token>> {
    AmberCompiler::new().tokenize(input)
}

fn parse(
    tokens: &[Spanned<Token>],
) -> (
    Option<Vec<Spanned<amber_lsp::grammar::alpha034::GlobalStatement>>>,
    Vec<Rich<'_, String>>,
) {
    let ParserResponse {
        ast,
        errors,
        semantic_tokens: _,
    } = AmberCompiler::new().parse(tokens);

    let ast = match ast {
        amber_lsp::grammar::Grammar::Alpha034(ast) => ast,
        _ => panic!("Unexpected AST"),
    };

    (ast, errors)
}

fn parse_unwrap(
    tokens: &[Spanned<Token>],
) -> Vec<Spanned<amber_lsp::grammar::alpha034::GlobalStatement>> {
    parse(tokens).0.unwrap()
}

#[test]
fn test_text() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("\"Hello, world!\"")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("\"Hello, {name}!\"")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("\"Hello, {name}! How are you?\"")));
    assert_debug_snapshot!(parse_unwrap(&tokenize(r#""\"text in quotes\" \\""#)));
    assert_debug_snapshot!(parse(&tokenize(r#""{unclosed""#)));
    assert_debug_snapshot!(parse(&tokenize(r#""{""#)));
    assert_debug_snapshot!(parse(&tokenize(r#"""#)));
    assert_debug_snapshot!(parse(&tokenize(r#""\""#)));
    assert_debug_snapshot!(parse(&tokenize(r#""\"#)));
}

#[test]
fn test_variable_get() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("name")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("name1")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("name_1")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("name_1_")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("_var")));
}

#[test]
fn test_number() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1.0")));
    assert_debug_snapshot!(parse_unwrap(&tokenize(".23")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("-1.0")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("-1.24")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("-5")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("001.00004")));
}

#[test]
fn test_whitespace() {
    assert_debug_snapshot!(parse_unwrap(&tokenize(" 0")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("  0")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("  0 ")));
}

#[test]
fn test_bool() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("true")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("false")));
}

#[test]
fn test_add() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2 + 3")));
    assert_debug_snapshot!(parse(&tokenize("1 +")));
    assert_debug_snapshot!(parse(&tokenize(
        "
        1 +
        let x = 10
    "
    )));
}

#[test]
fn test_subtract() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 - 2")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 - 2 - 3")));
}

#[test]
fn test_add_and_subtract() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2 - 3")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 - 2 + 3")));
}

#[test]
fn test_multiply() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 * 2")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 * 2 * 3")));
    assert_debug_snapshot!(parse(&tokenize("1 * 2 *")));
}

#[test]
fn test_divide() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 / 2")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 / 2 / 3")));
}

#[test]
fn test_multiply_and_divide() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 * 2 / 3")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 / 2 * 3")));
}

#[test]
fn test_mults_and_adds() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2 * 3")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 * 2 + 3")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 / 2 + 3")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 - 2 / 3")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("3 * 2 - --2 * 2")));
}

#[test]
fn test_modulo() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 % 2")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 % 2 % 3")));
    assert_debug_snapshot!(parse(&tokenize("1 % 2 %")));
}

#[test]
fn test_neg() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("-(1)")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("-(1 - 2)")));
}

#[test]
fn test_and() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("true and false")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("true and false and true")));
    assert_debug_snapshot!(parse(&tokenize("true and false and")));
}

#[test]
fn test_or() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("false or false")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("false or false or true")));
}

#[test]
fn test_gt() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 > 2")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2 > 2 + 1")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2 > 2 + 1 > 5")));
    assert_debug_snapshot!(parse(&tokenize("1 + 2 > ")));
}

#[test]
fn test_ge() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 >= 2")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2 >= 2 + 1")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2 >= 2 + 1 >= 5")));
}

#[test]
fn test_lt() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 < 2")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2 < 2 + 1")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2 < 2 + 1 < 5")));
}

#[test]
fn test_le() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 <= 2")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2 <= 2 + 1")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2 <= 2 + 1 <= 5")));
}

#[test]
fn test_eq() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 == 2")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2 == 2 + 1")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2 == 2 + 1 + 5")));
}

#[test]
fn test_neq() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 != 2")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2 != 2 + 1")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2 != 2 + 1 + 5")));
}

#[test]
fn test_not() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("not true")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("not not true")));
}

#[test]
fn test_ternary() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("   true then 1 else 2   ")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("true then 1 + 2 else 2 + 1")));
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "true then 1 + 2 else false then 5 else 6"
    )));
    assert_debug_snapshot!(parse(&tokenize("true then")));
    assert_debug_snapshot!(parse(&tokenize("true then 1")));
    assert_debug_snapshot!(parse(&tokenize("true then 1 else")));
}

#[test]
fn test_command() {
    assert_debug_snapshot!(parse_unwrap(&tokenize(r#"$echo \"Hello, world!\"$"#)));
    assert_debug_snapshot!(parse_unwrap(&tokenize("$echo \"Hello, {name}!\"$")));
    assert_debug_snapshot!(parse(&tokenize("$command --arg1 -v$")));
    assert_debug_snapshot!(parse(&tokenize("$command -$")));
    assert_debug_snapshot!(parse(&tokenize("$command --arg1 -v")));
    assert_debug_snapshot!(parse(&tokenize("$command {unclosed")));
    assert_debug_snapshot!(parse(&tokenize(
        "$command {unclosed interpolation$ let x = 10"
    )));
    assert_debug_snapshot!(parse(&tokenize("$command {")));
    assert_debug_snapshot!(parse(&tokenize("$command {}$")));
    // TODO: Issue with Heraclitus lexer. Uncomment when fixed
    // assert_debug_snapshot!(parse(r#"$echo "\$\{source//{pattern}/{replacement}}"$"#));
}

#[test]
fn test_array() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("[1, 2, 3]")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("[1]")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("[]")));
    assert_debug_snapshot!(parse(&tokenize("[")));
    assert_debug_snapshot!(parse(&tokenize("[1")));
    assert_debug_snapshot!(parse(&tokenize("[,")));
    assert_debug_snapshot!(parse(&tokenize("[1,")));
    assert_debug_snapshot!(parse(&tokenize("[1, 2")));
    assert_debug_snapshot!(parse(&tokenize("[1, 2 3")));
    assert_debug_snapshot!(parse(&tokenize("[1, 2 3 let")));
    assert_debug_snapshot!(parse(&tokenize("[1, 2 3] 4")));
}

#[test]
fn test_parentheses() {
    assert_debug_snapshot!(parse(&tokenize("()")));
    assert_debug_snapshot!(parse(&tokenize("(")));
    assert_debug_snapshot!(parse(&tokenize("(1")));
    assert_debug_snapshot!(parse(&tokenize("(1,)")));
}

#[test]
fn test_null() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("null")));
}

#[test]
fn test_range() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1..2")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1..=2")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1..2..3")));
    assert_debug_snapshot!(parse(&tokenize("1..")));
    assert_debug_snapshot!(parse(&tokenize("1..=")));
}

#[test]
fn test_function_invocation() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("func()")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("func(1)")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("func(1, 2)")));
    assert_debug_snapshot!(parse(&tokenize("func(")));
    assert_debug_snapshot!(parse(&tokenize("func(1")));
    assert_debug_snapshot!(parse(&tokenize("func(,)")));
    assert_debug_snapshot!(parse(&tokenize("func(1 2")));
    assert_debug_snapshot!(parse(&tokenize("func(1 2 let")));
    assert_debug_snapshot!(parse(&tokenize("func(1 2) 3")));
}

#[test]
fn test_cast() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 as Num")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 as Num as Text")));
    assert_debug_snapshot!(parse(&tokenize("1 as ")));
}

#[test]
fn test_nameof() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("nameof variable")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("nameof nameof variable")));
}

#[test]
fn test_expr_precedence() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2 * 3")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1 + 2 / 4 / 6")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("2 - 3 - 4")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("2 - (3 - 4)")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("-(2 + 3) * 5")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("(8+2)*(7-3)/2")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("2 / 3 + 1")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("25 / 5 * 3 + 7 - 2 * 4")));
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "
            2 + 5 > 3 + 4
                then 15 + 10
                else 5 - 4 <= 1/2
                    then 3 * 4
                    else 2"
    )));
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "true or false and true and true or false"
    )));
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "true as Bool as Text as Num * 2 / foo()"
    )));
}

#[test]
fn test_comment() {
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "
        // This is a comment
        1 + 2
    "
    )));
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "1 + 2 // This is a comment without a newline"
    )));
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "
        main {
            // abc
        }
        "
    )));
}

#[test]
fn test_import() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("import * from \"path/to/module\"")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("import {} from \"path/to/module\"")));
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "import { var1 } from \"path/to/module\""
    )));
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "import { var1, var2 } from \"path/to/module\""
    )));
    assert_debug_snapshot!(parse(&tokenize("import { var1 var2 from \"unclosed")));
    assert_debug_snapshot!(parse(&tokenize("import { var1 var2 \"unclosed")));
    assert_debug_snapshot!(parse(&tokenize("import  \"unclosed")));
    assert_debug_snapshot!(parse(&tokenize("import")));
    assert_debug_snapshot!(parse(&tokenize("import {")));
    assert_debug_snapshot!(parse(&tokenize("import { var1")));
    assert_debug_snapshot!(parse(&tokenize("import { var1 \"path\"")));
}

#[test]
fn test_function_def() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("fun func() {}")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("fun func(a) {}")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("fun func(a : Num) {}")));
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "fun func(a: Num, b, c: Bool): Num {}"
    )));
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "fun func(a: Num, b, c: Bool): [Num] {}"
    )));
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "
        fun func(a: Num, b: Text, c: Bool): Num {
            echo 10

            return 10
        }
    "
    )));
    assert_debug_snapshot!(parse(&tokenize("fun")));
    assert_debug_snapshot!(parse(&tokenize(
        "fun foo {
        echo 10
    }"
    )));
    assert_debug_snapshot!(parse(&tokenize(
        "fun foo(abc! {
        echo 10
    }"
    )));
    assert_debug_snapshot!(parse(&tokenize(
        "fun foo(abc:  {
        echo 10
    }"
    )));
    assert_debug_snapshot!(parse(&tokenize(
        "fun foo(abc: !WrongType {
        echo 10
    }"
    )));
    assert_debug_snapshot!(parse_unwrap(&tokenize("pub fun func() {}")));
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        r#"
    #[allow_absurd_cast]
    pub fun func() {}
    "#
    )));
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        r#"
    #[allow_absurd_cast]
    #[allow_generic_return]
    pub fun func() {}
    "#
    )));
    assert_debug_snapshot!(parse(&tokenize(
        r#"
    #[
    pub fun func() {}
    "#
    )));
    assert_debug_snapshot!(parse(&tokenize(
        r#"
    #[invalid]
    pub fun func() {}
    "#
    )));
}

#[test]
fn test_main_block() {
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "
        main {
            echo 10
        }

        main (args) {
            echo args;
        }
    "
    )));

    assert_debug_snapshot!(parse(&tokenize("main")));
}

#[test]
fn test_var_init() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("let a = 10")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("let a = 10 + 2")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("let a = 10 + 2 * 3")));
    assert_debug_snapshot!(parse(&tokenize("let a = [Text]")));
}

#[test]
fn test_var_set() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("a = 10")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("a = 10 + 2")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("a = 10 + 2 * 3")));
}

#[test]
fn test_if_condition() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("if true {}")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("if true { echo 10 }")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("if true { echo 10 } else {}")));
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "if true { echo 10 } else { echo 20 }"
    )));
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "
        if true: echo 10
        else: echo 20
    "
    )));
    assert_debug_snapshot!(semantic_tokens_from_ast(
        parse(&tokenize(
            r#"
fun bar(a: Text) {
    if true {
    }
}
    "#
        ))
        .0
        .as_ref()
    ));
}

#[test]
fn test_if_chain() {
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "
        if {
            2 == 3 {
                echo 10
            }
            true: echo 10
            else {
                echo 20
            }
        }
    "
    )));
}

#[test]
fn test_semicolon() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("1;")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("1; 2;")));
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "
        main {
            echo 10;
            echo 20

            echo 30;

            10 20 30
        }
    "
    )));
}

#[test]
fn test_shorthands() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("a += 10")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("a -= 10")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("a *= 10")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("a /= 10")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("a %= 10")));
}

#[test]
fn test_loops() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("loop {}")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("loop var1 in 1..2 {}")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("loop var1, var2 in 1..2 {}")));
}

#[test]
fn test_keywords() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("break")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("continue")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("fail")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("fail 1")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("echo 1")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("return")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("return 1")));
}

#[test]
fn test_modifiers() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("silent unsafe {}")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("unsafe silent {}")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("unsafe silent $command$")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("unsafe silent foo()")));
}

#[test]
fn test_failure_handlers() {
    assert_debug_snapshot!(parse_unwrap(&tokenize("$$?")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("$$ failed {}")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("foo()?")));
    assert_debug_snapshot!(parse_unwrap(&tokenize("foo() failed {}")));
}

#[test]
fn test_blocks() {
    assert_debug_snapshot!(parse_unwrap(&tokenize(
        "
        {
            echo 10
        }
        {
            {
                echo 20
            }
        }
    "
    )));
}

#[test]
fn test_recovery() {
    assert_debug_snapshot!(parse(&tokenize("fun foo(abc!) {}")));
    assert_debug_snapshot!(parse(&tokenize(
        "
    5 + 5 +;
    echo 10"
    )));
    assert_debug_snapshot!(parse(&tokenize(
        r#"
        import {}

    "#
    )));
    assert_debug_snapshot!(parse(&tokenize(
        r#"
        fun foo(a) {

            return "echo \"{5 + 5}\"";
        }

        unsafe {
    "#
    )));

    assert_debug_snapshot!(parse(&tokenize(
        r#"
        // comments
        // comments

        import {} from "test.ab";

        fun test_cat_cmd(file: Text): CmdText {
            return `echo "NOT READY" > {file}`
        }

        fun foo(a) {
            return "echo \"{5 + 5}\"";
        }

        unsafe {
            let x = 5;

            echo x;

            if {
                2 == 2 {
                    echo 3
                }
                else: 5
            }
        }
    "#
    )));
}

#[test]
fn test_lexer() {
    let compiler = AmberCompiler::new();

    assert_debug_snapshot!(compiler.tokenize(
        r#"
        let x = "my \"interpolated\" string {name} end";

        $this --should be - tokenized \$$
        "unclosed string

        abcd {let x = 10
    "#
    ));
}

#[test]
fn test_lexer_double_dollar_edge_cases() {
    let compiler = AmberCompiler::new();

    // Test $$ followed by whitespace
    assert_debug_snapshot!("double_dollar_whitespace", compiler.tokenize("$$ \n"));

    // Test $$ at end of input
    assert_debug_snapshot!("double_dollar_end", compiler.tokenize("$$"));

    // Test $$ followed by identifier without space
    assert_debug_snapshot!("double_dollar_no_space", compiler.tokenize("$$failed"));

    // Test $$ followed by {
    assert_debug_snapshot!("double_dollar_brace", compiler.tokenize("$$ {var}$"));
}

#[test]
fn test_lexer_string_escapes() {
    let compiler = AmberCompiler::new();

    // Test string with various escape sequences
    assert_debug_snapshot!(
        "string_escapes",
        compiler.tokenize(r#""hello\nworld\t\"quote\"""#)
    );

    // Test string with backslash at end
    assert_debug_snapshot!("string_trailing_backslash", compiler.tokenize(r#""test\""#));

    // Test empty string
    assert_debug_snapshot!("empty_string", compiler.tokenize(r#""""#));
}

#[test]
fn test_lexer_command_escapes() {
    let compiler = AmberCompiler::new();

    // Test command with escaped spaces
    assert_debug_snapshot!(
        "command_escapes",
        compiler.tokenize(r#"$echo\ with\ spaces$"#)
    );

    // Test command with backslash
    assert_debug_snapshot!("command_backslash", compiler.tokenize(r#"$test\n$"#));
}

#[test]
fn test_lexer_command_context_whitespace() {
    let compiler = AmberCompiler::new();

    // Test $$ command with leading whitespace after $$
    assert_debug_snapshot!("double_dollar_cmd_space", compiler.tokenize("$$ cmd$"));
}

#[test]
fn test_lexer_dollar_at_end() {
    let compiler = AmberCompiler::new();

    // Test single $ at end of input (line 226 - None case)
    assert_debug_snapshot!("dollar_at_end", compiler.tokenize("text$"));

    // Test $ followed by end of string
    assert_debug_snapshot!("just_dollar", compiler.tokenize("$"));
}

#[test]
fn test_lexer_dollar_followed_by_dollar() {
    let compiler = AmberCompiler::new();

    // Test first $ of $$ (line 217-218 - next_ch == '$' false branch)
    assert_debug_snapshot!("dollar_dollar", compiler.tokenize("$$"));

    // Test $$$ (triple dollar)
    assert_debug_snapshot!("triple_dollar", compiler.tokenize("$$$"));
}

#[test]
fn test_lexer_dollar_prev_is_dollar() {
    let compiler = AmberCompiler::new();

    // Test second $ with whitespace after (lines 221-223)
    assert_debug_snapshot!("dollar_dollar_space", compiler.tokenize("$$ "));

    // Test second $ with non-whitespace after (line 222 - !next_ch.is_whitespace())
    assert_debug_snapshot!("dollar_dollar_char", compiler.tokenize("$$x"));
}

#[test]
fn test_lexer_string_interpolation_edge_cases() {
    let compiler = AmberCompiler::new();

    // Test nested braces in string interpolation
    assert_debug_snapshot!(
        "string_nested_braces",
        compiler.tokenize(r#""text {if true { "inner" } else { "other" }} end""#)
    );

    // Test multiple interpolations
    assert_debug_snapshot!(
        "string_multi_interpolation",
        compiler.tokenize(r#""{a} and {b} and {c}""#)
    );

    // Test empty interpolation
    assert_debug_snapshot!(
        "string_empty_interpolation",
        compiler.tokenize(r#""test {} end""#)
    );
}

#[test]
fn test_lexer_command_interpolation_edge_cases() {
    let compiler = AmberCompiler::new();

    // Test nested braces in command interpolation
    assert_debug_snapshot!(
        "command_nested_braces",
        compiler.tokenize(r#"$echo {if true { 1 } else { 0 }}$"#)
    );

    // Test command with multiple interpolations
    assert_debug_snapshot!(
        "command_multi_interpolation",
        compiler.tokenize("$echo {a} {b} {c}$")
    );
}

#[test]
fn test_lexer_context_transitions() {
    let compiler = AmberCompiler::new();

    // Test transitioning between contexts
    assert_debug_snapshot!(
        "context_string_to_main",
        compiler.tokenize(r#""start {expr} end""#)
    );

    // Test command to main context
    assert_debug_snapshot!("context_command_to_main", compiler.tokenize("$cmd {expr}$"));

    // Test multiple context switches
    assert_debug_snapshot!(
        "context_multiple_switches",
        compiler.tokenize(r#""a {$b$} c""#)
    );
}

#[test]
fn test_lexer_brace_depth_tracking() {
    let compiler = AmberCompiler::new();

    // Test brace depth increases and decreases correctly
    assert_debug_snapshot!("brace_depth_simple", compiler.tokenize(r#""{{}}" "#));

    // Test brace depth with nested interpolations
    assert_debug_snapshot!(
        "brace_depth_nested",
        compiler.tokenize(r#""outer { "inner {x}" }""#)
    );

    // Test braces outside interpolation context
    assert_debug_snapshot!(
        "brace_no_interpolation",
        compiler.tokenize("{ let x = {} }")
    );
}

#[test]
fn test_lexer_error_recovery() {
    let compiler = AmberCompiler::new();

    // Test with various malformed inputs
    assert_debug_snapshot!("unclosed_string", compiler.tokenize(r#""unclosed"#));

    // Test with unclosed command
    assert_debug_snapshot!("unclosed_command", compiler.tokenize("$echo test"));

    // Test with mismatched braces
    assert_debug_snapshot!("mismatched_braces", compiler.tokenize(r#""{{{""#));
}

#[test]
fn test_lexer_single_dollar_variations() {
    let compiler = AmberCompiler::new();

    // Test single $ starting command
    assert_debug_snapshot!("single_dollar_cmd", compiler.tokenize("$echo$"));

    // Test $ not starting command (followed by whitespace in $$ case)
    assert_debug_snapshot!("dollar_no_cmd", compiler.tokenize("$ "));

    // Test $ followed by another $
    assert_debug_snapshot!("dollar_followed_by_dollar", compiler.tokenize("$$$"));
}

#[test]
fn test_lexer_edge_case_positions() {
    let compiler = AmberCompiler::new();

    // Test token at very end of input
    assert_debug_snapshot!("token_at_end", compiler.tokenize("let x"));

    // Test empty input
    assert_debug_snapshot!("empty_input", compiler.tokenize(""));

    // Test single character
    assert_debug_snapshot!("single_char", compiler.tokenize("x"));
}

#[test]
fn test_stdlib() {
    let stdlib = read_to_string("resources/alpha034/std/main.ab").unwrap();

    assert_debug_snapshot!(parse(&tokenize(&stdlib)));
}
