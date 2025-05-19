use std::fs::read_to_string;

use chumsky::error::Rich;
use insta::assert_debug_snapshot;

use amber_lsp::grammar::{
    alpha034::{lexer::Token, semantic_tokens::semantic_tokens_from_ast, AmberCompiler, Spanned},
    LSPAnalysis, ParserResponse,
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
fn test_stdlib() {
    let stdlib = read_to_string("resources/alpha034/std/main.ab").unwrap();

    assert_debug_snapshot!(parse(&tokenize(&stdlib)));
}
