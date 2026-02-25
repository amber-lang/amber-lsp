use amber_fmt::format;
use amber_grammar::{
    Grammar, LSPAnalysis as _, Span,
    alpha040::{AmberCompiler, GlobalStatement},
};

/// Parse the input string through the amber compiler.
fn parse(data: &str) -> Vec<(GlobalStatement, Span)> {
    let amber_compiler = AmberCompiler::new();
    let tokenize = amber_compiler.tokenize(&data);
    let parse = amber_compiler.parse(&tokenize);

    let Grammar::Alpha040(Some(items)) = parse.ast else {
        panic!("Cannot parse test text");
    };

    items
}

/// Asserts that the input string matches the output string after formatting.
fn test_format(input: &str, output: &str) {
    let items = parse(input);
    let formatted = format(&items, input).expect("Able to format amber file");
    eprintln!("Formatted:\n{}\nOutput:\n{}", formatted, output);
    assert_eq!(formatted, output);
}

#[test]
fn import_allow_newlines() {
    test_format(
        r#"import { function } from "std/array"
import { function } from "std/array"

import { function } from "std/array"


import { large } from "std/array""#,
        r#"import { function } from "std/array"
import { function } from "std/array"

import { function } from "std/array"

import { large } from "std/array""#,
    );
}

#[test]
fn imports_singleline() {
    let input = r#"import { function } from "std/array" import { function } from "std/array" import { function } from "std/array""#;
    let output = r#"import { function } from "std/array"
import { function } from "std/array"
import { function } from "std/array""#;

    test_format(input, output);
}

#[test]
fn imports_whitespace() {
    test_format(
        r#"import  {  function  }  from  "std/array"  "#,
        r#"import { function } from "std/array""#,
    );
}

#[test]
fn imports_newlines() {
    test_format(
        r#"import
{
function
}
from
"std/array"
"#,
        r#"import { function } from "std/array""#,
    );
}

#[test]
fn imports_multifunction() {
    test_format(
        r#"import { function,other } from "std/array""#,
        r#"import { function, other } from "std/array""#,
    );
}

#[test]
fn top_level() {
    test_format(
        r#"echo "abc" echo "123""#,
        r#"echo "abc"
echo "123""#,
    );
}

#[test]
fn top_level_newlines() {
    test_format(
        r#"echo "abc"
echo "123"

echo "123"


echo "large"
"#,
        r#"echo "abc"
echo "123"

echo "123"

echo "large""#,
    );
}

#[test]
fn if_spacing() {
    test_format(
        r#"if true {
    echo "a"
}"#,
        r#"if true {
    echo "a"
}"#,
    );
}

#[test]
fn newline_around_funcs() {
    test_format(
        concat!(
            "let a = 1\n",
            "fun some() {\n    let a = 2\n}\n",
            "let a = 3\n",
            "main {\n    let a = 4\n}\n",
            "let a = 5",
        ),
        concat!(
            "let a = 1\n\n",
            "fun some() {\n    let a = 2\n}\n\n",
            "let a = 3\n\n",
            "main {\n    let a = 4\n}\n\n",
            "let a = 5",
        ),
    );
}

#[test]
fn return_type_spacing_func() {
    test_format("fun some() :Num{\n    2\n}", "fun some(): Num {\n    2\n}");
}

#[test]
fn function_args() {
    test_format(
        "fun some(arg1,arg2:Num,arg3=10,arg4:Num=10) :Num{\n    2\n}",
        "fun some(arg1, arg2: Num, arg3 = 10, arg4: Num = 10): Num {\n    2\n}",
    );
}
