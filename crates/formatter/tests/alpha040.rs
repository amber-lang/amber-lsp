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
    assert_eq!(formatted, output);
}

#[test]
fn import_allow_newlines() {
    let data = r#"import { function } from "std/array"
import { function } from "std/array"

import { function } from "std/array"
"#;

    test_format(data, data);
}

#[test]
fn imports_singleline() {
    let input = r#"import { function } from "std/array" import { function } from "std/array" import { function } from "std/array""#;
    let output = r#"import { function } from "std/array"
import { function } from "std/array"
import { function } from "std/array"
"#;

    test_format(input, output);
}

#[test]
fn imports_whitespace() {
    test_format(
        r#"import  {  function  }  from  "std/array"  "#,
        r#"import { function } from "std/array"
"#,
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
        r#"import { function } from "std/array"
"#,
    );
}

#[test]
fn imports_multifunction() {
    test_format(
        r#"import { function,other } from "std/array""#,
        r#"import { function, other } from "std/array"
"#,
    );
}
