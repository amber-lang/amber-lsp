//! Tests for the parse query and tokenize_and_parse.

use amber_grammar::Grammar;
use amber_types::AmberVersion;

use super::make_db_with_file;
use crate::alpha050::salsa::parse::{
    parse_file,
    tokenize_and_parse,
};

#[test]
fn test_parse_file_simple_function() {
    let (db, sf, _fi) = make_db_with_file(
        r#"
fun add(a, b) {
    return a + b
}
"#,
    );
    let parsed = parse_file(&db, sf);
    assert!(parsed.source(&db) == sf);
    assert!(
        parsed.parse_errors(&db).is_empty(),
        "Unexpected parse errors: {:?}",
        parsed.parse_errors(&db)
    );
    match parsed.ast(&db) {
        Grammar::Alpha050(Some(stmts)) => {
            assert!(!stmts.is_empty(), "Expected at least one global statement");
        }
        other => panic!("Expected Grammar::Alpha050(Some(_)), got {:?}", other),
    }
}

#[test]
fn test_parse_file_with_errors() {
    let (db, sf, _fi) = make_db_with_file("fun {");
    let parsed = parse_file(&db, sf);
    assert!(
        !parsed.parse_errors(&db).is_empty(),
        "Expected parse errors for invalid syntax"
    );
}

#[test]
fn test_parse_file_caching() {
    let (db, sf, _fi) = make_db_with_file("let x = 42");
    let result1 = parse_file(&db, sf);
    let result2 = parse_file(&db, sf);
    assert!(result1 == result2);
}

#[test]
fn test_parse_file_invalidation() {
    use salsa::Setter;
    let (mut db, sf, _fi) = make_db_with_file("let x = 42");

    let result1 = parse_file(&db, sf);
    let errors1 = result1.parse_errors(&db).clone();

    sf.set_text(&mut db).to("fun {".to_string());

    let result2 = parse_file(&db, sf);
    let errors2 = result2.parse_errors(&db).clone();

    assert!(errors1.is_empty());
    assert!(!errors2.is_empty());
}

#[test]
fn test_tokenize_and_parse_pure() {
    let output = tokenize_and_parse("let x = 42", AmberVersion::Alpha050);
    assert!(output.errors.is_empty());
    match output.ast {
        Grammar::Alpha050(Some(stmts)) => assert!(!stmts.is_empty()),
        _ => panic!("Expected Alpha050 grammar"),
    }
}
