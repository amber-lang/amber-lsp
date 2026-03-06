//! Tests for file analysis queries.

use std::collections::HashMap;

use amber_types::AmberVersion;

use super::make_db_with_file;
use crate::alpha050::salsa::analyze::analyze_file;
use crate::alpha050::salsa::types::{
    DiagnosticSeverity,
    FileIndex,
    SourceFile,
};
use crate::alpha050::salsa::AnalysisDatabase;

#[test]
fn test_analyze_file_returns_parse_errors_as_diagnostics() {
    let (db, sf, fi) = make_db_with_file("fun {");
    let analysis = analyze_file(&db, sf, fi);
    let diagnostics = analysis.diagnostics(&db);
    assert!(
        !diagnostics.is_empty(),
        "Expected diagnostics from parse errors"
    );
    assert_eq!(diagnostics[0].severity, DiagnosticSeverity::Error);
}

#[test]
fn test_analyze_file_valid_code() {
    let (db, sf, fi) = make_db_with_file(
        r#"
fun greet(name) {
    echo "Hello, {name}"
}
"#,
    );
    let analysis = analyze_file(&db, sf, fi);
    assert!(
        analysis.diagnostics(&db).is_empty(),
        "Unexpected diagnostics: {:?}",
        analysis.diagnostics(&db)
    );
}

#[test]
fn test_set_file_creates_new() {
    let mut db = AnalysisDatabase::new();
    let sf = db.set_file(None, "/test.ab", "let x = 1", 1, AmberVersion::Alpha050);
    assert_eq!(sf.path(&db), "/test.ab");
    assert_eq!(sf.text(&db), "let x = 1");
    assert_eq!(sf.version(&db), 1);
}

#[test]
fn test_set_file_updates_existing() {
    let mut db = AnalysisDatabase::new();
    let sf = db.set_file(None, "/test.ab", "let x = 1", 1, AmberVersion::Alpha050);

    let sf2 = db.set_file(Some(sf), "/test.ab", "let x = 2", 2, AmberVersion::Alpha050);

    assert!(sf == sf2);
    assert_eq!(sf2.text(&db), "let x = 2");
    assert_eq!(sf2.version(&db), 2);
}

#[test]
fn test_analyze_file_with_imports() {
    let mut db = AnalysisDatabase::new();
    let fi = db.new_file_index();
    db.register_stdlib(AmberVersion::Alpha050, fi);

    let sf = SourceFile::new(
        &db,
        "/test/main.ab".to_string(),
        r#"import * from "std/math"
let x = 42
"#
        .to_string(),
        1,
        AmberVersion::Alpha050,
    );
    db.update_file_index(fi, "/test/main.ab".to_string(), sf);

    let analysis = analyze_file(&db, sf, fi);
    assert!(
        analysis.diagnostics(&db).is_empty(),
        "Unexpected diagnostics: {:?}",
        analysis.diagnostics(&db)
    );
}

#[test]
fn test_analyze_file_produces_symbol_table() {
    let (db, sf, fi) = make_db_with_file(
        r#"
fun greet(name) {
    echo "Hello, {name}"
}
"#,
    );
    let result = analyze_file(&db, sf, fi);
    let table = result.symbol_table(&db);
    let has_greet = table.symbols.iter().any(|(_, info)| info.name == "greet");
    assert!(has_greet, "Symbol table should contain 'greet' function");
}

#[test]
fn test_analyze_file_variable_in_symbol_table() {
    let (db, sf, fi) = make_db_with_file("let x = 42");
    let result = analyze_file(&db, sf, fi);
    let table = result.symbol_table(&db);
    let has_x = table.symbols.iter().any(|(_, info)| info.name == "x");
    assert!(has_x, "Symbol table should contain 'x'");
}

#[test]
fn test_analyze_file_import_resolution_populates_symbol_table() {
    let db = AnalysisDatabase::new();

    let lib_sf = SourceFile::new(
        &db,
        "/test/lib.ab".to_string(),
        "pub fun helper() {\n    echo \"help\"\n}".to_string(),
        1,
        AmberVersion::Alpha050,
    );

    let main_sf = SourceFile::new(
        &db,
        "/test/main.ab".to_string(),
        "import * from \"./lib.ab\"\nhelper()".to_string(),
        1,
        AmberVersion::Alpha050,
    );

    let builtin_sf = SourceFile::new(
        &db,
        "builtin.ab".to_string(),
        String::new(),
        1,
        AmberVersion::Alpha050,
    );

    let mut files = HashMap::new();
    files.insert("/test/main.ab".to_string(), main_sf);
    files.insert("/test/lib.ab".to_string(), lib_sf);
    files.insert("builtin.ab".to_string(), builtin_sf);
    let fi = FileIndex::new(&db, files);

    let result = analyze_file(&db, main_sf, fi);
    let diags = result.diagnostics(&db);
    let has_no_file_error = !diags.iter().any(|d| d.message == "File doesn't exist");
    assert!(
        has_no_file_error,
        "Should not have 'File doesn\\'t exist' error, got: {:?}",
        diags
    );
}

#[test]
fn test_analyze_file_caching_with_analysis() {
    let (db, sf, fi) = make_db_with_file("let x = 42");
    let result1 = analyze_file(&db, sf, fi);
    let result2 = analyze_file(&db, sf, fi);
    assert_eq!(result1.diagnostics(&db), result2.diagnostics(&db));
}

#[test]
fn test_analyze_file_generics_snapshot() {
    let (db, sf, fi) = make_db_with_file(
        r#"
fun identity(x) {
    return x
}
"#,
    );
    let result = analyze_file(&db, sf, fi);
    let _snapshot = result.generics_snapshot(&db);
}
