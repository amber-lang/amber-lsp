//! Tests for stdlib utilities and generics.

use amber_types::AmberVersion;

use crate::alpha050::salsa::generics::derive_function_generics;
use crate::alpha050::salsa::parse::parse_file;
use crate::alpha050::salsa::stdlib::{
    collect_stdlib_paths,
    read_stdlib_content,
};
use crate::alpha050::salsa::types::FunctionId;
use crate::alpha050::salsa::AnalysisDatabase;

#[test]
fn test_collect_stdlib_paths_alpha050() {
    let paths = collect_stdlib_paths(AmberVersion::Alpha050);
    assert!(!paths.is_empty(), "Expected stdlib paths for Alpha050");
    assert!(
        paths.contains("std/array.ab"),
        "Expected std/array.ab in stdlib paths"
    );
    assert!(
        paths.contains("std/math.ab"),
        "Expected std/math.ab in stdlib paths"
    );
}

#[test]
fn test_read_stdlib_content() {
    let content = read_stdlib_content(AmberVersion::Alpha050, "std/math.ab");
    assert!(
        content.is_some(),
        "Expected to read std/math.ab for Alpha050"
    );
    assert!(
        !content.unwrap().is_empty(),
        "Expected non-empty stdlib content"
    );
}

#[test]
fn test_register_stdlib() {
    let mut db = AnalysisDatabase::new();
    let fi = db.new_file_index();
    let files = db.register_stdlib(AmberVersion::Alpha050, fi);
    assert!(!files.is_empty(), "Expected stdlib files to be registered");

    for sf in &files {
        let parsed = parse_file(&db, *sf);
        let _ = parsed.ast(&db);
    }
}

#[test]
fn test_derive_function_generics_stub() {
    let db = AnalysisDatabase::new();
    let func = FunctionId::new(&db, "/test.ab".to_string(), "add".to_string());
    let generics = derive_function_generics(&db, func);
    assert_eq!(generics, amber_types::GenericsSnapshot::default());
}
