//! Tests for import resolution.

use std::collections::{
    HashMap,
    HashSet,
};

use amber_types::AmberVersion;

use super::make_db_with_file;
use crate::alpha050::salsa::imports::{
    resolve_import_path,
    resolve_imports,
};
use crate::alpha050::salsa::stdlib::collect_stdlib_paths;
use crate::alpha050::salsa::types::{
    FileIndex,
    SourceFile,
};
use crate::alpha050::salsa::AnalysisDatabase;

#[test]
fn test_resolve_imports_empty() {
    let (db, sf, fi) = make_db_with_file("let x = 42");
    let imports = resolve_imports(&db, sf, fi);
    assert_eq!(imports.len(), 1);
    assert_eq!(imports[0].path(&db), "builtin.ab");
}

#[test]
fn test_resolve_import_path_stdlib() {
    let known = collect_stdlib_paths(AmberVersion::Alpha050);
    let result = resolve_import_path("/main.ab", "std/math", AmberVersion::Alpha050, &known);
    assert_eq!(result, Some("std/math.ab".to_string()));
}

#[test]
fn test_resolve_import_path_relative() {
    let known = HashSet::new();
    let result = resolve_import_path("/src/main.ab", "utils.ab", AmberVersion::Alpha050, &known);
    assert_eq!(result, Some("/src/utils.ab".to_string()));
}

#[test]
fn test_resolve_import_path_relative_dot_slash() {
    let known = HashSet::new();
    let result = resolve_import_path("/src/main.ab", "./utils.ab", AmberVersion::Alpha050, &known);
    assert_eq!(result, Some("/src/utils.ab".to_string()));
}

#[test]
fn test_resolve_import_path_unknown_stdlib() {
    let known = collect_stdlib_paths(AmberVersion::Alpha050);
    let result = resolve_import_path(
        "/main.ab",
        "std/nonexistent",
        AmberVersion::Alpha050,
        &known,
    );
    assert_eq!(result, None);
}

#[test]
fn test_resolve_imports_with_stdlib() {
    let mut db = AnalysisDatabase::new();
    let fi = db.new_file_index();
    db.register_stdlib(AmberVersion::Alpha050, fi);

    let sf = SourceFile::new(
        &db,
        "/test/main.ab".to_string(),
        r#"import * from "std/math""#.to_string(),
        1,
        AmberVersion::Alpha050,
    );
    db.update_file_index(fi, "/test/main.ab".to_string(), sf);

    let imports = resolve_imports(&db, sf, fi);
    let import_paths: Vec<&str> = imports.iter().map(|s| s.path(&db).as_str()).collect();
    assert!(
        import_paths.contains(&"std/math.ab"),
        "Expected std/math.ab in imports, got: {:?}",
        import_paths
    );
    assert!(
        import_paths.contains(&"builtin.ab"),
        "Expected builtin.ab in imports (implicit), got: {:?}",
        import_paths
    );
}

#[test]
fn test_resolve_imports_builtin_no_self_import() {
    let mut db = AnalysisDatabase::new();
    let fi = db.new_file_index();

    let builtin_sf = SourceFile::new(
        &db,
        "builtin.ab".to_string(),
        "let BUILTIN = true".to_string(),
        0,
        AmberVersion::Alpha050,
    );
    db.update_file_index(fi, "builtin.ab".to_string(), builtin_sf);

    let imports = resolve_imports(&db, builtin_sf, fi);
    assert!(
        imports.is_empty(),
        "builtin.ab should not import itself, got: {:?}",
        imports.iter().map(|s| s.path(&db)).collect::<Vec<_>>()
    );
}

#[test]
fn test_file_index_lookup() {
    let db = AnalysisDatabase::new();
    let sf = SourceFile::new(
        &db,
        "test.ab".to_string(),
        "let x = 1".to_string(),
        1,
        AmberVersion::Alpha050,
    );
    let mut files = HashMap::new();
    files.insert("test.ab".to_string(), sf);
    let fi = FileIndex::new(&db, files);

    assert!(fi.get(&db, "test.ab") == Some(sf));
    assert!(fi.get(&db, "nonexistent.ab").is_none());
}
