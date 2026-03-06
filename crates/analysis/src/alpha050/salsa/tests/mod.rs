//! Tests for the Salsa incremental analysis pipeline.

mod analyze_tests;
mod import_tests;
mod parse_tests;
mod stdlib_tests;

use std::collections::HashMap;

use amber_types::AmberVersion;

use super::types::{
    FileIndex,
    SourceFile,
};
use super::AnalysisDatabase;

/// Create a test database with a single file and a file index.
///
/// Registers a minimal builtin so the implicit `import * from "builtin"` resolves.
pub(crate) fn make_db_with_file(code: &str) -> (AnalysisDatabase, SourceFile, FileIndex) {
    let db = AnalysisDatabase::new();
    let sf = SourceFile::new(
        &db,
        "/test/main.ab".to_string(),
        code.to_string(),
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
    files.insert("/test/main.ab".to_string(), sf);
    files.insert("builtin.ab".to_string(), builtin_sf);
    let file_index = FileIndex::new(&db, files);
    (db, sf, file_index)
}
