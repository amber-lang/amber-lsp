//! Salsa-based incremental analysis database for alpha050.
//!
//! This module defines the query-based analysis pipeline that mirrors
//! the existing imperative analysis in `global.rs`, `stmnts.rs`, and `exp.rs`.
//!
//! The Salsa database coexists with the legacy `Backend`/`AnalysisHost`
//! via the "Shadow Analysis" pattern: both engines run on the same input,
//! and results are compared for validation before the legacy path is retired.
//!
//! ## Module structure
//!
//! - [`types`] — Salsa interned, input, and tracked types plus plain data types
//! - [`parse`] — Tokenize + parse query with memoization
//! - [`imports`] — Import resolution query and path resolution helpers
//! - [`analyze`] — Main per-file analysis query with cycle recovery
//! - [`generics`] — Per-function generics derivation query
//! - [`stdlib`] — Embedded stdlib file utilities and pre-registration

use std::collections::HashMap;

use amber_types::AmberVersion;

pub mod analyze;
pub mod generics;
pub mod imports;
pub mod parse;
pub mod stdlib;
pub mod types;

#[cfg(test)]
mod tests;

// Re-export commonly used items for convenience
pub use self::analyze::{
    analyze_file,
    build_import_resolution,
    ImportResolution,
};
pub use self::generics::derive_function_generics;
pub use self::imports::{
    resolve_import_path,
    resolve_imports,
};
pub use self::parse::{
    file_parse_errors,
    file_semantic_tokens,
    parse_file,
    tokenize_and_parse,
};
pub use self::stdlib::{
    collect_stdlib_paths,
    read_stdlib_content,
};
pub use self::types::{
    AnalysisResult,
    DiagnosticSeverity,
    FileIndex,
    FilePath,
    FunctionId,
    ParseOutput,
    ParsedFile,
    PureDiagnostic,
    SourceFile,
};

// ---------------------------------------------------------------------------
// Salsa database trait & implementation
// ---------------------------------------------------------------------------

/// The Salsa database trait.
///
/// All tracked query functions take `&dyn Db` as their first parameter.
#[salsa::db]
pub trait Db: salsa::Database {}

/// Concrete Salsa database that stores all tables.
#[salsa::db]
#[derive(Default, Clone)]
pub struct AnalysisDatabase {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for AnalysisDatabase {}

#[salsa::db]
impl Db for AnalysisDatabase {}

impl AnalysisDatabase {
    /// Create a new empty analysis database.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new empty FileIndex.
    pub fn new_file_index(&self) -> FileIndex {
        FileIndex::new(self, HashMap::new())
    }

    /// Register or update a file in the Salsa database.
    ///
    /// Returns the `SourceFile` input handle. If the file already exists
    /// (tracked externally), its text and version are updated.
    /// Otherwise a new SourceFile is created.
    pub fn set_file(
        &mut self,
        existing: Option<SourceFile>,
        path: &str,
        text: &str,
        version: i32,
        amber_version: AmberVersion,
    ) -> SourceFile {
        use salsa::Setter;
        if let Some(sf) = existing {
            sf.set_text(self).to(text.to_string());
            sf.set_version(self).to(version);
            sf
        } else {
            SourceFile::new(
                self,
                path.to_string(),
                text.to_string(),
                version,
                amber_version,
            )
        }
    }

    /// Update the FileIndex to include a new path→SourceFile mapping.
    pub fn update_file_index(&mut self, file_index: FileIndex, path: String, source: SourceFile) {
        use salsa::Setter;
        let mut files = file_index.files(self).clone();
        files.insert(path, source);
        file_index.set_files(self).to(files);
    }
}
