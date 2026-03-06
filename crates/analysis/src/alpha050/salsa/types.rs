//! Salsa type definitions: interned, input, tracked, and plain data types.
//!
//! This module contains all the Salsa-managed types used by the incremental
//! analysis pipeline, plus plain data types shared across the pipeline.

use std::collections::HashMap;

use amber_grammar::{
    Grammar,
    SpannedSemanticToken,
};
use amber_types::{
    AmberVersion,
    GenericsSnapshot,
};
use chumsky::span::SimpleSpan;

use super::Db;
use crate::SymbolTable;

// ---------------------------------------------------------------------------
// Salsa interned types
// ---------------------------------------------------------------------------

/// Interned file path — cheap to copy, Eq, Hash.
#[salsa::interned]
pub struct FilePath<'db> {
    pub path: String,
}

/// Interned function identity for per-function generic memoization.
#[salsa::interned]
pub struct FunctionId<'db> {
    pub file_path: String,
    pub name: String,
}

// ---------------------------------------------------------------------------
// Salsa input types
// ---------------------------------------------------------------------------

/// The primary input — one per file known to the LSP.
///
/// Setting `text` is the "change notification" that invalidates downstream queries.
#[salsa::input(debug)]
pub struct SourceFile {
    /// The file path string (used for display / import resolution).
    #[returns(ref)]
    pub path: String,

    /// The raw source text.
    #[returns(ref)]
    pub text: String,

    /// Monotonic version counter (for LSP versioning).
    pub version: i32,

    /// Which Amber version grammar to use.
    pub amber_version: AmberVersion,
}

/// Global file index — maps path strings to SourceFile handles.
///
/// This is the central lookup for import resolution.
/// Updated whenever files are added/removed from the database.
#[salsa::input]
pub struct FileIndex {
    /// Map from canonical file path to SourceFile.
    /// Keyed by the path string used in `SourceFile::path`.
    #[returns(ref)]
    pub files: HashMap<String, SourceFile>,
}

impl FileIndex {
    /// Look up a SourceFile by path.
    pub fn get(&self, db: &dyn Db, path: &str) -> Option<SourceFile> {
        self.files(db).get(path).copied()
    }
}

// ---------------------------------------------------------------------------
// Salsa tracked structs
// ---------------------------------------------------------------------------

/// Tracked struct holding parse results.
///
/// `source` is the identity (untracked, has Hash). Data fields are `#[tracked]`
/// meaning they have per-field revision tracking and don't need Hash — they
/// only need `Update` (or fall back to `'static + PartialEq`).
#[salsa::tracked(debug)]
pub struct ParsedFile<'db> {
    pub source: SourceFile,
    #[tracked]
    pub ast: Grammar,
    #[tracked]
    pub parse_errors: Vec<(String, SimpleSpan)>,
    #[tracked]
    pub semantic_tokens: Vec<SpannedSemanticToken>,
}

/// Per-file analysis output tracked struct.
///
/// `source` is the identity (untracked). All data fields are `#[tracked]`
/// for individual revision tracking. `SymbolTable` lacks `PartialEq` so
/// salsa will always consider it changed — acceptable for initial integration.
#[salsa::tracked(debug)]
pub struct AnalysisResult<'db> {
    pub source: SourceFile,
    #[tracked]
    pub diagnostics: Vec<PureDiagnostic>,
    #[tracked]
    pub symbol_table: SymbolTable,
    #[tracked]
    pub generics_snapshot: GenericsSnapshot,
}

// ---------------------------------------------------------------------------
// Plain data types (not Salsa-managed)
// ---------------------------------------------------------------------------

/// Severity level for diagnostics produced by the Salsa pipeline.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
}

/// A diagnostic produced by pure analysis.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PureDiagnostic {
    pub message: String,
    pub span: SimpleSpan,
    pub severity: DiagnosticSeverity,
}

/// Output of the pure tokenize + parse step.
pub struct ParseOutput {
    pub ast: Grammar,
    pub errors: Vec<(String, SimpleSpan)>,
    pub semantic_tokens: Vec<SpannedSemanticToken>,
}
