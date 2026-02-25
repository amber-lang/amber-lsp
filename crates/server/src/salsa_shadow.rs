//! Shadow analysis module using the Salsa incremental framework.
//!
//! `SalsaShadow` wraps the Salsa-based `AnalysisDatabase` and runs
//! in parallel with the existing imperative analysis pipeline.
//! It receives the same document change events, runs Salsa queries,
//! and provides comparison methods to detect divergences.
//!
//! This module is the bridge between the LSP server's async, URI-based
//! world and the Salsa database's synchronous, path-based world.

use std::collections::HashMap;

use amber_analysis::alpha050::pure_analysis::file_key_for_path;
use amber_analysis::alpha050::salsa_db::{
    analyze_file,
    collect_stdlib_paths,
    parse_file,
    resolve_import_path,
    resolve_imports,
    AnalysisDatabase,
    DiagnosticSeverity,
    FileIndex,
    PureDiagnostic,
    SourceFile,
};
use amber_analysis::files::FileVersion;
use amber_analysis::SymbolTable;
use amber_grammar::{
    Grammar,
    SpannedSemanticToken,
};
use amber_types::paths::FileId;
use amber_types::{
    AmberVersion,
    GenericsSnapshot,
};
use chumsky::span::SimpleSpan;

/// Shadow analysis engine backed by Salsa.
///
/// Maintains a Salsa `AnalysisDatabase` that mirrors the imperative pipeline's
/// file state. When a document changes, `update_file` propagates the change
/// into the Salsa database, and `analyze` runs the memoized query graph.
pub struct SalsaShadow {
    db: AnalysisDatabase,
    file_index: FileIndex,
    /// Maps the legacy `FileId` (from `PathInterner`) → Salsa `SourceFile`.
    file_map: HashMap<FileId, SourceFile>,
    amber_version: AmberVersion,
}

/// Comparison result between legacy and Salsa analysis outputs.
#[derive(Debug)]
pub struct ComparisonResult {
    pub diagnostics_match: bool,
    pub symbol_table_match: bool,
    pub legacy_error_count: usize,
    pub salsa_error_count: usize,
    pub legacy_warning_count: usize,
    pub salsa_warning_count: usize,
}

impl SalsaShadow {
    /// Create a new shadow analysis engine.
    ///
    /// Registers stdlib files into the Salsa database immediately.
    pub fn new(amber_version: AmberVersion) -> Self {
        let mut db = AnalysisDatabase::new();
        let file_index = db.new_file_index();

        // Register stdlib so imports resolve
        let _stdlib_files = db.register_stdlib(amber_version.clone(), file_index);

        SalsaShadow {
            db,
            file_index,
            file_map: HashMap::new(),
            amber_version,
        }
    }

    /// Register or update a file in the Salsa database.
    ///
    /// `file_id` is the legacy `FileId` from `PathInterner`.
    /// `path` is the file's URI path (e.g. `/home/user/project/main.ab`).
    /// `text` is the full file content.
    /// `version` is the LSP document version.
    pub fn update_file(&mut self, file_id: FileId, path: &str, text: &str, version: i32) {
        let existing = self.file_map.get(&file_id).copied();
        let source = self
            .db
            .set_file(existing, path, text, version, self.amber_version.clone());

        if existing.is_none() {
            // New file — add to file index
            self.db
                .update_file_index(self.file_index, path.to_string(), source);
        }

        self.file_map.insert(file_id, source);
    }

    /// Build a mapping from Salsa-generated file keys to Backend file keys.
    ///
    /// The Salsa pipeline uses `file_key_for_path` to create `(FileId, FileVersion)`
    /// tuples by hashing path strings. These differ from the Backend's own FileIds.
    /// This method returns a mapping that can be used to rewrite FileIds in the
    /// symbol table so they match the Backend's file system.
    pub fn build_file_key_mapping(
        &self,
        backend_file_id: FileId,
        backend_version: FileVersion,
    ) -> HashMap<(FileId, FileVersion), (FileId, FileVersion)> {
        let mut mapping = HashMap::new();

        // Map the current file's Salsa key to the Backend's key
        if let Some(source) = self.file_map.get(&backend_file_id) {
            let path = source.path(&self.db);
            let salsa_key = file_key_for_path(path);
            mapping.insert(salsa_key, (backend_file_id, backend_version));
        }

        // Map other tracked files
        for (&other_file_id, source) in &self.file_map {
            if other_file_id == backend_file_id {
                continue;
            }
            let path = source.path(&self.db);
            let salsa_key = file_key_for_path(path);
            // For other files we don't know the Backend version, so use the
            // Salsa key's version (0). The Backend will look up the latest
            // version when it needs the document.
            mapping.insert(salsa_key, (other_file_id, FileVersion(0)));
        }

        mapping
    }

    /// Discover local import paths that are not yet registered in the file index.
    ///
    /// Parses the file AST for import statements, resolves relative paths,
    /// and returns the list of canonical file paths that need to be read
    /// from disk and registered.
    pub fn discover_unregistered_imports(&self, file_id: &FileId) -> Vec<String> {
        let source = match self.file_map.get(file_id) {
            Some(s) => *s,
            None => return vec![],
        };

        let parsed = parse_file(&self.db, source);
        let ast = parsed.ast(&self.db);
        let source_path = source.path(&self.db);
        let amber_version = source.amber_version(&self.db);

        let known_stdlib = collect_stdlib_paths(amber_version.clone());
        let file_index_files = self.file_index.files(&self.db);

        let mut unregistered = Vec::new();

        if let Grammar::Alpha050(Some(stmts)) = &ast {
            for (stmt, _) in stmts {
                if let amber_grammar::alpha050::GlobalStatement::Import(_, _, _, _, (path, _)) =
                    stmt
                {
                    if let Some(resolved) =
                        resolve_import_path(source_path, path, amber_version.clone(), &known_stdlib)
                    {
                        // Skip stdlib paths — they're already registered
                        if resolved.starts_with("std/") || resolved == "builtin.ab" {
                            continue;
                        }
                        // Skip if already in the file index
                        if file_index_files.contains_key(&resolved) {
                            continue;
                        }
                        unregistered.push(resolved);
                    }
                }
            }
        }

        unregistered
    }

    /// Register a local file by path and content (without a Backend FileId).
    ///
    /// Used for auto-discovered imported files that haven't been opened in the editor.
    /// Returns true if the file was newly registered, false if already known.
    pub fn register_local_file(&mut self, path: &str, text: &str) -> bool {
        // Check if already in the file index
        if self.file_index.files(&self.db).contains_key(path) {
            return false;
        }

        let source = self
            .db
            .set_file(None, path, text, 0, self.amber_version.clone());

        self.db
            .update_file_index(self.file_index, path.to_string(), source);
        true
    }

    /// Discover unregistered imports from a file that was registered by path
    /// (not through the Backend's FileId system).
    ///
    /// Like `discover_unregistered_imports` but takes a path instead of FileId.
    pub fn discover_unregistered_imports_by_path(&self, path: &str) -> Vec<String> {
        let file_index_files = self.file_index.files(&self.db);
        let source = match file_index_files.get(path) {
            Some(s) => *s,
            None => return vec![],
        };

        let parsed = parse_file(&self.db, source);
        let ast = parsed.ast(&self.db);
        let source_path = source.path(&self.db);
        let amber_version = source.amber_version(&self.db);

        let known_stdlib = collect_stdlib_paths(amber_version.clone());

        let mut unregistered = Vec::new();

        if let Grammar::Alpha050(Some(stmts)) = &ast {
            for (stmt, _) in stmts {
                if let amber_grammar::alpha050::GlobalStatement::Import(
                    _,
                    _,
                    _,
                    _,
                    (import_path, _),
                ) = stmt
                {
                    if let Some(resolved) = resolve_import_path(
                        source_path,
                        import_path,
                        amber_version.clone(),
                        &known_stdlib,
                    ) {
                        if resolved.starts_with("std/") || resolved == "builtin.ab" {
                            continue;
                        }
                        if file_index_files.contains_key(&resolved) {
                            continue;
                        }
                        unregistered.push(resolved);
                    }
                }
            }
        }

        unregistered
    }

    /// Remove a file from the shadow tracking.
    ///
    /// Note: Salsa inputs can't truly be deleted, but we remove the mapping
    /// so the file won't be analyzed again.
    pub fn remove_file(&mut self, file_id: &FileId) {
        self.file_map.remove(file_id);
    }

    /// Run Salsa analysis on a file and return the result.
    ///
    /// Returns `None` if the file hasn't been registered with `update_file`.
    pub fn analyze(&self, file_id: &FileId) -> Option<SalsaAnalysisOutput> {
        let source = self.file_map.get(file_id)?;
        let result = analyze_file(&self.db, *source, self.file_index);

        Some(SalsaAnalysisOutput {
            diagnostics: result.diagnostics(&self.db).clone(),
            symbol_table: result.symbol_table(&self.db).clone(),
        })
    }

    /// Run full Salsa analysis and return everything the Backend needs.
    ///
    /// This combines parse results (AST, semantic tokens) with analysis results
    /// (diagnostics, symbol table, generics) into a single output struct that
    /// can be written directly into the legacy `Files` state.
    ///
    /// Returns `None` if the file hasn't been registered with `update_file`.
    pub fn full_analyze(&self, file_id: &FileId) -> Option<SalsaFullOutput> {
        let source = self.file_map.get(file_id)?;
        let source = *source;

        // Run parse (memoized)
        let parsed = parse_file(&self.db, source);
        let ast = parsed.ast(&self.db);
        let semantic_tokens = parsed.semantic_tokens(&self.db);

        // Run analysis (memoized)
        let result = analyze_file(&self.db, source, self.file_index);
        let diagnostics = result.diagnostics(&self.db);
        let symbol_table = result.symbol_table(&self.db).clone();
        let generics_snapshot = result.generics_snapshot(&self.db).clone();

        // Split diagnostics into errors and warnings (matching legacy format)
        let mut errors = Vec::new();
        let mut warnings = Vec::new();
        for d in diagnostics {
            let pair = (d.message.clone(), d.span);
            match d.severity {
                DiagnosticSeverity::Error => errors.push(pair),
                DiagnosticSeverity::Warning => warnings.push(pair),
            }
        }

        // Collect resolved import file data for registration in Backend
        let imports = resolve_imports(&self.db, source, self.file_index);
        let mut imported_files = Vec::new();
        for imported_source in imports.iter() {
            let imported_path = imported_source.path(&self.db).clone();
            let imported_text = imported_source.text(&self.db).clone();
            let imported_file_key = file_key_for_path(&imported_path);
            let imported_result = analyze_file(&self.db, *imported_source, self.file_index);
            let imported_symbol_table = imported_result.symbol_table(&self.db).clone();
            imported_files.push(ImportedFileInfo {
                salsa_file_key: imported_file_key,
                path: imported_path,
                text: imported_text,
                symbol_table: imported_symbol_table,
            });
        }

        Some(SalsaFullOutput {
            ast,
            errors,
            warnings,
            semantic_tokens,
            symbol_table,
            generics_snapshot,
            imported_files,
        })
    }

    /// Compare Salsa analysis output with legacy pipeline output.
    ///
    /// Takes the legacy diagnostics (errors + warnings) and symbol table,
    /// and the Salsa output, and returns a comparison summary.
    pub fn compare(
        legacy_errors: &[(String, chumsky::span::SimpleSpan)],
        legacy_warnings: &[(String, chumsky::span::SimpleSpan)],
        legacy_symbol_table: &SymbolTable,
        salsa_output: &SalsaAnalysisOutput,
    ) -> ComparisonResult {
        let salsa_errors: Vec<_> = salsa_output
            .diagnostics
            .iter()
            .filter(|d| d.severity == DiagnosticSeverity::Error)
            .collect();
        let salsa_warnings: Vec<_> = salsa_output
            .diagnostics
            .iter()
            .filter(|d| d.severity == DiagnosticSeverity::Warning)
            .collect();

        // Compare error messages and spans
        let diagnostics_match = {
            let mut legacy_err_set: Vec<(&str, (usize, usize))> = legacy_errors
                .iter()
                .map(|(msg, span)| (msg.as_str(), (span.start, span.end)))
                .collect();
            legacy_err_set.sort();

            let mut salsa_err_set: Vec<(&str, (usize, usize))> = salsa_errors
                .iter()
                .map(|d| (d.message.as_str(), (d.span.start, d.span.end)))
                .collect();
            salsa_err_set.sort();

            let mut legacy_warn_set: Vec<(&str, (usize, usize))> = legacy_warnings
                .iter()
                .map(|(msg, span)| (msg.as_str(), (span.start, span.end)))
                .collect();
            legacy_warn_set.sort();

            let mut salsa_warn_set: Vec<(&str, (usize, usize))> = salsa_warnings
                .iter()
                .map(|d| (d.message.as_str(), (d.span.start, d.span.end)))
                .collect();
            salsa_warn_set.sort();

            legacy_err_set == salsa_err_set && legacy_warn_set == salsa_warn_set
        };

        // Symbol table comparison (best-effort — RangeInclusiveMap layout may differ)
        let symbol_table_match = *legacy_symbol_table == salsa_output.symbol_table;

        ComparisonResult {
            diagnostics_match,
            symbol_table_match,
            legacy_error_count: legacy_errors.len(),
            salsa_error_count: salsa_errors.len(),
            legacy_warning_count: legacy_warnings.len(),
            salsa_warning_count: salsa_warnings.len(),
        }
    }

    /// Run analysis and compare with legacy output, logging any divergences.
    ///
    /// This is the main entry point called from `Backend::analyze_document`.
    pub fn analyze_and_compare(
        &self,
        file_id: &FileId,
        legacy_errors: &[(String, chumsky::span::SimpleSpan)],
        legacy_warnings: &[(String, chumsky::span::SimpleSpan)],
        legacy_symbol_table: &SymbolTable,
    ) -> Option<ComparisonResult> {
        let salsa_output = self.analyze(file_id)?;
        let comparison = Self::compare(
            legacy_errors,
            legacy_warnings,
            legacy_symbol_table,
            &salsa_output,
        );

        if !comparison.diagnostics_match {
            tracing::warn!(
                "Salsa shadow: diagnostic divergence for {:?} — legacy: {} errors + {} warnings, salsa: {} errors + {} warnings",
                file_id,
                comparison.legacy_error_count,
                comparison.legacy_warning_count,
                comparison.salsa_error_count,
                comparison.salsa_warning_count,
            );
        }

        if !comparison.symbol_table_match {
            tracing::warn!("Salsa shadow: symbol table divergence for {:?}", file_id,);
        }

        Some(comparison)
    }
}

/// Info about an imported file resolved by Salsa.
pub struct ImportedFileInfo {
    /// The Salsa-generated file key (from `file_key_for_path`).
    pub salsa_file_key: (FileId, FileVersion),
    /// The canonical path of the imported file (e.g. "std/env.ab", "builtin.ab").
    pub path: String,
    /// The file content.
    pub text: String,
    /// The symbol table produced by Salsa analysis of the imported file.
    pub symbol_table: SymbolTable,
}

/// Full output of a Salsa analysis run, containing everything
/// the Backend needs to populate `Files` state.
pub struct SalsaFullOutput {
    pub ast: Grammar,
    pub errors: Vec<(String, SimpleSpan)>,
    pub warnings: Vec<(String, SimpleSpan)>,
    pub semantic_tokens: Vec<SpannedSemanticToken>,
    pub symbol_table: SymbolTable,
    pub generics_snapshot: GenericsSnapshot,
    /// Resolved imports with their symbol tables, for cross-file features.
    pub imported_files: Vec<ImportedFileInfo>,
}

/// Output of a Salsa analysis run for a single file.
#[derive(Debug, Clone)]
pub struct SalsaAnalysisOutput {
    pub diagnostics: Vec<PureDiagnostic>,
    pub symbol_table: SymbolTable,
}

#[cfg(test)]
mod tests {
    use super::*;
    use chumsky::span::SimpleSpan;

    #[test]
    fn test_salsa_shadow_new() {
        let shadow = SalsaShadow::new(AmberVersion::Alpha050);
        assert!(shadow.file_map.is_empty());
    }

    #[test]
    fn test_salsa_shadow_update_and_analyze() {
        let mut shadow = SalsaShadow::new(AmberVersion::Alpha050);
        let file_id = FileId(100);

        shadow.update_file(file_id, "/test/main.ab", "let x = 42", 1);

        let output = shadow.analyze(&file_id);
        assert!(output.is_some(), "Should produce analysis output");

        let output = output.unwrap();
        // A simple `let x = 42` should analyze without errors
        let errors: Vec<_> = output
            .diagnostics
            .iter()
            .filter(|d| d.severity == DiagnosticSeverity::Error)
            .collect();
        assert!(
            errors.is_empty(),
            "Simple let statement should have no errors, got: {:?}",
            errors
        );
    }

    #[test]
    fn test_salsa_shadow_update_existing_file() {
        let mut shadow = SalsaShadow::new(AmberVersion::Alpha050);
        let file_id = FileId(100);

        shadow.update_file(file_id, "/test/main.ab", "let x = 42", 1);
        shadow.update_file(file_id, "/test/main.ab", "let y = 99", 2);

        let output = shadow.analyze(&file_id).unwrap();
        // Should have symbol 'y' in the table
        let has_y = output
            .symbol_table
            .symbols
            .iter()
            .any(|(_, info)| info.name == "y");
        assert!(has_y, "Updated file should have 'y' in symbol table");
    }

    #[test]
    fn test_salsa_shadow_analyze_unknown_file() {
        let shadow = SalsaShadow::new(AmberVersion::Alpha050);
        let file_id = FileId(999);
        assert!(shadow.analyze(&file_id).is_none());
    }

    #[test]
    fn test_salsa_shadow_remove_file() {
        let mut shadow = SalsaShadow::new(AmberVersion::Alpha050);
        let file_id = FileId(100);

        shadow.update_file(file_id, "/test/main.ab", "let x = 42", 1);
        assert!(shadow.analyze(&file_id).is_some());

        shadow.remove_file(&file_id);
        assert!(shadow.analyze(&file_id).is_none());
    }

    #[test]
    fn test_comparison_matching() {
        let legacy_errors: Vec<(String, SimpleSpan)> = vec![];
        let legacy_warnings: Vec<(String, SimpleSpan)> = vec![];
        let legacy_table = SymbolTable::default();
        let salsa_output = SalsaAnalysisOutput {
            diagnostics: vec![],
            symbol_table: SymbolTable::default(),
        };

        let result = SalsaShadow::compare(
            &legacy_errors,
            &legacy_warnings,
            &legacy_table,
            &salsa_output,
        );

        assert!(result.diagnostics_match);
        assert!(result.symbol_table_match);
        assert_eq!(result.legacy_error_count, 0);
        assert_eq!(result.salsa_error_count, 0);
    }

    #[test]
    fn test_comparison_divergent_errors() {
        let legacy_errors = vec![("error one".to_string(), SimpleSpan::from(0..5))];
        let legacy_warnings: Vec<(String, SimpleSpan)> = vec![];
        let legacy_table = SymbolTable::default();
        let salsa_output = SalsaAnalysisOutput {
            diagnostics: vec![PureDiagnostic {
                message: "different error".to_string(),
                span: SimpleSpan::from(0..5),
                severity: DiagnosticSeverity::Error,
            }],
            symbol_table: SymbolTable::default(),
        };

        let result = SalsaShadow::compare(
            &legacy_errors,
            &legacy_warnings,
            &legacy_table,
            &salsa_output,
        );

        assert!(!result.diagnostics_match);
        assert_eq!(result.legacy_error_count, 1);
        assert_eq!(result.salsa_error_count, 1);
    }

    #[test]
    fn test_salsa_shadow_function_analysis() {
        let mut shadow = SalsaShadow::new(AmberVersion::Alpha050);
        let file_id = FileId(100);

        shadow.update_file(
            file_id,
            "/test/main.ab",
            "fun greet(name) {\n    echo \"Hello, {name}\"\n}",
            1,
        );

        let output = shadow.analyze(&file_id).unwrap();
        let has_greet = output
            .symbol_table
            .symbols
            .iter()
            .any(|(_, info)| info.name == "greet");
        assert!(has_greet, "Should have 'greet' in symbol table");
    }

    #[test]
    fn test_full_analyze_generics() {
        let mut shadow = SalsaShadow::new(AmberVersion::Alpha050);
        let file_id = FileId(100);

        shadow.update_file(
            file_id,
            "/test/main.ab",
            "fun add(a, b) {\n    return a + b\n}",
            1,
        );

        let output = shadow
            .full_analyze(&file_id)
            .expect("full_analyze should return Some");
        assert!(
            !output.generics_snapshot.constraints.is_empty(),
            "Generic function should have constraints in snapshot"
        );
    }
}
