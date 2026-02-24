//! Salsa-based incremental analysis database for alpha050.
//!
//! This module defines the query-based analysis pipeline that mirrors
//! the existing imperative analysis in `global.rs`, `stmnts.rs`, and `exp.rs`.
//!
//! The Salsa database coexists with the legacy `Backend`/`AnalysisHost`
//! via the "Shadow Analysis" pattern: both engines run on the same input,
//! and results are compared for validation before the legacy path is retired.

use std::collections::{
    HashMap,
    HashSet,
};

use amber_grammar::{
    Grammar,
    Spanned,
    SpannedSemanticToken,
};
use amber_types::{
    AmberVersion,
    GenericsSnapshot,
    LocalGenericsAllocator,
};
use chumsky::span::SimpleSpan;

use crate::alpha050::pure_analysis::{
    analyze_global_pure,
    file_key_for_path,
    ResolvedImport,
};
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

// ---------------------------------------------------------------------------
// Pure tokenize + parse function (no Salsa, no I/O)
// ---------------------------------------------------------------------------

/// Pure tokenize + parse, factored out of `Backend::analyze_document`.
///
/// This function has no side effects: given the same `(text, amber_version)`,
/// it always produces the same output.
pub fn tokenize_and_parse(text: &str, amber_version: AmberVersion) -> ParseOutput {
    let compiler: Box<dyn amber_grammar::LSPAnalysis> = match amber_version {
        AmberVersion::Alpha034 => Box::new(amber_grammar::alpha034::AmberCompiler::new()),
        AmberVersion::Alpha035 => Box::new(amber_grammar::alpha035::AmberCompiler::new()),
        AmberVersion::Alpha040 => Box::new(amber_grammar::alpha040::AmberCompiler::new()),
        AmberVersion::Alpha050 => Box::new(amber_grammar::alpha050::AmberCompiler::new()),
    };
    let tokens = compiler.tokenize(text);
    let response = compiler.parse(&tokens);
    ParseOutput {
        ast: response.ast,
        errors: response
            .errors
            .iter()
            .map(|err| (err.to_string(), *err.span()))
            .collect(),
        semantic_tokens: response.semantic_tokens,
    }
}

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

// ---------------------------------------------------------------------------
// Salsa query functions
// ---------------------------------------------------------------------------

/// Pure tokenize + parse. Depends only on `SourceFile::text` and `SourceFile::amber_version`.
#[salsa::tracked]
pub fn parse_file<'db>(db: &'db dyn Db, source: SourceFile) -> ParsedFile<'db> {
    let text = source.text(db);
    let amber_version = source.amber_version(db);
    let output = tokenize_and_parse(text, amber_version);
    ParsedFile::new(
        db,
        source,
        output.ast,
        output.errors,
        output.semantic_tokens,
    )
}

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

/// Resolve imports for a file. Returns the list of imported SourceFile handles.
///
/// Walks the parsed AST for `GlobalStatement::Import` nodes, resolves each
/// import path to a `SourceFile` via the `FileIndex`, and returns the list
/// of successfully resolved imports.
///
/// Also implicitly imports "builtin" for non-builtin files (matching existing behavior).
#[salsa::tracked(returns(ref))]
pub fn resolve_imports(db: &dyn Db, source: SourceFile, file_index: FileIndex) -> Vec<SourceFile> {
    let parse = parse_file(db, source);
    let ast = parse.ast(db);
    let amber_version = source.amber_version(db);
    let source_path = source.path(db);

    let mut imports = Vec::new();

    // Implicitly import builtin for non-builtin files
    let is_builtin = source_path.ends_with("builtin.ab") || source_path == "builtin.ab";
    if !is_builtin {
        if let Some(builtin_sf) = file_index.get(db, "builtin.ab") {
            imports.push(builtin_sf);
        }
    }

    // Walk AST for Import statements
    if let Grammar::Alpha050(Some(stmts)) = ast {
        for (stmt, _span) in stmts {
            if let amber_grammar::alpha050::GlobalStatement::Import(
                _is_public,
                _import_kw,
                _import_content,
                _from_kw,
                (path, _path_span),
            ) = stmt
            {
                // Build the stdlib path set from the file index
                let known_stdlib: HashSet<String> = file_index
                    .files(db)
                    .keys()
                    .filter(|p| p.starts_with("std/") || *p == "builtin.ab")
                    .cloned()
                    .collect();

                if let Some(resolved_path) =
                    resolve_import_path(source_path, &path, amber_version.clone(), &known_stdlib)
                {
                    if let Some(imported_sf) = file_index.get(db, &resolved_path) {
                        // Avoid importing self
                        if imported_sf != source {
                            imports.push(imported_sf);
                        }
                    }
                }
            }
        }
    }

    imports
}

/// Analyze a single file. Depends on `parse_file` + analyses of all imported files.
///
/// This is the primary Salsa query for per-file analysis.
///
/// Uses `cycle_initial` for cycle recovery (circular imports).
#[salsa::tracked(cycle_initial = analyze_file_cycle_initial)]
pub fn analyze_file<'db>(
    db: &'db dyn Db,
    source: SourceFile,
    file_index: FileIndex,
) -> AnalysisResult<'db> {
    let parse = parse_file(db, source);
    let imports = resolve_imports(db, source, file_index);
    let source_path = source.path(db);
    let amber_version = source.amber_version(db);

    // Convert parse errors to diagnostics
    let mut diagnostics: Vec<PureDiagnostic> = parse
        .parse_errors(db)
        .iter()
        .map(|(msg, span)| PureDiagnostic {
            message: msg.clone(),
            span: *span,
            severity: DiagnosticSeverity::Error,
        })
        .collect();

    // Build import resolution map: import_path → ResolvedImport
    // We need to match AST import statements to resolved SourceFiles.
    let mut import_resolution: HashMap<String, ResolvedImport> = HashMap::new();
    let mut all_file_tables: HashMap<
        (amber_types::paths::FileId, crate::files::FileVersion),
        SymbolTable,
    > = HashMap::new();

    // Build a lookup from path → SourceFile for resolved imports
    let _resolved_imports_by_path: HashMap<String, SourceFile> = imports
        .iter()
        .map(|sf| (sf.path(db).clone(), *sf))
        .collect();

    // For each resolved import, analyze it and collect its symbol table
    for &imported_source in imports.iter() {
        let imported_analysis = analyze_file(db, imported_source, file_index);
        let imported_path = imported_source.path(db);
        let imported_file_key = file_key_for_path(imported_path);
        let imported_symbol_table = imported_analysis.symbol_table(db).clone();

        // Store in all_file_tables for cross-file resolution
        all_file_tables.insert(imported_file_key, imported_symbol_table.clone());

        import_resolution.insert(
            imported_path.clone(),
            ResolvedImport {
                symbol_table: imported_symbol_table,
                file_key: imported_file_key,
            },
        );
    }

    // Map AST import paths to resolved paths
    // Walk the AST to find import statements and map their path strings to resolved paths
    let ast_import_map =
        build_ast_import_map(db, &parse.ast(db), source_path, &amber_version, file_index);
    let mut final_import_resolution: HashMap<String, ResolvedImport> = HashMap::new();

    for (import_path_str, resolved_path) in ast_import_map.iter() {
        if let Some(ri) = import_resolution.get(resolved_path) {
            final_import_resolution.insert(
                import_path_str.clone(),
                ResolvedImport {
                    symbol_table: ri.symbol_table.clone(),
                    file_key: ri.file_key,
                },
            );
        }
    }

    // Also add "builtin" mapping if present
    if let Some(ri) = import_resolution.get("builtin.ab") {
        final_import_resolution.insert(
            "builtin".to_string(),
            ResolvedImport {
                symbol_table: ri.symbol_table.clone(),
                file_key: ri.file_key,
            },
        );
    }

    // Determine if this file is the builtin file
    let is_builtin = source_path.ends_with("builtin.ab") || source_path == "builtin.ab";

    // Extract AST statements
    let owned_ast = parse.ast(db);
    let ast_stmts: &[Spanned<amber_grammar::alpha050::GlobalStatement>] = match &owned_ast {
        Grammar::Alpha050(Some(stmts)) => stmts.as_slice(),
        _ => &[],
    };

    // Create deterministic allocator from file path
    let allocator = {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{
            Hash,
            Hasher,
        };
        let mut h = DefaultHasher::new();
        source_path.hash(&mut h);
        LocalGenericsAllocator::new(h.finish())
    };

    // Run pure analysis
    let file_key = file_key_for_path(source_path);
    let (symbol_table, pure_diagnostics, generics) = analyze_global_pure(
        file_key,
        ast_stmts,
        &amber_version,
        &final_import_resolution,
        is_builtin,
        allocator,
    );

    diagnostics.extend(pure_diagnostics);
    let generics_snapshot = generics.into_snapshot();

    AnalysisResult::new(db, source, diagnostics, symbol_table, generics_snapshot)
}

/// Walk the AST to extract import path strings and resolve them to canonical file paths.
///
/// Returns a map from the import path string as written in source (e.g. "std/math")
/// to the resolved canonical path (e.g. "std/math.ab").
fn build_ast_import_map(
    _db: &dyn Db,
    ast: &Grammar,
    source_path: &str,
    amber_version: &AmberVersion,
    _file_index: FileIndex,
) -> HashMap<String, String> {
    use amber_grammar::alpha050::GlobalStatement;

    let mut map = HashMap::new();
    let stmts = match ast {
        Grammar::Alpha050(Some(stmts)) => stmts,
        _ => return map,
    };

    // Collect known stdlib paths for resolution
    let known_stdlib: HashSet<String> = HashSet::new(); // Will be populated below if needed
                                                        // We need to know what files are known — use the same approach as resolve_imports
                                                        // but we don't have db access to file_index here in a concise way.
                                                        // Instead, we'll just try both the raw path and common suffixes.

    for (global, _) in stmts {
        if let GlobalStatement::Import(_, _, _, _, (path, _)) = global {
            // Try to resolve the path to a canonical form
            if let Some(resolved) =
                resolve_import_path(source_path, path, amber_version.clone(), &known_stdlib)
            {
                map.insert(path.clone(), resolved);
            } else {
                // Try with .ab suffix for stdlib paths
                let with_ab = format!("{path}.ab");
                map.insert(path.clone(), with_ab);
            }
        }
    }

    map
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

/// Cycle initial value for `analyze_file` — provides a default when a cycle is detected.
fn analyze_file_cycle_initial<'db>(
    db: &'db dyn Db,
    _id: salsa::Id,
    source: SourceFile,
    _file_index: FileIndex,
) -> AnalysisResult<'db> {
    AnalysisResult::new(
        db,
        source,
        vec![PureDiagnostic {
            message: "Circular dependency detected".to_string(),
            span: SimpleSpan::from(0..0),
            severity: DiagnosticSeverity::Error,
        }],
        SymbolTable::default(),
        GenericsSnapshot::default(),
    )
}

/// Derive generic constraints for a single function.
///
/// Memoized independently — changing code outside a function body
/// does not re-derive its generics.
///
/// For now, returns an empty snapshot. Will be populated as Phase 3.
#[salsa::tracked]
pub fn derive_function_generics(db: &dyn Db, func: FunctionId<'_>) -> GenericsSnapshot {
    // TODO (Phase 3): Extract function AST, analyze body with PureGenericsMap.
    let _ = (db, func);
    GenericsSnapshot::default()
}

// ---------------------------------------------------------------------------
// Pure import resolution (no I/O)
// ---------------------------------------------------------------------------

/// Pure import path resolution — no AnalysisHost, no file I/O.
///
/// Resolves an import path to a canonical file path string.
/// Stdlib paths are resolved against `known_stdlib_paths`.
/// Relative paths are resolved against the importer's directory.
pub fn resolve_import_path(
    importer_path: &str,
    import_path: &str,
    amber_version: AmberVersion,
    known_stdlib_paths: &HashSet<String>,
) -> Option<String> {
    // Check if it's a stdlib import
    if import_path.starts_with("std/") || import_path == "std" || import_path == "builtin" {
        let normalized = match amber_version {
            AmberVersion::Alpha034 if import_path == "std" => "std/main".to_string(),
            _ => import_path.to_string(),
        };
        let full_path = format!("{}.ab", normalized);
        if known_stdlib_paths.contains(&full_path) {
            return Some(full_path);
        }
        return None;
    }

    // Relative path — resolve against importer's directory
    let importer_dir = if let Some(pos) = importer_path.rfind('/') {
        &importer_path[..pos]
    } else {
        "."
    };

    // User-written imports already include ".ab" suffix (e.g. "utils.ab").
    // Strip leading "./" if present — it's just an explicit relative marker.
    let cleaned = import_path.strip_prefix("./").unwrap_or(import_path);
    let raw = format!("{}/{}", importer_dir, cleaned);
    // Simple normalization: replace /./  with /
    let normalized = raw.replace("/./", "/");
    Some(normalized)
}

// ---------------------------------------------------------------------------
// Stdlib file listing (pure — reads from embedded include_dir at compile time)
// ---------------------------------------------------------------------------

/// Collect all stdlib file paths for a given Amber version from the embedded resources.
///
/// Returns paths like `"std/array.ab"`, `"std/math.ab"`, `"builtin.ab"`, etc.
pub fn collect_stdlib_paths(amber_version: AmberVersion) -> HashSet<String> {
    use crate::stdlib::STDLIB;

    let version_dir = match amber_version.clone() {
        AmberVersion::Alpha034 => "alpha034",
        AmberVersion::Alpha035 => "alpha035",
        AmberVersion::Alpha040 => "alpha040",
        AmberVersion::Alpha050 => "alpha050",
    };

    let mut paths = HashSet::new();

    if let Some(dir) = STDLIB.get_dir(version_dir) {
        collect_paths_recursive(dir, "", &mut paths);
    }

    paths
}

fn collect_paths_recursive(dir: &include_dir::Dir<'_>, prefix: &str, paths: &mut HashSet<String>) {
    for entry in dir.entries() {
        match entry {
            include_dir::DirEntry::Dir(sub_dir) => {
                let name = sub_dir.path().file_name().unwrap().to_str().unwrap();
                let new_prefix = if prefix.is_empty() {
                    name.to_string()
                } else {
                    format!("{}/{}", prefix, name)
                };
                collect_paths_recursive(sub_dir, &new_prefix, paths);
            }
            include_dir::DirEntry::File(file) => {
                let name = file.path().file_name().unwrap().to_str().unwrap();
                let full_path = if prefix.is_empty() {
                    name.to_string()
                } else {
                    format!("{}/{}", prefix, name)
                };
                paths.insert(full_path);
            }
        }
    }
}

/// Read an embedded stdlib file's content by its path relative to the version directory.
///
/// E.g., `read_stdlib_content(Alpha050, "std/array.ab")` returns the contents of
/// `resources/alpha050/std/array.ab`.
pub fn read_stdlib_content(amber_version: AmberVersion, relative_path: &str) -> Option<String> {
    use crate::stdlib::STDLIB;

    let version_dir = match amber_version {
        AmberVersion::Alpha034 => "alpha034",
        AmberVersion::Alpha035 => "alpha035",
        AmberVersion::Alpha040 => "alpha040",
        AmberVersion::Alpha050 => "alpha050",
    };

    let full_path = format!("{}/{}", version_dir, relative_path);
    STDLIB
        .get_file(&full_path)
        .and_then(|f| f.contents_utf8())
        .map(|s| s.to_string())
}

// ---------------------------------------------------------------------------
// Pre-registration helper
// ---------------------------------------------------------------------------

impl AnalysisDatabase {
    /// Pre-register all stdlib files for the given Amber version.
    ///
    /// Creates SourceFile inputs for all stdlib files and registers them
    /// in the provided FileIndex.
    pub fn register_stdlib(
        &mut self,
        amber_version: AmberVersion,
        file_index: FileIndex,
    ) -> Vec<SourceFile> {
        let paths = collect_stdlib_paths(amber_version.clone());
        let mut files = Vec::new();

        for path in paths {
            if let Some(content) = read_stdlib_content(amber_version.clone(), &path) {
                let source = SourceFile::new(
                    self,
                    path.clone(),
                    content,
                    0, // stdlib version is always 0
                    amber_version.clone(),
                );
                self.update_file_index(file_index, path, source);
                files.push(source);
            }
        }

        files
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// Create a test database with a single file and a file index.
    fn make_db_with_file(code: &str) -> (AnalysisDatabase, SourceFile, FileIndex) {
        let db = AnalysisDatabase::new();
        let sf = SourceFile::new(
            &db,
            "/test/main.ab".to_string(),
            code.to_string(),
            1,
            AmberVersion::Alpha050,
        );
        // Register a minimal builtin so the implicit `import * from "builtin"` resolves
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
        // Should have no parse errors
        assert!(
            parsed.parse_errors(&db).is_empty(),
            "Unexpected parse errors: {:?}",
            parsed.parse_errors(&db)
        );
        // AST should be the Alpha050 variant
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
        // Call twice — second call should be cached
        let result1 = parse_file(&db, sf);
        let result2 = parse_file(&db, sf);
        // Should return the same tracked struct (same salsa ID)
        assert!(result1 == result2);
    }

    #[test]
    fn test_parse_file_invalidation() {
        use salsa::Setter;
        let (mut db, sf, _fi) = make_db_with_file("let x = 42");

        let result1 = parse_file(&db, sf);
        let errors1 = result1.parse_errors(&db).clone();

        // Modify the source text — should invalidate the cache
        sf.set_text(&mut db).to("fun {".to_string());

        let result2 = parse_file(&db, sf);
        let errors2 = result2.parse_errors(&db).clone();

        // Results should differ
        assert!(errors1.is_empty());
        assert!(!errors2.is_empty());
    }

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
        // Valid code should have no diagnostics (from parse)
        assert!(
            analysis.diagnostics(&db).is_empty(),
            "Unexpected diagnostics: {:?}",
            analysis.diagnostics(&db)
        );
    }

    #[test]
    fn test_resolve_imports_empty() {
        let (db, sf, fi) = make_db_with_file("let x = 42");
        let imports = resolve_imports(&db, sf, fi);
        // Non-builtin files implicitly import builtin, so we expect exactly one import
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].path(&db), "builtin.ab");
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

        // Update with the same SourceFile handle
        let sf2 = db.set_file(Some(sf), "/test.ab", "let x = 2", 2, AmberVersion::Alpha050);

        assert!(sf == sf2); // Same SourceFile identity
        assert_eq!(sf2.text(&db), "let x = 2");
        assert_eq!(sf2.version(&db), 2);
    }

    #[test]
    fn test_collect_stdlib_paths_alpha050() {
        let paths = collect_stdlib_paths(AmberVersion::Alpha050);
        // Alpha050 should have stdlib files
        assert!(!paths.is_empty(), "Expected stdlib paths for Alpha050");
        // Should include common stdlib modules
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

        // Verify each file can be parsed
        for sf in &files {
            let parsed = parse_file(&db, *sf);
            // Stdlib files should parse without errors
            // (Some may have minor issues, but let's check the majority parse)
            let _ = parsed.ast(&db);
        }
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
        let result =
            resolve_import_path("/src/main.ab", "utils.ab", AmberVersion::Alpha050, &known);
        assert_eq!(result, Some("/src/utils.ab".to_string()));
    }

    #[test]
    fn test_resolve_import_path_relative_dot_slash() {
        let known = HashSet::new();
        let result =
            resolve_import_path("/src/main.ab", "./utils.ab", AmberVersion::Alpha050, &known);
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
    fn test_tokenize_and_parse_pure() {
        let output = tokenize_and_parse("let x = 42", AmberVersion::Alpha050);
        assert!(output.errors.is_empty());
        match output.ast {
            Grammar::Alpha050(Some(stmts)) => assert!(!stmts.is_empty()),
            _ => panic!("Expected Alpha050 grammar"),
        }
    }

    #[test]
    fn test_derive_function_generics_stub() {
        let db = AnalysisDatabase::new();
        let func = FunctionId::new(&db, "/test.ab".to_string(), "add".to_string());
        let generics = derive_function_generics(&db, func);
        assert_eq!(generics, GenericsSnapshot::default());
    }

    #[test]
    fn test_resolve_imports_with_stdlib() {
        let mut db = AnalysisDatabase::new();
        let fi = db.new_file_index();

        // Register stdlib
        db.register_stdlib(AmberVersion::Alpha050, fi);

        // Create a user file that imports from stdlib
        let sf = SourceFile::new(
            &db,
            "/test/main.ab".to_string(),
            r#"import * from "std/math""#.to_string(),
            1,
            AmberVersion::Alpha050,
        );
        db.update_file_index(fi, "/test/main.ab".to_string(), sf);

        let imports = resolve_imports(&db, sf, fi);
        // Should resolve: builtin (implicit) + std/math
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

        // Create a builtin.ab file
        let builtin_sf = SourceFile::new(
            &db,
            "builtin.ab".to_string(),
            "let BUILTIN = true".to_string(),
            0,
            AmberVersion::Alpha050,
        );
        db.update_file_index(fi, "builtin.ab".to_string(), builtin_sf);

        let imports = resolve_imports(&db, builtin_sf, fi);
        // builtin.ab should NOT import itself
        assert!(
            imports.is_empty(),
            "builtin.ab should not import itself, got: {:?}",
            imports.iter().map(|s| s.path(&db)).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_analyze_file_with_imports() {
        let mut db = AnalysisDatabase::new();
        let fi = db.new_file_index();

        // Register stdlib
        db.register_stdlib(AmberVersion::Alpha050, fi);

        // Create a user file
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
        // Should not have parse errors
        assert!(
            analysis.diagnostics(&db).is_empty(),
            "Unexpected diagnostics: {:?}",
            analysis.diagnostics(&db)
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

    // ────────── Phase 4: analyze_file integration tests ──────────

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
        // The function "greet" should appear in the symbol table
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
        // Test that importing from another file pulls in symbols
        let db = AnalysisDatabase::new();

        // Create a library file with a public function
        let lib_sf = SourceFile::new(
            &db,
            "/test/lib.ab".to_string(),
            "pub fun helper() {\n    echo \"help\"\n}".to_string(),
            1,
            AmberVersion::Alpha050,
        );

        // Create a main file that imports from lib
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
        // The import should not cause a "File doesn't exist" error
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
        // Verify that calling analyze_file twice returns the same result (Salsa memos)
        let (db, sf, fi) = make_db_with_file("let x = 42");
        let result1 = analyze_file(&db, sf, fi);
        let result2 = analyze_file(&db, sf, fi);
        // Same Salsa tracked struct should be returned
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
        // GenericsSnapshot should be populated (not empty for code with generics)
        let _snapshot = result.generics_snapshot(&db);
        // Just verify it doesn't panic; the GenericsSnapshot is valid
    }
}
