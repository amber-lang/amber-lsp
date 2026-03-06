//! File analysis query with import resolution.
//!
//! Contains the main `analyze_file` Salsa query, the `build_import_resolution`
//! memoization point, and cycle recovery logic.

use std::collections::HashMap;

use amber_grammar::{
    Grammar,
    Spanned,
};
use amber_types::{
    GenericsSnapshot,
    LocalGenericsAllocator,
};

use super::imports::{
    build_ast_import_map,
    resolve_imports,
};
use super::parse::parse_file;
use super::types::{
    AnalysisResult,
    DiagnosticSeverity,
    FileIndex,
    PureDiagnostic,
    SourceFile,
};
use super::Db;
use crate::alpha050::pure_analysis::{
    analyze_global_pure,
    file_key_for_path,
    ResolvedImport,
};
use crate::SymbolTable;

// ---------------------------------------------------------------------------
// Import resolution memoization
// ---------------------------------------------------------------------------

/// Collected import tables — the result of resolving and analyzing all imports.
///
/// This tracked struct provides a memoization boundary: if a file's own text
/// changes but its imports haven't changed (or their symbol tables are stable),
/// the import resolution map is reused without re-collecting.
#[salsa::tracked(debug)]
pub struct ImportResolution<'db> {
    /// Map from import path string → (symbol_table, file_key).
    /// Uses Vec of pairs because HashMap is not salsa-friendly.
    #[tracked]
    pub entries: Vec<(String, ResolvedImport)>,
}

/// Build the import resolution map for a file.
///
/// Resolves all imports, recursively analyzes each imported file, and collects
/// their symbol tables. This is a separate memoization point from `analyze_file`
/// so that if an imported file is re-analyzed but produces the same symbol table,
/// the downstream `analyze_file` call is not invalidated.
#[salsa::tracked]
pub fn build_import_resolution<'db>(
    db: &'db dyn Db,
    source: SourceFile,
    file_index: FileIndex,
) -> ImportResolution<'db> {
    let parse = parse_file(db, source);
    let imports = resolve_imports(db, source, file_index);
    let source_path = source.path(db);
    let amber_version = source.amber_version(db);

    let mut import_resolution: HashMap<String, ResolvedImport> = HashMap::new();

    // For each resolved import, analyze it and collect its symbol table
    for &imported_source in imports.iter() {
        let imported_analysis = analyze_file(db, imported_source, file_index);
        let imported_path = imported_source.path(db);
        let imported_file_key = file_key_for_path(imported_path);
        let imported_symbol_table = imported_analysis.symbol_table(db).clone();

        import_resolution.insert(
            imported_path.clone(),
            ResolvedImport {
                symbol_table: imported_symbol_table,
                file_key: imported_file_key,
            },
        );
    }

    // Map AST import paths to resolved paths
    let ast_import_map = build_ast_import_map(&parse.ast(db), source_path, &amber_version);
    let mut entries: Vec<(String, ResolvedImport)> = Vec::new();

    for (import_path_str, resolved_path) in ast_import_map.iter() {
        if let Some(ri) = import_resolution.get(resolved_path) {
            entries.push((
                import_path_str.clone(),
                ResolvedImport {
                    symbol_table: ri.symbol_table.clone(),
                    file_key: ri.file_key,
                },
            ));
        }
    }

    // Also add "builtin" mapping if present
    if let Some(ri) = import_resolution.get("builtin.ab") {
        entries.push((
            "builtin".to_string(),
            ResolvedImport {
                symbol_table: ri.symbol_table.clone(),
                file_key: ri.file_key,
            },
        ));
    }

    ImportResolution::new(db, entries)
}

// ---------------------------------------------------------------------------
// Main analysis query
// ---------------------------------------------------------------------------

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

    // Use the memoized import resolution
    let import_resolution = build_import_resolution(db, source, file_index);
    let final_import_resolution: HashMap<String, ResolvedImport> =
        import_resolution.entries(db).iter().cloned().collect();

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

// ---------------------------------------------------------------------------
// Cycle recovery
// ---------------------------------------------------------------------------

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
            span: chumsky::span::SimpleSpan::from(0..0),
            severity: DiagnosticSeverity::Error,
        }],
        SymbolTable::default(),
        GenericsSnapshot::default(),
    )
}
