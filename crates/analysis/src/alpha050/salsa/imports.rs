//! Import resolution queries.
//!
//! Contains the Salsa-tracked `resolve_imports` query and pure helper
//! functions for resolving import paths.

use std::collections::{
    HashMap,
    HashSet,
};

use amber_grammar::Grammar;
use amber_types::AmberVersion;

use super::parse::parse_file;
use super::types::{
    FileIndex,
    SourceFile,
};
use super::Db;

// ---------------------------------------------------------------------------
// Salsa query
// ---------------------------------------------------------------------------

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

/// Walk the AST to extract import path strings and resolve them to canonical file paths.
///
/// Returns a map from the import path string as written in source (e.g. "std/math")
/// to the resolved canonical path (e.g. "std/math.ab").
pub(crate) fn build_ast_import_map(
    ast: &Grammar,
    source_path: &str,
    amber_version: &AmberVersion,
) -> HashMap<String, String> {
    use amber_grammar::alpha050::GlobalStatement;

    let mut map = HashMap::new();
    let stmts = match ast {
        Grammar::Alpha050(Some(stmts)) => stmts,
        _ => return map,
    };

    // Collect known stdlib paths for resolution
    let known_stdlib: HashSet<String> = HashSet::new();

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
