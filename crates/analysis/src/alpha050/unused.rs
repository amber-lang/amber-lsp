use std::collections::{
    HashMap,
    HashSet,
};

use chumsky::span::SimpleSpan;

use crate::files::{
    FileVersion,
    Files,
};
use crate::{
    Context,
    FunctionSymbol,
    ImportContext,
    SymbolLocation,
    SymbolTable,
    SymbolType,
};
use amber_types::paths::FileId;

/// Extract the `ImportContext` from a symbol's contexts, if present.
fn get_import_context(symbol_info: &crate::SymbolInfo) -> Option<&ImportContext> {
    symbol_info.contexts.iter().find_map(|c| match c {
        Context::Import(ctx) => Some(ctx),
        _ => None,
    })
}

/// A key that uniquely identifies a definition by name, file, and start offset.
type DefKey = (String, (FileId, FileVersion), usize);

fn def_key(name: &str, loc: &SymbolLocation) -> DefKey {
    (name.to_string(), loc.file, loc.start)
}

/// Precompute the set of definitions that are actually referenced
fn build_used_defs(symbol_table: &SymbolTable) -> HashSet<DefKey> {
    let mut used = HashSet::new();

    for (name, references) in &symbol_table.references {
        if let Some(defs) = symbol_table.definitions.get(name) {
            for reference in references {
                if let Some(resolved) = defs.get(&reference.start) {
                    used.insert(def_key(name, resolved));
                }
            }
        }
    }

    used
}

/// Info about one imported symbol collected during the first pass.
struct ImportedSymbol {
    name: String,
    /// The span of this individual symbol inside the import list.
    symbol_span: SimpleSpan,
    is_used: bool,
}

#[tracing::instrument(skip_all)]
pub fn check_unused_symbols(file_id: FileId, file_version: FileVersion, files: &Files) {
    let symbol_table = match files.symbol_table.get(&(file_id, file_version)) {
        Some(st) => st.clone(),
        None => return,
    };

    let file = (file_id, file_version);
    let used_defs = build_used_defs(&symbol_table);

    // ── Pass 1: collect import symbols grouped by statement span ──────────
    // Key: statement span start (unique per import statement).
    // Value: (statement_span, vec of imported symbols).
    let mut import_statements: HashMap<usize, (SimpleSpan, Vec<ImportedSymbol>)> = HashMap::new();

    for (range, symbol_info) in symbol_table.symbols.iter() {
        if symbol_info.is_definition {
            continue;
        }

        let import_ctx = match get_import_context(symbol_info) {
            Some(ctx) => ctx,
            None => continue,
        };

        let is_public_import = symbol_table
            .public_definitions
            .contains_key(&symbol_info.name);

        if is_public_import {
            continue;
        }

        let import_def = symbol_table
            .definitions
            .get(&symbol_info.name)
            .and_then(|defs| defs.get(range.start()));

        let is_used = match &import_def {
            Some(def_loc) => used_defs.contains(&def_key(&symbol_info.name, def_loc)),
            None => true,
        };

        let symbol_span = SimpleSpan::from(*range.start()..*range.end());

        if let Some(stmt_span) = import_ctx.statement_span {
            import_statements
                .entry(stmt_span.start)
                .or_insert_with(|| (stmt_span, Vec::new()))
                .1
                .push(ImportedSymbol {
                    name: symbol_info.name.clone(),
                    symbol_span,
                    is_used,
                });
        } else if !is_used {
            // No statement span (e.g. builtin import) — flag individually.
            files.report_unused(
                &file,
                &format!("Unused import \"{}\"", symbol_info.name),
                symbol_span,
            );
        }
    }

    // ── Pass 2: report unused imports (import { ... }) ─────────────────
    for (stmt_span, symbols) in import_statements.values() {
        let all_unused = symbols.iter().all(|s| !s.is_used);

        if all_unused {
            // Every symbol in the statement is unused → flag the whole statement.
            files.report_unused(&file, "Unused import", *stmt_span);
        } else {
            // Only some symbols are unused → flag them individually.
            for sym in symbols.iter().filter(|s| !s.is_used) {
                files.report_unused(
                    &file,
                    &format!("Unused import \"{}\"", sym.name),
                    sym.symbol_span,
                );
            }
        }
    }

    // ── Pass 3: report unused import * statements ────────────────────────
    for (stmt_span, imported_symbols) in &symbol_table.import_all_statements {
        let all_unused = imported_symbols
            .iter()
            .all(|(name, def_loc)| !used_defs.contains(&def_key(name, def_loc)));

        if all_unused && !imported_symbols.is_empty() {
            files.report_unused(&file, "Unused import", *stmt_span);
        }
    }

    // ── Check variables and functions ─────────────────────────────────────
    for (_, symbol_info) in symbol_table.symbols.iter() {
        if !symbol_info.is_definition {
            continue;
        }

        let def_location = SymbolLocation {
            file,
            start: symbol_info.span.start,
            end: symbol_info.span.end,
        };

        match &symbol_info.symbol_type {
            SymbolType::Variable(_) => {
                if !used_defs.contains(&def_key(&symbol_info.name, &def_location)) {
                    files.report_unused(
                        &file,
                        &format!("Unused variable \"{}\"", symbol_info.name),
                        symbol_info.span,
                    );
                }
            }
            SymbolType::Function(FunctionSymbol { is_public, .. }) => {
                if !is_public && !used_defs.contains(&def_key(&symbol_info.name, &def_location)) {
                    files.report_unused(
                        &file,
                        &format!("Unused function \"{}\"", symbol_info.name),
                        symbol_info.span,
                    );
                }
            }
            _ => {}
        }
    }
}
