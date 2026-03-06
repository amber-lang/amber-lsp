use std::collections::HashSet;

use chumsky::span::SimpleSpan;

use crate::files::{
    FileVersion,
    Files,
};
use crate::{
    Context,
    FunctionSymbol,
    SymbolLocation,
    SymbolTable,
    SymbolType,
};
use amber_types::paths::FileId;

/// Check if a symbol has an `Import` context, indicating it was brought in
/// by an `import { ... } from "..."` statement.
fn has_import_context(symbol_info: &crate::SymbolInfo) -> bool {
    symbol_info
        .contexts
        .iter()
        .any(|c| matches!(c, Context::Import(_)))
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

#[tracing::instrument(skip_all)]
pub fn check_unused_symbols(file_id: FileId, file_version: FileVersion, files: &Files) {
    let symbol_table = match files.symbol_table.get(&(file_id, file_version)) {
        Some(st) => st.clone(),
        None => return,
    };

    let file = (file_id, file_version);
    let used_defs = build_used_defs(&symbol_table);

    for (range, symbol_info) in symbol_table.symbols.iter() {
        // Check for unused named imports (non-public, non-definition symbols
        // that were introduced by an import statement).
        if !symbol_info.is_definition && has_import_context(symbol_info) {
            let is_public_import = symbol_table
                .public_definitions
                .contains_key(&symbol_info.name);

            if is_public_import {
                continue;
            }

            // For imports, the definition they introduce is looked up
            // via the import-site span in the definitions map.
            let import_def = symbol_table
                .definitions
                .get(&symbol_info.name)
                .and_then(|defs| defs.get(range.start()));

            let is_used = match &import_def {
                Some(def_loc) => used_defs.contains(&def_key(&symbol_info.name, def_loc)),
                // If we can't resolve the definition, fall back to not flagging.
                None => true,
            };

            if is_used {
                continue;
            }

            files.report_unused(
                &file,
                &format!("Unused import \"{}\"", symbol_info.name),
                SimpleSpan::from(*range.start()..*range.end()),
            );
        }

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
