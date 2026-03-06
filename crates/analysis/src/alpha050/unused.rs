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

/// Check whether a specific definition (identified by its `SymbolLocation`) has
/// any reference or function-call usage that resolves back to it.
///
/// For references we look up which definition the reference resolves to at its
/// position in the `definitions` range map, and compare against `def_location`.
///
/// For function calls (non-definition `Function` symbols that are not imports)
/// we do the same positional lookup.
fn is_definition_referenced(
    name: &str,
    def_location: &SymbolLocation,
    symbol_table: &SymbolTable,
) -> bool {
    let definitions_map = symbol_table.definitions.get(name);

    // Check explicit references (variable reads, etc.)
    if let Some(references) = symbol_table.references.get(name) {
        for reference in references {
            if let Some(defs) = &definitions_map {
                if let Some(resolved) = defs.get(&reference.start) {
                    if resolved.start == def_location.start && resolved.file == def_location.file {
                        return true;
                    }
                }
            }
        }
    }

    // Check function call-site symbols (non-definition, non-import Function entries)
    for (_, sym) in symbol_table.symbols.iter() {
        if sym.is_definition
            || !matches!(sym.symbol_type, SymbolType::Function(_))
            || has_import_context(sym)
        {
            continue;
        }
        if sym.name != name {
            continue;
        }
        if let Some(defs) = &definitions_map {
            if let Some(resolved) = defs.get(&sym.span.start) {
                if resolved.start == def_location.start && resolved.file == def_location.file {
                    return true;
                }
            }
        }
    }

    false
}

#[tracing::instrument(skip_all)]
pub fn check_unused_symbols(file_id: FileId, file_version: FileVersion, files: &Files) {
    let symbol_table = match files.symbol_table.get(&(file_id, file_version)) {
        Some(st) => st.clone(),
        None => return,
    };

    let file = (file_id, file_version);

    for (range, symbol_info) in symbol_table.symbols.iter() {
        // Check for unused named imports (non-public, non-definition symbols
        // that were introduced by an import statement).
        if !symbol_info.is_definition && has_import_context(symbol_info) {
            let is_public_import = symbol_table
                .public_definitions
                .contains_key(&symbol_info.name);

            // For imports, the definition they introduce is looked up
            // via the import-site span in the definitions map.
            let import_def = symbol_table
                .definitions
                .get(&symbol_info.name)
                .and_then(|defs| defs.get(range.start()));

            let is_used = match &import_def {
                Some(def_loc) => {
                    is_definition_referenced(&symbol_info.name, def_loc, &symbol_table)
                }
                // If we can't resolve the definition, fall back to not flagging.
                None => true,
            };

            if !is_public_import && !is_used {
                // Use the range key (import-site span) rather than
                // symbol_info.span which points into the source file.
                let import_span = SimpleSpan::from(*range.start()..*range.end());
                files.report_unused(
                    &file,
                    &format!("Unused import \"{}\"", symbol_info.name),
                    import_span,
                );
            }

            continue;
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
                if !is_definition_referenced(&symbol_info.name, &def_location, &symbol_table) {
                    files.report_unused(
                        &file,
                        &format!("Unused variable \"{}\"", symbol_info.name),
                        symbol_info.span,
                    );
                }
            }
            SymbolType::Function(FunctionSymbol { is_public, .. }) => {
                if !is_public
                    && !is_definition_referenced(&symbol_info.name, &def_location, &symbol_table)
                {
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
