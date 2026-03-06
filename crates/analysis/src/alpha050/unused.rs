use std::collections::HashSet;

use chumsky::span::SimpleSpan;

use crate::files::{
    FileVersion,
    Files,
};
use crate::{
    Context,
    FunctionSymbol,
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

#[tracing::instrument(skip_all)]
pub fn check_unused_symbols(file_id: FileId, file_version: FileVersion, files: &Files) {
    let symbol_table = match files.symbol_table.get(&(file_id, file_version)) {
        Some(st) => st.clone(),
        None => return,
    };

    // Collect names that have at least one reference
    let referenced_names: HashSet<&String> = symbol_table.references.keys().collect();

    // Collect function names that appear as non-definition symbols (function calls).
    // Exclude imported symbols — they are also non-definition functions but represent
    // the import itself, not an actual call site.
    let mut called_functions: HashSet<String> = HashSet::new();
    for (_, symbol_info) in symbol_table.symbols.iter() {
        if !symbol_info.is_definition
            && matches!(symbol_info.symbol_type, SymbolType::Function(_))
            && !has_import_context(symbol_info)
        {
            called_functions.insert(symbol_info.name.clone());
        }
    }

    let file = (file_id, file_version);

    for (range, symbol_info) in symbol_table.symbols.iter() {
        // Check for unused named imports (non-public, non-definition symbols
        // that were introduced by an import statement).
        if !symbol_info.is_definition && has_import_context(symbol_info) {
            let is_public_import = symbol_table
                .public_definitions
                .contains_key(&symbol_info.name);

            if !is_public_import
                && !referenced_names.contains(&symbol_info.name)
                && !called_functions.contains(&symbol_info.name)
            {
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

        match &symbol_info.symbol_type {
            SymbolType::Variable(_) => {
                if !referenced_names.contains(&symbol_info.name) {
                    files.report_unused(
                        &file,
                        &format!("Unused variable \"{}\"", symbol_info.name),
                        symbol_info.span,
                    );
                }
            }
            SymbolType::Function(FunctionSymbol { is_public, .. }) => {
                if !is_public
                    && !referenced_names.contains(&symbol_info.name)
                    && !called_functions.contains(&symbol_info.name)
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
