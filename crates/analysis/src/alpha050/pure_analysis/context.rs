//! Pure analysis context — the mutable state carrier for analysis.
//!
//! `PureContext` replaces `Files` + `Backend` in the legacy pipeline.
//! All mutable state is owned and local. No DashMaps, no async, no I/O.

use std::collections::HashMap;

use amber_grammar::Span;
use amber_types::paths::FileId;
use amber_types::{
    DataType,
    PureGenericsMap,
};

use crate::alpha050::salsa::types::{
    DiagnosticSeverity,
    PureDiagnostic,
};
use crate::files::FileVersion;
use crate::{
    import_symbol,
    Context,
    FunctionArgument,
    FunctionSymbol,
    ImportContext,
    SymbolInfo,
    SymbolLocation,
    SymbolTable,
    SymbolType,
    VariableSymbol,
};

use amber_types::LocalGenericsAllocator;

/// Pure analysis context — replaces `Files` + `Backend` in the legacy pipeline.
///
/// All mutable state is owned and local. No DashMaps, no async, no I/O.
pub struct PureContext {
    /// The symbol table being built for the current file.
    pub symbol_table: SymbolTable,
    /// Accumulated diagnostics (errors and warnings).
    pub diagnostics: Vec<PureDiagnostic>,
    /// Synthetic file key for the current file (used in `SymbolLocation`).
    pub file_key: (FileId, FileVersion),
    /// Fallback lookup for imported symbols that don't have a span in the local table.
    /// These are symbols imported via `import *` where we don't create span entries.
    pub imported_symbol_infos: HashMap<String, SymbolInfo>,
    /// Imported files' symbol tables for cross-file definition lookup.
    pub file_tables: HashMap<(FileId, FileVersion), SymbolTable>,
    /// Generics allocator for deterministic ID generation.
    pub allocator: LocalGenericsAllocator,
}

impl PureContext {
    pub fn report_error(&mut self, msg: &str, span: Span) {
        self.diagnostics.push(PureDiagnostic {
            message: msg.to_string(),
            span,
            severity: DiagnosticSeverity::Error,
        });
    }

    #[allow(dead_code)]
    pub fn report_warning(&mut self, msg: &str, span: Span) {
        self.diagnostics.push(PureDiagnostic {
            message: msg.to_string(),
            span,
            severity: DiagnosticSeverity::Warning,
        });
    }

    /// Look up a symbol definition at the given byte position.
    ///
    /// Checks scoped definitions first, then falls back to imported symbols.
    pub fn get_definition_info(&self, symbol: &str, position: usize) -> Option<SymbolInfo> {
        // Check the scoped definitions map first
        if let Some(defs) = self.symbol_table.definitions.get(symbol) {
            if let Some(location) = defs.get(&position) {
                // Same file: look up in local symbols
                if location.file == self.file_key {
                    if let Some(info) = self.symbol_table.symbols.get(&location.start) {
                        return Some(info.clone());
                    }
                }
                // Different file: look up in that file's symbol table
                if let Some(table) = self.file_tables.get(&location.file) {
                    if let Some(info) = table.symbols.get(&location.start) {
                        return Some(info.clone());
                    }
                }
            }
        }
        // Fall back to imported symbols (for import * without local span entries)
        self.imported_symbol_infos.get(symbol).cloned()
    }

    /// Insert a symbol reference (the pure version of `insert_symbol_reference`).
    ///
    /// Resolves the symbol definition, creates a reference entry in the symbol table,
    /// and reports "not defined" errors for undefined symbols.
    pub fn insert_reference(
        &mut self,
        symbol: &str,
        start: usize,
        end: usize,
        generics: &PureGenericsMap,
        contexts: &[Context],
    ) {
        let span = start..=end;
        if span.is_empty() {
            return;
        }

        let symbol_info = self.get_definition_info(symbol, start);

        match symbol_info {
            Some(info) => {
                // Resolve generic types
                let data_type = match info.data_type {
                    DataType::Generic(id) if generics.is_inferred(id) => generics.get_recursive(id),
                    DataType::Union(ref types) => {
                        DataType::Union(types.iter().map(|ty| generics.deref_type(ty)).collect())
                    }
                    ref ty => ty.clone(),
                };

                let symbol_type = match info.symbol_type {
                    SymbolType::Function(FunctionSymbol {
                        ref arguments,
                        is_public,
                        ref compiler_flags,
                        ref docs,
                    }) => SymbolType::Function(FunctionSymbol {
                        arguments: arguments
                            .iter()
                            .map(|(arg, arg_span)| {
                                (
                                    FunctionArgument {
                                        name: arg.name.clone(),
                                        data_type: generics.deref_type(&arg.data_type),
                                        is_optional: arg.is_optional,
                                        default_value_type: arg.default_value_type.clone(),
                                        is_ref: arg.is_ref,
                                    },
                                    *arg_span,
                                )
                            })
                            .collect(),
                        is_public,
                        compiler_flags: compiler_flags.clone(),
                        docs: docs.clone(),
                    }),
                    ref sym => sym.clone(),
                };

                self.symbol_table.symbols.insert(
                    span.clone(),
                    SymbolInfo {
                        name: symbol.to_string(),
                        symbol_type,
                        data_type,
                        is_definition: false,
                        undefined: false,
                        span: Span::from(start..end),
                        contexts: contexts.to_vec(),
                    },
                );
            }
            None => {
                self.report_error(&format!("\"{symbol}\" is not defined"), (start..end).into());

                self.symbol_table.symbols.insert(
                    span.clone(),
                    SymbolInfo {
                        name: symbol.to_string(),
                        symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                        data_type: DataType::Null,
                        is_definition: false,
                        undefined: true,
                        span: Span::from(start..end),
                        contexts: contexts.to_vec(),
                    },
                );
            }
        }

        // Add reference
        let references = self
            .symbol_table
            .references
            .entry(symbol.to_string())
            .or_default();
        references.push(SymbolLocation {
            file: self.file_key,
            start,
            end,
        });
    }

    /// Inject all public symbols from an imported file's symbol table.
    ///
    /// For `import *`, symbols are injected at global scope (0..=usize::MAX).
    pub(crate) fn import_all_symbols(
        &mut self,
        imported_table: &SymbolTable,
        imported_file_key: (FileId, FileVersion),
        is_public: bool,
    ) {
        for (name, location) in imported_table.public_definitions.iter() {
            let symbol_info =
                if let Some(info) = imported_table.symbols.get(&location.start).cloned() {
                    info
                } else if let Some(orig_table) = self.file_tables.get(&location.file) {
                    if let Some(info) = orig_table.symbols.get(&location.start).cloned() {
                        info
                    } else {
                        continue;
                    }
                } else {
                    continue;
                };

            import_symbol(
                &mut self.symbol_table,
                &SymbolInfo {
                    is_definition: false,
                    contexts: vec![Context::Import(ImportContext {
                        public_definitions: imported_table.public_definitions.clone(),
                        imported_symbols: vec![],
                    })],
                    ..symbol_info.clone()
                },
                None,
                &SymbolLocation {
                    file: imported_file_key,
                    start: location.start,
                    end: location.end,
                },
                is_public,
            );

            self.imported_symbol_infos.insert(name.clone(), symbol_info);
        }
    }
}
