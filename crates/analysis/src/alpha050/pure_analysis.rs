//! Pure analysis functions for the Salsa pipeline.
//!
//! These mirror the existing `analyze_global_stmnt`, `analyze_stmnt`, and `analyze_exp`
//! but work with local, owned state instead of the shared `Files` / `Backend` structures.
//!
//! Key differences from the legacy pipeline:
//! - No `&Files` / `&impl AnalysisHost` — uses `PureContext` for all state
//! - No `async` — all import resolution is pre-computed
//! - No DashMap mutations — builds a local `SymbolTable` + `Vec<PureDiagnostic>`
//! - Uses `PureGenericsMap` instead of global `GenericsMap`

use amber_grammar::alpha050::{
    Block,
    Comment,
    ElseCondition,
    Expression,
    FailableHandler,
    FailureHandler,
    FunctionArgument as GrammarFunctionArgument,
    GlobalStatement,
    IfChainContent,
    IfCondition,
    ImportContent,
    InterpolatedCommand,
    InterpolatedText,
    IterLoopVars,
    Statement,
    VariableInitType,
};
use amber_grammar::{
    CommandModifier,
    Span,
    Spanned,
};
use amber_types::paths::FileId;
use amber_types::{
    AmberVersion,
    DataType,
    PureGenericsMap,
};
use std::collections::HashMap;

use crate::alpha050::salsa_db::{
    DiagnosticSeverity,
    PureDiagnostic,
};
use crate::files::FileVersion;
use crate::types::make_union_type;
use crate::{
    import_symbol,
    insert_symbol_definition,
    BlockContext,
    Context,
    FunctionArgument,
    FunctionContext,
    FunctionSymbol,
    ImportContext,
    SymbolInfo,
    SymbolLocation,
    SymbolTable,
    SymbolType,
    VariableSymbol,
};
use amber_types::matches_type_pure;

use amber_types::LocalGenericsAllocator;

// ─── PureContext ──────────────────────────────────────────────────────────────

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
    fn import_all_symbols(
        &mut self,
        imported_table: &SymbolTable,
        imported_file_key: (FileId, FileVersion),
        is_public: bool,
    ) {
        for (name, location) in imported_table.public_definitions.iter() {
            // Try to find the SymbolInfo in the imported file's table
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

            // Store in imported_symbol_infos for fallback resolution
            self.imported_symbol_infos.insert(name.clone(), symbol_info);
        }
    }
}

// ─── Result types (re-exported from existing modules) ─────────────────────────

/// Result from analyzing a statement.
#[derive(Debug, Clone)]
pub struct StmntAnalysisResult {
    pub is_propagating_failure: bool,
    pub return_ty: Option<DataType>,
}

/// Result from analyzing an expression.
#[derive(Debug, Clone)]
pub struct ExpAnalysisResult {
    pub exp_ty: DataType,
    pub is_propagating_failure: bool,
    pub return_ty: Option<DataType>,
}

// ─── Entry Point ──────────────────────────────────────────────────────────────

/// Pre-resolved import: the imported file's symbol table and its file key.
pub struct ResolvedImport {
    pub symbol_table: SymbolTable,
    pub file_key: (FileId, FileVersion),
}

/// Pure version of `analyze_global_stmnt`.
///
/// Analyzes a file's AST given pre-resolved imports.
/// Returns (SymbolTable, diagnostics, generics_snapshot).
pub fn analyze_global_pure(
    file_key: (FileId, FileVersion),
    ast: &[Spanned<GlobalStatement>],
    _amber_version: &AmberVersion,
    // Map from import path string → resolved import info.
    // Includes "builtin" if the file is not builtin.ab itself.
    import_resolution: &HashMap<String, ResolvedImport>,
    is_builtin: bool,
    allocator: LocalGenericsAllocator,
) -> (SymbolTable, Vec<PureDiagnostic>, PureGenericsMap) {
    let mut generics = PureGenericsMap::new();
    let mut file_tables: HashMap<(FileId, FileVersion), SymbolTable> = HashMap::new();

    // Collect all imported file tables for cross-file resolution
    for ri in import_resolution.values() {
        file_tables.insert(ri.file_key, ri.symbol_table.clone());
    }

    let mut ctx = PureContext {
        symbol_table: SymbolTable::default(),
        diagnostics: Vec::new(),
        file_key,
        imported_symbol_infos: HashMap::new(),
        file_tables,
        allocator,
    };

    let mut contexts: Vec<Context> = vec![];

    // Implicit builtin import
    let mut default_imports: Vec<Spanned<GlobalStatement>> = vec![];
    if !is_builtin {
        default_imports.push((
            GlobalStatement::Import(
                (false, Span::from(0..0)),
                ("import".to_string(), Span::from(0..0)),
                (ImportContent::ImportAll, Span::from(0..0)),
                ("from".to_string(), Span::from(0..0)),
                ("builtin".to_string(), Span::from(0..0)),
            ),
            Span::from(0..0),
        ));
    }

    for (global, span) in default_imports.iter().chain(ast.iter()) {
        match global {
            GlobalStatement::FunctionDefinition(
                compiler_flags,
                (is_pub, _),
                _,
                (name, name_span),
                args,
                declared_return_ty,
                body,
            ) => {
                analyze_function_def(
                    &mut ctx,
                    &mut generics,
                    compiler_flags,
                    *is_pub,
                    name,
                    name_span,
                    args,
                    declared_return_ty,
                    body,
                    *span,
                    &mut contexts,
                );
            }
            GlobalStatement::Import(
                (is_public_import, _),
                _,
                (import_content, _),
                _,
                (path, path_span),
            ) => {
                analyze_import(
                    &mut ctx,
                    import_resolution,
                    *is_public_import,
                    import_content,
                    path,
                    path_span,
                );
            }
            GlobalStatement::Main(_, args, (body, body_span)) => {
                if let Some((args_name, args_span)) = args {
                    insert_symbol_definition(
                        &mut ctx.symbol_table,
                        &SymbolInfo {
                            name: args_name.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                            data_type: DataType::Array(Box::new(DataType::Text)),
                            is_definition: true,
                            undefined: false,
                            span: *args_span,
                            contexts: vec![],
                        },
                        ctx.file_key,
                        args_span.end..=span.end,
                        false,
                    );
                }

                let mut main_contexts = vec![Context::Main];
                match body {
                    Block::Block(..) => {
                        analyze_stmnt_pure(
                            &(Statement::Block((body.clone(), *body_span)), *body_span),
                            &mut ctx,
                            body_span.end,
                            &mut generics.clone(),
                            &mut main_contexts,
                        );
                    }
                    Block::Singleline(stmnt) => {
                        analyze_stmnt_pure(
                            stmnt,
                            &mut ctx,
                            body_span.end,
                            &mut generics.clone(),
                            &mut main_contexts,
                        );
                    }
                    Block::Error => {}
                }
            }
            GlobalStatement::Statement(stmnt) => {
                analyze_stmnt_pure(
                    stmnt,
                    &mut ctx,
                    usize::MAX,
                    &mut generics.clone(),
                    &mut contexts,
                );
            }
        }
    }

    (ctx.symbol_table, ctx.diagnostics, generics)
}

// ─── Function Definition Analysis ─────────────────────────────────────────────

#[allow(clippy::too_many_arguments)]
fn analyze_function_def(
    ctx: &mut PureContext,
    generics: &mut PureGenericsMap,
    compiler_flags: &[Spanned<amber_grammar::CompilerFlag>],
    is_pub: bool,
    name: &str,
    name_span: &Span,
    args: &[Spanned<GrammarFunctionArgument>],
    declared_return_ty: &Option<Spanned<DataType>>,
    body: &[Spanned<Statement>],
    span: Span,
    contexts: &mut Vec<Context>,
) {
    // Scoped generics: clone to isolate function-local generics
    let mut scoped_generics = generics.clone();
    let mut new_generic_types: Vec<usize> = vec![];
    let mut prev_arg_optional = false;

    args.iter().for_each(|(arg, _)| {
        let (arg_name, ty, arg_name_span) = match arg {
            GrammarFunctionArgument::Generic(_, (arg_name, arg_span)) => {
                let generic_id = ctx.allocator.next_id();
                scoped_generics.constrain_generic_type(generic_id, DataType::Any);
                new_generic_types.push(generic_id);

                if prev_arg_optional {
                    ctx.report_error("Optional argument must be the last one", *arg_span);
                }

                (arg_name, DataType::Generic(generic_id), arg_span)
            }
            GrammarFunctionArgument::Typed(_, (arg_name, arg_span), (ty, _)) => {
                if prev_arg_optional {
                    ctx.report_error("Optional argument must be the last one", *arg_span);
                }
                (arg_name, ty.clone(), arg_span)
            }
            GrammarFunctionArgument::Optional((is_ref, _), (arg_name, arg_span), ty, exp) => {
                prev_arg_optional = true;
                if *is_ref {
                    ctx.report_error("Optional argument cannot be a reference", *arg_span);
                }
                (
                    arg_name,
                    match ty {
                        Some((ty, _)) => {
                            analyze_exp_pure(exp, ty.clone(), ctx, &mut generics.clone(), &[]);
                            ty.clone()
                        }
                        None => {
                            let generic_id = ctx.allocator.next_id();
                            scoped_generics.constrain_generic_type(generic_id, DataType::Any);
                            new_generic_types.push(generic_id);
                            DataType::Generic(generic_id)
                        }
                    },
                    arg_span,
                )
            }
            GrammarFunctionArgument::Error => return,
        };

        insert_symbol_definition(
            &mut ctx.symbol_table,
            &SymbolInfo {
                name: arg_name.to_string(),
                symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                data_type: ty,
                is_definition: true,
                undefined: false,
                span: *arg_name_span,
                contexts: vec![],
            },
            ctx.file_key,
            arg_name_span.end..=span.end,
            false,
        );
    });

    let mut return_types = vec![];
    let mut is_propagating = false;

    let mut function_contexts = vec![Context::Function(FunctionContext {
        compiler_flags: vec![],
    })];

    body.iter().for_each(|stmnt| {
        let result = analyze_stmnt_pure(
            stmnt,
            ctx,
            span.end,
            &mut scoped_generics,
            &mut function_contexts,
        );
        is_propagating |= result.is_propagating_failure;
        return_types.extend(result.return_ty);
    });

    // Propagate inferred generics back to the global generics map
    for generic_id in new_generic_types.iter() {
        let ty = scoped_generics.get(*generic_id);
        generics.constrain_generic_type(*generic_id, ty.clone());
        generics.mark_as_inferred(*generic_id);
        propagate_nested_generics_pure(&ty, &scoped_generics, generics);
    }

    let mut inferred_return_type = match return_types.len() {
        0 => DataType::Null,
        _ => make_union_type(return_types),
    };

    if is_propagating && !matches!(inferred_return_type, DataType::Failable(_)) {
        inferred_return_type = DataType::Failable(Box::new(inferred_return_type));
    }

    let data_type = match declared_return_ty {
        Some((ty, ty_span)) => {
            if !matches_type_pure(ty, &inferred_return_type, generics) {
                ctx.report_error(
                    &format!(
                        "Function returns type {inferred_return_type:?}, but expected {ty:?}",
                    ),
                    *ty_span,
                );
            }
            if is_propagating && !matches!(ty, DataType::Failable(_)) {
                ctx.report_error(
                    "Function is propagating an error, but return type is not failable",
                    *ty_span,
                );
            }
            ty.clone()
        }
        None => inferred_return_type,
    };

    // Build the function's symbol info with argument types
    let mut generic_ids_for_symbol = new_generic_types.clone();
    let function_arguments: Vec<Spanned<FunctionArgument>> = args
        .iter()
        .filter_map(|(arg, arg_span)| match arg {
            GrammarFunctionArgument::Generic((is_ref, _), (arg_name, _)) => Some((
                FunctionArgument {
                    name: arg_name.clone(),
                    data_type: DataType::Generic(generic_ids_for_symbol.remove(0)),
                    is_optional: false,
                    default_value_type: None,
                    is_ref: *is_ref,
                },
                *arg_span,
            )),
            GrammarFunctionArgument::Typed((is_ref, _), (arg_name, _), (ty, _)) => Some((
                FunctionArgument {
                    name: arg_name.clone(),
                    data_type: ty.clone(),
                    is_optional: false,
                    default_value_type: None,
                    is_ref: *is_ref,
                },
                *arg_span,
            )),
            GrammarFunctionArgument::Optional((is_ref, _), (arg_name, _), ty, exp) => Some((
                FunctionArgument {
                    name: arg_name.clone(),
                    data_type: match ty {
                        Some((ty, _)) => ty.clone(),
                        None => DataType::Generic(generic_ids_for_symbol.remove(0)),
                    },
                    is_optional: true,
                    default_value_type: {
                        let ExpAnalysisResult { exp_ty, .. } = analyze_exp_pure(
                            exp,
                            DataType::Any,
                            ctx,
                            &mut scoped_generics,
                            &function_contexts,
                        );
                        Some(exp_ty)
                    },
                    is_ref: *is_ref,
                },
                *arg_span,
            )),
            GrammarFunctionArgument::Error => None,
        })
        .collect();

    insert_symbol_definition(
        &mut ctx.symbol_table,
        &SymbolInfo {
            name: name.to_string(),
            symbol_type: SymbolType::Function(FunctionSymbol {
                arguments: function_arguments,
                is_public: is_pub,
                compiler_flags: compiler_flags
                    .iter()
                    .map(|(flag, _)| flag.clone())
                    .collect(),
                docs: match contexts.last() {
                    Some(Context::DocString(doc)) => {
                        let doc = doc.clone();
                        contexts.pop();
                        Some(doc)
                    }
                    _ => None,
                },
            }),
            data_type: data_type.clone(),
            is_definition: true,
            undefined: false,
            span: *name_span,
            contexts: vec![],
        },
        ctx.file_key,
        span.end..=usize::MAX,
        is_pub,
    );
}

// ─── Import Analysis ──────────────────────────────────────────────────────────

fn analyze_import(
    ctx: &mut PureContext,
    import_resolution: &HashMap<String, ResolvedImport>,
    is_public_import: bool,
    import_content: &ImportContent,
    path: &str,
    path_span: &Span,
) {
    // Look up the pre-resolved import
    let resolved = import_resolution.get(path);

    // Insert import path symbol
    let import_file_key = resolved.map(|r| r.file_key).unwrap_or(ctx.file_key);
    insert_symbol_definition(
        &mut ctx.symbol_table,
        &SymbolInfo {
            name: path.to_string(),
            symbol_type: SymbolType::ImportPath,
            data_type: DataType::Text,
            is_definition: true,
            undefined: false,
            span: *path_span,
            contexts: vec![],
        },
        import_file_key,
        path_span.start..=path_span.end,
        false,
    );

    let resolved = match resolved {
        Some(r) => r,
        None => {
            ctx.report_error("File doesn't exist", *path_span);
            return;
        }
    };

    let imported_table = &resolved.symbol_table;
    let imported_file_key = resolved.file_key;

    match import_content {
        ImportContent::ImportSpecific(ident_list) => {
            let mut import_context = ImportContext {
                public_definitions: imported_table.public_definitions.clone(),
                imported_symbols: vec![],
            };

            ident_list.iter().for_each(|(ident, ident_span)| {
                if import_context.imported_symbols.contains(&ident.to_string()) {
                    ctx.report_error(&format!("Duplicate import '{ident}'"), *ident_span);

                    ctx.symbol_table.symbols.insert(
                        ident_span.start..=ident_span.end,
                        SymbolInfo {
                            name: ident.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                            data_type: DataType::Null,
                            is_definition: false,
                            undefined: true,
                            span: Span::from(ident_span.start..ident_span.end),
                            contexts: vec![Context::Import(import_context.clone())],
                        },
                    );
                    return;
                }

                let symbol_definition = imported_table.public_definitions.get(ident);

                match symbol_definition {
                    Some(definition_location) => {
                        // Find the full SymbolInfo from the definition's file
                        let symbol_info =
                            find_symbol_info(ctx, imported_table, definition_location);

                        if let Some(info) = symbol_info {
                            import_symbol(
                                &mut ctx.symbol_table,
                                &SymbolInfo {
                                    is_definition: false,
                                    contexts: vec![Context::Import(import_context.clone())],
                                    ..info.clone()
                                },
                                Some(ident_span.start..=ident_span.end),
                                &SymbolLocation {
                                    file: imported_file_key,
                                    start: definition_location.start,
                                    end: definition_location.end,
                                },
                                is_public_import,
                            );

                            // Also store as imported symbol for fallback resolution
                            ctx.imported_symbol_infos.insert(ident.to_string(), info);
                        }

                        import_context.imported_symbols.push(ident.to_string());
                    }
                    None => {
                        ctx.report_error(&format!("Could not resolve '{ident}'"), *ident_span);

                        ctx.symbol_table.symbols.insert(
                            ident_span.start..=ident_span.end,
                            SymbolInfo {
                                name: ident.to_string(),
                                symbol_type: SymbolType::Variable(VariableSymbol {
                                    is_const: false,
                                }),
                                data_type: DataType::Null,
                                is_definition: false,
                                undefined: true,
                                span: Span::from(ident_span.start..ident_span.end),
                                contexts: vec![Context::Import(import_context.clone())],
                            },
                        );
                    }
                };
            });
        }
        ImportContent::ImportAll => {
            ctx.import_all_symbols(imported_table, imported_file_key, is_public_import);
        }
    }
}

/// Find the full SymbolInfo for a definition location, searching both the
/// imported table and any transitively-available file tables.
fn find_symbol_info(
    ctx: &PureContext,
    imported_table: &SymbolTable,
    location: &SymbolLocation,
) -> Option<SymbolInfo> {
    // Try the imported table directly
    if let Some(info) = imported_table.symbols.get(&location.start) {
        return Some(info.clone());
    }
    // Try cross-file tables (for re-exports)
    if let Some(table) = ctx.file_tables.get(&location.file) {
        if let Some(info) = table.symbols.get(&location.start) {
            return Some(info.clone());
        }
    }
    None
}

// ─── Statement Analysis ───────────────────────────────────────────────────────

pub fn analyze_stmnt_pure(
    (stmnt, span): &Spanned<Statement>,
    ctx: &mut PureContext,
    scope_end: usize,
    scoped_generic_types: &mut PureGenericsMap,
    contexts: &mut Vec<Context>,
) -> StmntAnalysisResult {
    match stmnt {
        Statement::Block(block) => analyze_block_pure(block, ctx, scoped_generic_types, contexts),
        Statement::IfChain(_, if_chain) => {
            let mut stmnts = vec![];
            let mut exps = vec![];

            for (if_chain_content, _) in if_chain.iter() {
                match if_chain_content {
                    IfChainContent::IfCondition((condition, _)) => match condition {
                        IfCondition::IfCondition(exp, block) => {
                            let exp = analyze_exp_pure(
                                exp,
                                DataType::Boolean,
                                ctx,
                                scoped_generic_types,
                                contexts,
                            );
                            let stmnt =
                                analyze_block_pure(block, ctx, scoped_generic_types, contexts);
                            exps.push(exp);
                            stmnts.push(stmnt);
                        }
                        IfCondition::Error => {}
                        IfCondition::Comment((Comment::DocString(docs), _)) => {
                            let _ = handle_doc_strings(docs, contexts);
                        }
                        IfCondition::Comment(_) => {}
                    },
                    IfChainContent::Else((else_cond, _)) => match else_cond {
                        ElseCondition::Else(_, block) => {
                            let stmnt =
                                analyze_block_pure(block, ctx, scoped_generic_types, contexts);
                            stmnts.push(stmnt);
                        }
                    },
                    IfChainContent::Comment((Comment::DocString(docs), _)) => {
                        let _ = handle_doc_strings(docs, contexts);
                    }
                    IfChainContent::Comment(_) => {}
                }
            }

            get_stmnt_analysis_result(stmnts, exps)
        }
        Statement::IfCondition(_, if_cond, comments, else_cond) => {
            let mut stmnts = vec![];
            let mut exps = vec![];

            match &if_cond.0 {
                IfCondition::IfCondition(exp, block) => {
                    let exp = analyze_exp_pure(
                        exp,
                        DataType::Boolean,
                        ctx,
                        scoped_generic_types,
                        contexts,
                    );
                    let block = analyze_block_pure(block, ctx, scoped_generic_types, contexts);
                    stmnts.push(block);
                    exps.push(exp);
                }
                IfCondition::Comment((Comment::DocString(docs), _)) => {
                    let _ = handle_doc_strings(docs, contexts);
                }
                _ => {}
            }

            comments.iter().for_each(|(comment, _)| {
                if let Comment::DocString(docs) = comment {
                    let _ = handle_doc_strings(docs, contexts);
                }
            });

            if let Some(else_cond) = else_cond {
                match &else_cond.0 {
                    ElseCondition::Else(_, block) => {
                        let block = analyze_block_pure(block, ctx, scoped_generic_types, contexts);
                        stmnts.push(block);
                    }
                }
            }

            get_stmnt_analysis_result(stmnts, exps)
        }
        Statement::InfiniteLoop(_, block) => {
            let mut new_contexts = contexts.clone();
            new_contexts.push(Context::Loop);
            analyze_block_pure(block, ctx, scoped_generic_types, &new_contexts)
        }
        Statement::WhileLoop(_, exp, block) => {
            let exp_analysis =
                analyze_exp_pure(exp, DataType::Boolean, ctx, scoped_generic_types, contexts);
            let mut new_contexts = contexts.clone();
            new_contexts.push(Context::Loop);
            let block_analysis =
                analyze_block_pure(block, ctx, scoped_generic_types, &new_contexts);
            get_stmnt_analysis_result(vec![block_analysis], vec![exp_analysis])
        }
        Statement::IterLoop(_, (vars, _), _, exp, block) => {
            let block_span = block.1;
            let exp_result = analyze_exp_pure(
                exp,
                DataType::Array(Box::new(DataType::Any)),
                ctx,
                scoped_generic_types,
                contexts,
            );

            let iter_type = match exp_result.exp_ty.clone() {
                DataType::Array(ty) => *ty,
                DataType::Failable(ty) => {
                    if let DataType::Array(inner_ty) = *ty {
                        *inner_ty
                    } else {
                        DataType::Any
                    }
                }
                _ => DataType::Any,
            };

            match vars {
                IterLoopVars::WithIndex((var1, var1_span), (var2, var2_span)) => {
                    insert_symbol_definition(
                        &mut ctx.symbol_table,
                        &SymbolInfo {
                            name: var1.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                            data_type: DataType::Int,
                            is_definition: true,
                            undefined: false,
                            span: *var1_span,
                            contexts: contexts.clone(),
                        },
                        ctx.file_key,
                        block_span.start..=block_span.end,
                        false,
                    );
                    insert_symbol_definition(
                        &mut ctx.symbol_table,
                        &SymbolInfo {
                            name: var2.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                            data_type: iter_type,
                            is_definition: true,
                            undefined: false,
                            span: *var2_span,
                            contexts: contexts.clone(),
                        },
                        ctx.file_key,
                        block_span.start..=block_span.end,
                        false,
                    );
                }
                IterLoopVars::Single((var, var_span)) => {
                    insert_symbol_definition(
                        &mut ctx.symbol_table,
                        &SymbolInfo {
                            name: var.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                            data_type: iter_type,
                            is_definition: true,
                            undefined: false,
                            span: *var_span,
                            contexts: contexts.clone(),
                        },
                        ctx.file_key,
                        block_span.start..=block_span.end,
                        false,
                    );
                }
                _ => {}
            }

            let mut new_contexts = contexts.clone();
            new_contexts.push(Context::Loop);
            let block_result = analyze_block_pure(block, ctx, scoped_generic_types, &new_contexts);
            get_stmnt_analysis_result(vec![block_result], vec![exp_result])
        }
        Statement::VariableInit(_, (var_name, var_span), (value, _)) => {
            let exp = match value {
                VariableInitType::Expression(exp) => {
                    analyze_exp_pure(exp, DataType::Any, ctx, scoped_generic_types, contexts)
                }
                VariableInitType::DataType((ty, _)) => ExpAnalysisResult {
                    exp_ty: ty.clone(),
                    is_propagating_failure: false,
                    return_ty: None,
                },
                _ => ExpAnalysisResult {
                    exp_ty: DataType::Error,
                    is_propagating_failure: false,
                    return_ty: None,
                },
            };

            let var_type = match exp.exp_ty {
                DataType::Failable(ref ty) => scoped_generic_types.deref_type(ty),
                ref ty => scoped_generic_types.deref_type(ty),
            };

            insert_symbol_definition(
                &mut ctx.symbol_table,
                &SymbolInfo {
                    name: var_name.to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                    data_type: var_type,
                    is_definition: true,
                    undefined: false,
                    span: *var_span,
                    contexts: contexts.clone(),
                },
                ctx.file_key,
                span.end..=scope_end,
                false,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::ConstInit(_, (var_name, var_span), exp) => {
            let exp = analyze_exp_pure(exp, DataType::Any, ctx, scoped_generic_types, contexts);

            let var_type = match exp.exp_ty {
                DataType::Failable(ref ty) => scoped_generic_types.deref_type(ty),
                ref ty => scoped_generic_types.deref_type(ty),
            };

            insert_symbol_definition(
                &mut ctx.symbol_table,
                &SymbolInfo {
                    name: var_name.to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbol { is_const: true }),
                    data_type: var_type,
                    is_definition: true,
                    undefined: false,
                    span: *var_span,
                    contexts: contexts.clone(),
                },
                ctx.file_key,
                span.end..=scope_end,
                false,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::Echo(_, exp) => {
            let exp = analyze_exp_pure(exp, DataType::Any, ctx, scoped_generic_types, contexts);
            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::Expression(exp) => {
            let exp = analyze_exp_pure(exp, DataType::Any, ctx, scoped_generic_types, contexts);
            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::Fail(_, exp) => {
            if !contexts
                .iter()
                .any(|c| matches!(c, Context::Function(_) | Context::Main))
            {
                ctx.report_error(
                    "Fail statements can only be used inside of functions or the main block",
                    *span,
                );
            }
            if let Some(exp) = exp {
                let exp = analyze_exp_pure(exp, DataType::Int, ctx, scoped_generic_types, contexts);
                return StmntAnalysisResult {
                    is_propagating_failure: true,
                    return_ty: exp.return_ty,
                };
            }
            StmntAnalysisResult {
                is_propagating_failure: true,
                return_ty: None,
            }
        }
        Statement::Return(_, exp) => {
            if !contexts.iter().any(|c| matches!(c, Context::Function(_))) {
                ctx.report_error("Return statement outside of function", *span);
            }
            if let Some(exp) = exp {
                let exp_analysis =
                    analyze_exp_pure(exp, DataType::Any, ctx, scoped_generic_types, contexts);
                if let Some(ty) = exp_analysis.return_ty {
                    return StmntAnalysisResult {
                        is_propagating_failure: exp_analysis.is_propagating_failure,
                        return_ty: Some(make_union_type(vec![ty, exp_analysis.exp_ty])),
                    };
                }
                return StmntAnalysisResult {
                    is_propagating_failure: exp_analysis.is_propagating_failure,
                    return_ty: Some(exp_analysis.exp_ty),
                };
            }
            StmntAnalysisResult {
                is_propagating_failure: false,
                return_ty: None,
            }
        }
        Statement::ShorthandAdd((var, var_span), exp) => analyze_shorthand_op(
            ctx,
            scoped_generic_types,
            contexts,
            var,
            var_span,
            exp,
            ShorthandOp::Add,
        ),
        Statement::ShorthandSub((var, var_span), exp) => analyze_shorthand_op(
            ctx,
            scoped_generic_types,
            contexts,
            var,
            var_span,
            exp,
            ShorthandOp::Sub,
        ),
        Statement::ShorthandMul((var, var_span), exp) => analyze_shorthand_op(
            ctx,
            scoped_generic_types,
            contexts,
            var,
            var_span,
            exp,
            ShorthandOp::Mul,
        ),
        Statement::ShorthandDiv((var, var_span), exp) => analyze_shorthand_op(
            ctx,
            scoped_generic_types,
            contexts,
            var,
            var_span,
            exp,
            ShorthandOp::Div,
        ),
        Statement::ShorthandModulo((var, var_span), exp) => analyze_shorthand_op(
            ctx,
            scoped_generic_types,
            contexts,
            var,
            var_span,
            exp,
            ShorthandOp::Modulo,
        ),
        Statement::VariableSet((var, var_span), exp) => {
            let var_ty = match ctx.get_definition_info(var, var_span.start) {
                Some(info) => {
                    match info.symbol_type {
                        SymbolType::Function(_) => {
                            ctx.report_error("Cannot assign to a function", *var_span);
                        }
                        SymbolType::Variable(ref v) if v.is_const => {
                            ctx.report_error("Cannot assign to a constant", *var_span);
                        }
                        _ => {}
                    }
                    info.data_type
                }
                None => DataType::Any,
            };

            let exp_analysis = analyze_exp_pure(exp, var_ty, ctx, scoped_generic_types, contexts);

            ctx.insert_reference(
                var,
                var_span.start,
                var_span.end,
                scoped_generic_types,
                contexts,
            );

            StmntAnalysisResult {
                is_propagating_failure: exp_analysis.is_propagating_failure,
                return_ty: exp_analysis.return_ty,
            }
        }
        Statement::Break => {
            if !contexts.iter().any(|c| matches!(c, Context::Loop)) {
                ctx.report_error("Break statement outside of loop", *span);
            }
            StmntAnalysisResult {
                is_propagating_failure: false,
                return_ty: None,
            }
        }
        Statement::Continue => {
            if !contexts.iter().any(|c| matches!(c, Context::Loop)) {
                ctx.report_error("Continue statement outside of loop", *span);
            }
            StmntAnalysisResult {
                is_propagating_failure: false,
                return_ty: None,
            }
        }
        Statement::Cd(_, exp) => {
            let exp = analyze_exp_pure(exp, DataType::Text, ctx, scoped_generic_types, contexts);
            StmntAnalysisResult {
                is_propagating_failure: exp.is_propagating_failure,
                return_ty: exp.return_ty,
            }
        }
        Statement::MoveFiles(modifiers, _, from_exp, to_exp, failable_handlers) => {
            let exp1 = analyze_exp_pure(
                from_exp,
                DataType::Text,
                ctx,
                scoped_generic_types,
                contexts,
            );
            let exp2 =
                analyze_exp_pure(to_exp, DataType::Text, ctx, scoped_generic_types, contexts);
            let stmnt = analyze_failable_handlers_pure(
                failable_handlers,
                ctx,
                scoped_generic_types,
                contexts,
            );

            let has_failure_handler = failable_handlers.iter().any(|(modifier, _)| {
                matches!(modifier, FailableHandler::Failure(_))
                    || matches!(modifier, FailableHandler::Exited(_, _, _))
            });

            if !has_failure_handler
                && !modifiers.iter().any(|(modifier, _)| {
                    *modifier == CommandModifier::Unsafe || *modifier == CommandModifier::Trust
                })
                && !contexts.iter().any(|c| match c {
                    Context::Block(BlockContext { modifiers }) => modifiers
                        .iter()
                        .any(|m| *m == CommandModifier::Unsafe || *m == CommandModifier::Trust),
                    _ => false,
                })
            {
                ctx.report_error("Command must have a failure handler", *span);
            }

            get_stmnt_analysis_result(vec![stmnt], vec![exp1, exp2])
        }
        Statement::Comment((Comment::DocString(docs), _)) => handle_doc_strings(docs, contexts),
        Statement::Comment(_) | Statement::Shebang(_) | Statement::Error => StmntAnalysisResult {
            is_propagating_failure: false,
            return_ty: None,
        },
    }
}

// ─── Shorthand Ops ────────────────────────────────────────────────────────────

#[derive(Clone, Copy)]
enum ShorthandOp {
    Add,
    Sub,
    Mul,
    Div,
    Modulo,
}

fn analyze_shorthand_op(
    ctx: &mut PureContext,
    scoped_generic_types: &mut PureGenericsMap,
    contexts: &[Context],
    var: &str,
    var_span: &Span,
    exp: &Spanned<Expression>,
    op: ShorthandOp,
) -> StmntAnalysisResult {
    let var_ty = match ctx.get_definition_info(var, var_span.start) {
        Some(info) => {
            match info.symbol_type {
                SymbolType::Function(_) => {
                    ctx.report_error("Cannot assign to a function", *var_span);
                }
                SymbolType::Variable(ref v) if v.is_const => {
                    ctx.report_error("Cannot assign to a constant", *var_span);
                }
                _ => {}
            }
            info.data_type
        }
        None => DataType::Any,
    };

    let (valid_types, expected_exp_ty, error_verb) = match op {
        ShorthandOp::Add => {
            let default_ty = DataType::Union(vec![
                DataType::Text,
                DataType::Number,
                DataType::Int,
                DataType::Array(Box::new(DataType::Union(vec![
                    DataType::Text,
                    DataType::Number,
                    DataType::Int,
                ]))),
            ]);
            (default_ty, var_ty.clone(), "add to")
        }
        ShorthandOp::Sub => (
            DataType::Union(vec![DataType::Number, DataType::Int]),
            DataType::Union(vec![DataType::Number, DataType::Int]),
            "subtract with",
        ),
        ShorthandOp::Mul => (
            DataType::Union(vec![DataType::Number, DataType::Int]),
            DataType::Union(vec![DataType::Number, DataType::Int]),
            "multiply with",
        ),
        ShorthandOp::Div => (
            DataType::Union(vec![DataType::Number, DataType::Int]),
            DataType::Union(vec![DataType::Number, DataType::Int]),
            "divide",
        ),
        ShorthandOp::Modulo => (
            DataType::Union(vec![DataType::Number, DataType::Int]),
            DataType::Union(vec![DataType::Number, DataType::Int]),
            "use modulo with",
        ),
    };

    let exp_analysis = analyze_exp_pure(
        exp,
        match op {
            ShorthandOp::Add => var_ty.clone(),
            _ => expected_exp_ty,
        },
        ctx,
        scoped_generic_types,
        contexts,
    );

    match op {
        ShorthandOp::Add => {
            if !matches_type_pure(&valid_types, &var_ty, scoped_generic_types)
                || !matches_type_pure(&exp_analysis.exp_ty, &var_ty, scoped_generic_types)
            {
                ctx.report_error(
                    &format!(
                        "Cannot {} variable of type {}",
                        error_verb,
                        scoped_generic_types.to_display_string(&var_ty)
                    ),
                    *var_span,
                );
            }
        }
        _ => {
            if !matches_type_pure(&valid_types, &var_ty, scoped_generic_types) {
                ctx.report_error(
                    &format!(
                        "Cannot {} variable of type {}",
                        error_verb,
                        scoped_generic_types.to_display_string(&var_ty)
                    ),
                    *var_span,
                );
            }
        }
    }

    if let DataType::Generic(id) = var_ty {
        scoped_generic_types.constrain_generic_type(id, exp_analysis.exp_ty.clone());
    }

    ctx.insert_reference(
        var,
        var_span.start,
        var_span.end,
        scoped_generic_types,
        contexts,
    );

    StmntAnalysisResult {
        is_propagating_failure: exp_analysis.is_propagating_failure,
        return_ty: exp_analysis.return_ty,
    }
}

// ─── Block Analysis ───────────────────────────────────────────────────────────

pub fn analyze_block_pure(
    (block, span): &Spanned<Block>,
    ctx: &mut PureContext,
    scoped_generic_types: &mut PureGenericsMap,
    contexts: &[Context],
) -> StmntAnalysisResult {
    let mut types: Vec<DataType> = vec![];
    let mut is_propagating = false;

    match block {
        Block::Block(modifiers, stmnt_list) => {
            let mut new_contexts = contexts.to_owned();
            new_contexts.push(Context::Block(BlockContext {
                modifiers: modifiers.iter().map(|(m, _)| m.clone()).collect(),
            }));

            for stmnt in stmnt_list.iter() {
                let result = analyze_stmnt_pure(
                    stmnt,
                    ctx,
                    span.end,
                    scoped_generic_types,
                    &mut new_contexts,
                );
                if let Some(ty) = result.return_ty {
                    types.push(ty);
                }
                is_propagating |= result.is_propagating_failure;
            }
        }
        Block::Singleline(stmnt) => {
            let mut working_contexts = contexts.to_owned();
            let result = analyze_stmnt_pure(
                stmnt,
                ctx,
                span.end,
                scoped_generic_types,
                &mut working_contexts,
            );
            if let Some(ty) = result.return_ty {
                types.push(ty);
            }
            is_propagating |= result.is_propagating_failure;
        }
        Block::Error => {}
    }

    if types.is_empty() {
        StmntAnalysisResult {
            is_propagating_failure: is_propagating,
            return_ty: None,
        }
    } else {
        StmntAnalysisResult {
            is_propagating_failure: is_propagating,
            return_ty: Some(make_union_type(types)),
        }
    }
}

// ─── Failable Handlers ───────────────────────────────────────────────────────

pub fn analyze_failable_handlers_pure(
    failable_handlers: &[Spanned<FailableHandler>],
    ctx: &mut PureContext,
    scoped_generic_types: &mut PureGenericsMap,
    contexts: &[Context],
) -> StmntAnalysisResult {
    let mut types: Vec<DataType> = vec![];
    let mut is_propagating = false;
    let mut contexts = contexts.to_owned();

    let mut has_exited_handler = false;
    let mut has_status_handler = false;

    failable_handlers
        .iter()
        .for_each(|(handler, _)| match handler {
            FailableHandler::Failure((failure, handler_span)) => {
                let mut keyword_span = *handler_span;
                match failure {
                    FailureHandler::Handle((_, name_span), exit_code, block) => {
                        keyword_span = *name_span;

                        if let Some((code_var, code_var_span)) = exit_code {
                            insert_symbol_definition(
                                &mut ctx.symbol_table,
                                &SymbolInfo {
                                    name: code_var.to_string(),
                                    symbol_type: SymbolType::Variable(VariableSymbol {
                                        is_const: false,
                                    }),
                                    data_type: DataType::Int,
                                    is_definition: true,
                                    undefined: false,
                                    span: *code_var_span,
                                    contexts: vec![],
                                },
                                ctx.file_key,
                                code_var_span.end..=handler_span.end,
                                false,
                            );
                        }

                        let result = analyze_stmnt_pure(
                            &(Statement::Block(*block.clone()), block.1),
                            ctx,
                            handler_span.end,
                            scoped_generic_types,
                            &mut contexts,
                        );
                        types.extend(result.return_ty);
                        is_propagating |= result.is_propagating_failure;
                    }
                    FailureHandler::Propagate => {
                        if !contexts
                            .iter()
                            .any(|c| *c == Context::Main || matches!(c, Context::Function(_)))
                        {
                            ctx.report_error(
                                "Propagate can only be used inside of main block or function",
                                *handler_span,
                            );
                        }
                        is_propagating = true;
                    }
                };

                has_status_handler = true;
                if has_exited_handler {
                    ctx.report_error(
                        "Exited handler should be used without any other handlers. Remove this handler or `exited` handler",
                        keyword_span,
                    );
                }
            }
            FailableHandler::Succeeded((_, handler_span), block) => {
                let result = analyze_stmnt_pure(
                    &(Statement::Block(*block.clone()), block.1),
                    ctx,
                    block.1.end,
                    scoped_generic_types,
                    &mut contexts,
                );

                has_status_handler = true;
                if has_exited_handler {
                    ctx.report_error(
                        "Exited handler should be used without any other handlers. Remove this handler or `exited` handler",
                        *handler_span,
                    );
                }

                types.extend(result.return_ty);
                is_propagating |= result.is_propagating_failure;
            }
            FailableHandler::Exited((_, name_span), (code_var, code_var_span), block) => {
                insert_symbol_definition(
                    &mut ctx.symbol_table,
                    &SymbolInfo {
                        name: code_var.to_string(),
                        symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                        data_type: DataType::Int,
                        is_definition: true,
                        undefined: false,
                        span: *code_var_span,
                        contexts: vec![],
                    },
                    ctx.file_key,
                    code_var_span.end..=block.1.end,
                    false,
                );

                let result = analyze_stmnt_pure(
                    &(Statement::Block(*block.clone()), block.1),
                    ctx,
                    block.1.end,
                    scoped_generic_types,
                    &mut contexts,
                );

                has_exited_handler = true;
                if has_status_handler {
                    ctx.report_error(
                        "Exited handler should be used without any other handlers",
                        *name_span,
                    );
                }

                types.extend(result.return_ty);
                is_propagating |= result.is_propagating_failure;
            }
            FailableHandler::Comment(_) => {}
        });

    if types.is_empty() {
        StmntAnalysisResult {
            is_propagating_failure: is_propagating,
            return_ty: None,
        }
    } else {
        StmntAnalysisResult {
            is_propagating_failure: is_propagating,
            return_ty: Some(make_union_type(types)),
        }
    }
}

// ─── Expression Analysis ──────────────────────────────────────────────────────

pub fn analyze_exp_pure(
    (exp, exp_span): &Spanned<Expression>,
    expected_type: DataType,
    ctx: &mut PureContext,
    scoped_generic_types: &mut PureGenericsMap,
    contexts: &[Context],
) -> ExpAnalysisResult {
    let exp_span_inclusive = exp_span.start..=exp_span.end;

    if exp_span_inclusive.is_empty() {
        return ExpAnalysisResult {
            exp_ty: DataType::Null,
            is_propagating_failure: false,
            return_ty: None,
        };
    }

    let mut return_types: Vec<DataType> = vec![];
    let mut is_propagating_failure = false;

    macro_rules! analyze_expr {
        ($exp:expr, $exp_type:expr) => {{
            let result = analyze_exp_pure($exp, $exp_type, ctx, scoped_generic_types, contexts);
            is_propagating_failure |= result.is_propagating_failure;
            return_types.extend(result.return_ty.iter().cloned());
            result
        }};
    }

    macro_rules! analyze_binop_codep {
        ($exp1:expr, $exp1_type:expr, $exp2:expr) => {{
            let lhs_result = analyze_expr!($exp1, $exp1_type);
            let rhs_result = analyze_expr!($exp2, lhs_result.exp_ty.clone());

            if let DataType::Generic(id) = lhs_result.exp_ty.clone() {
                scoped_generic_types.constrain_generic_type(id, rhs_result.exp_ty.clone());
            }

            if !matches_type_pure(&rhs_result.exp_ty, &lhs_result.exp_ty, scoped_generic_types) {
                ctx.report_error(
                    &format!(
                        "Expected type {}, found type {}",
                        scoped_generic_types.to_display_string(&rhs_result.exp_ty),
                        scoped_generic_types.to_display_string(&lhs_result.exp_ty),
                    ),
                    $exp1.1,
                );
            }

            lhs_result.exp_ty
        }};
    }

    let ty: DataType = match exp {
        Expression::ArrayIndex(array_exp, index_exp) => {
            let array_result = analyze_expr!(array_exp, DataType::Array(Box::new(DataType::Any)));
            let index_result = analyze_expr!(
                index_exp,
                DataType::Union(vec![
                    DataType::Int,
                    DataType::Array(Box::new(DataType::Int))
                ])
            );

            match array_result.exp_ty {
                DataType::Generic(id) => match scoped_generic_types.get_recursive(id) {
                    DataType::Array(inner) => {
                        match scoped_generic_types.deref_type(&index_result.exp_ty) {
                            DataType::Array(_) => array_result.exp_ty,
                            _ => match *inner {
                                DataType::Any => {
                                    let inner_id = ctx.allocator.next_id();
                                    scoped_generic_types.constrain_generic_type(
                                        id,
                                        DataType::Array(Box::new(DataType::Generic(inner_id))),
                                    );
                                    DataType::Generic(inner_id)
                                }
                                other => other,
                            },
                        }
                    }
                    DataType::Any => {
                        let inner_id = ctx.allocator.next_id();
                        scoped_generic_types.constrain_generic_type(
                            id,
                            DataType::Array(Box::new(DataType::Generic(inner_id))),
                        );
                        match scoped_generic_types.deref_type(&index_result.exp_ty) {
                            DataType::Array(_) => {
                                DataType::Array(Box::new(DataType::Generic(inner_id)))
                            }
                            _ => DataType::Generic(inner_id),
                        }
                    }
                    _ => DataType::Null,
                },
                DataType::Array(ref inner) => {
                    match scoped_generic_types.deref_type(&index_result.exp_ty) {
                        DataType::Array(_) => array_result.exp_ty,
                        _ => *inner.clone(),
                    }
                }
                _ => DataType::Null,
            }
        }
        Expression::Exit(_, exit_code) => {
            if let Some(exit_code) = exit_code {
                analyze_expr!(exit_code, DataType::Int);
            }
            DataType::Null
        }
        Expression::FunctionInvocation(modifiers, (name, name_span), args, failable_handlers) => {
            let fun_symbol = ctx.get_definition_info(name, name_span.start);

            let expected_types = match fun_symbol {
                Some(SymbolInfo {
                    symbol_type: SymbolType::Function(ref fun_sym),
                    ..
                }) => fun_sym
                    .arguments
                    .iter()
                    .map(|(arg, _)| {
                        (
                            arg.data_type.clone(),
                            arg.is_optional,
                            arg.is_ref,
                            arg.default_value_type.clone(),
                        )
                    })
                    .collect::<Vec<_>>(),
                Some(_) => {
                    ctx.report_error(&format!("{name} is not a function"), *exp_span);
                    vec![]
                }
                None => {
                    ctx.report_error(&format!("{name} is not defined"), *exp_span);
                    vec![]
                }
            };

            args.iter().enumerate().for_each(|(idx, arg)| {
                if let Some((ty, _, is_ref, _)) = expected_types.get(idx) {
                    match (is_ref, arg.0.clone()) {
                        (true, Expression::Var((ref_name, ref_span))) => {
                            if let Some(var) = ctx.get_definition_info(&ref_name, ref_span.start) {
                                if let SymbolType::Variable(ref var_sym) = var.symbol_type {
                                    if var_sym.is_const {
                                        ctx.report_error(
                                            "Cannot modify a constant variable",
                                            ref_span,
                                        );
                                    }
                                }
                            }
                        }
                        (true, _) => {
                            ctx.report_error("Cannot pass a non-variable as a reference", arg.1);
                        }
                        _ => {}
                    }

                    let arg_result = analyze_expr!(arg, ty.clone());
                    if let DataType::Generic(id) = ty {
                        scoped_generic_types.constrain_generic_type(*id, arg_result.exp_ty.clone());
                    }
                } else {
                    ctx.report_error(
                        &format!("Function takes only {} arguments", expected_types.len()),
                        arg.1,
                    );
                }
            });

            if expected_types
                .iter()
                .filter(|(_, is_optional, ..)| !*is_optional)
                .count()
                > args.len()
                && fun_symbol.is_some()
            {
                ctx.report_error(
                    &format!("Function takes {} arguments", expected_types.len()),
                    *name_span,
                );
            } else {
                expected_types
                    .iter()
                    .skip(args.len())
                    .filter(|(_, is_optional, ..)| *is_optional)
                    .for_each(|(ty, _, _, default_type)| {
                        if let DataType::Generic(id) = ty {
                            scoped_generic_types
                                .constrain_generic_type(*id, default_type.clone().unwrap());
                        }
                    });
            }

            let exp_ty = fun_symbol
                .clone()
                .map(|f| f.data_type)
                .unwrap_or(DataType::Null);

            let handler_result = analyze_failable_handlers_pure(
                failable_handlers,
                ctx,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= handler_result.is_propagating_failure;
            return_types.extend(handler_result.return_ty);

            let function_call_scope_end = failable_handlers
                .first()
                .map(|(_, s)| s.start - 1)
                .unwrap_or(exp_span.end - 1);

            // Insert function call symbol info
            if let Some(SymbolInfo {
                symbol_type: SymbolType::Function(ref fun_sym),
                ref data_type,
                ..
            }) = fun_symbol
            {
                use chumsky::span::SimpleSpan;

                let mut last_span_end = name_span.end + 1;
                let fun_info = SymbolInfo {
                    name: name.clone(),
                    symbol_type: SymbolType::Function(FunctionSymbol {
                        arguments: fun_sym
                            .arguments
                            .iter()
                            .enumerate()
                            .map(|(idx, (arg, _))| {
                                let arg_span = args
                                    .get(idx)
                                    .map(|(_, s)| *s)
                                    .unwrap_or(SimpleSpan::from(last_span_end..exp_span.end));
                                last_span_end = arg_span.end;
                                (
                                    FunctionArgument {
                                        name: arg.name.clone(),
                                        data_type: scoped_generic_types.deref_type(&arg.data_type),
                                        is_optional: arg.is_optional,
                                        default_value_type: arg.default_value_type.clone(),
                                        is_ref: arg.is_ref,
                                    },
                                    arg_span,
                                )
                            })
                            .collect(),
                        ..fun_sym.clone()
                    }),
                    data_type: scoped_generic_types.deref_type(data_type),
                    is_definition: false,
                    undefined: false,
                    span: *name_span,
                    contexts: contexts.to_vec(),
                };

                ctx.symbol_table
                    .symbols
                    .insert(name_span.start..=name_span.end, fun_info.clone());
                ctx.symbol_table
                    .fun_call_arg_scope
                    .insert(name_span.end..=function_call_scope_end, fun_info);
            }

            // Check for unhandled failable
            let has_failure_handler = failable_handlers.iter().any(|(m, _)| {
                matches!(m, FailableHandler::Failure(_))
                    || matches!(m, FailableHandler::Exited(_, _, _))
            });

            if matches!(
                scoped_generic_types.deref_type(&exp_ty),
                DataType::Failable(_)
            ) && modifiers
                .iter()
                .all(|(m, _)| *m != CommandModifier::Unsafe && *m != CommandModifier::Trust)
                && contexts.iter().all(|c| match c {
                    Context::Block(BlockContext { modifiers }) => modifiers
                        .iter()
                        .all(|m| *m != CommandModifier::Unsafe && *m != CommandModifier::Trust),
                    _ => true,
                })
                && !has_failure_handler
            {
                ctx.report_error(
                    "Failable function must be handled with a failure handler or marked with `trust` modifier",
                    *name_span,
                );
            }

            exp_ty
        }
        Expression::Var((name, name_span)) => {
            ctx.insert_reference(
                name,
                name_span.start,
                name_span.end,
                scoped_generic_types,
                contexts,
            );

            match ctx.get_definition_info(name, name_span.start) {
                Some(info) => {
                    if matches!(info.symbol_type, SymbolType::Function(_)) {
                        ctx.report_error(&format!("{name} is a function"), *name_span);
                    }
                    info.data_type
                }
                None => DataType::Null,
            }
        }
        Expression::Add(exp1, exp2) => {
            analyze_binop_codep!(
                exp1,
                DataType::Union(vec![
                    DataType::Number,
                    DataType::Int,
                    DataType::Text,
                    DataType::Array(Box::new(DataType::Union(vec![
                        DataType::Number,
                        DataType::Int,
                        DataType::Text,
                    ]))),
                ]),
                exp2
            )
        }
        Expression::And(exp1, _, exp2) | Expression::Or(exp1, _, exp2) => {
            analyze_binop_codep!(exp1, DataType::Boolean, exp2);
            DataType::Boolean
        }
        Expression::Array(elements) => {
            let types: Vec<DataType> = elements
                .iter()
                .map(|el| {
                    let result = analyze_expr!(
                        el,
                        DataType::Union(vec![DataType::Number, DataType::Int, DataType::Text])
                    );
                    result.exp_ty
                })
                .collect();

            let array_type = make_union_type(types);

            if let DataType::Union(_) = array_type {
                ctx.report_error("Array must have elements of the same type", *exp_span);
            }

            DataType::Array(Box::new(array_type))
        }
        Expression::Cast(exp, _, (ty, _)) => {
            analyze_expr!(exp, DataType::Any);
            ty.clone()
        }
        Expression::Command(modifiers, inter_cmd, failable_handlers) => {
            inter_cmd.iter().for_each(|(cmd, _)| {
                if let InterpolatedCommand::Expression(exp) = cmd {
                    analyze_expr!(exp, DataType::Any);
                }
            });

            let handler_result = analyze_failable_handlers_pure(
                failable_handlers,
                ctx,
                scoped_generic_types,
                contexts,
            );

            is_propagating_failure |= handler_result.is_propagating_failure;
            return_types.extend(handler_result.return_ty);

            let has_failure_handler = failable_handlers.iter().any(|(m, _)| {
                matches!(m, FailableHandler::Failure(_))
                    || matches!(m, FailableHandler::Exited(_, _, _))
            });

            if !has_failure_handler
                && !modifiers
                    .iter()
                    .any(|(m, _)| *m == CommandModifier::Unsafe || *m == CommandModifier::Trust)
                && !contexts.iter().any(|c| match c {
                    Context::Block(BlockContext { modifiers }) => modifiers
                        .iter()
                        .any(|m| *m == CommandModifier::Unsafe || *m == CommandModifier::Trust),
                    _ => false,
                })
            {
                ctx.report_error("Command must have a failure handler", *exp_span);
            }

            DataType::Text
        }
        Expression::Multiply(exp1, exp2)
        | Expression::Divide(exp1, exp2)
        | Expression::Modulo(exp1, exp2)
        | Expression::Subtract(exp1, exp2) => {
            analyze_binop_codep!(
                exp1,
                DataType::Union(vec![DataType::Number, DataType::Int]),
                exp2
            )
        }
        Expression::Eq(exp1, exp2) | Expression::Neq(exp1, exp2) => {
            analyze_binop_codep!(exp1, DataType::Any, exp2);
            DataType::Boolean
        }
        Expression::Ge(exp1, exp2)
        | Expression::Gt(exp1, exp2)
        | Expression::Le(exp1, exp2)
        | Expression::Lt(exp1, exp2) => {
            let lhs = analyze_expr!(
                exp1,
                DataType::Union(vec![
                    DataType::Number,
                    DataType::Int,
                    DataType::Text,
                    DataType::Array(Box::new(DataType::Union(vec![
                        DataType::Number,
                        DataType::Int,
                        DataType::Text,
                    ]))),
                ])
            );

            let mut left_constrain_ty =
                get_constrain_ty_for_compare_pure(lhs.exp_ty.clone(), scoped_generic_types);
            let rhs = analyze_expr!(exp2, left_constrain_ty.clone());
            let right_constrain_ty =
                get_constrain_ty_for_compare_pure(rhs.exp_ty, scoped_generic_types);

            if let DataType::Generic(id) = lhs.exp_ty.clone() {
                scoped_generic_types.constrain_generic_type(id, right_constrain_ty.clone());
                left_constrain_ty =
                    get_constrain_ty_for_compare_pure(lhs.exp_ty, scoped_generic_types);
            }

            if !matches_type_pure(
                &right_constrain_ty,
                &left_constrain_ty,
                scoped_generic_types,
            ) {
                ctx.report_error(
                    &format!(
                        "Expected type {}, found type {}",
                        scoped_generic_types.to_display_string(&right_constrain_ty),
                        scoped_generic_types.to_display_string(&left_constrain_ty),
                    ),
                    exp1.1,
                );
            }

            DataType::Boolean
        }
        Expression::Is(exp, _, _) => {
            analyze_expr!(exp, DataType::Any);
            DataType::Boolean
        }
        Expression::Nameof(_, exp) => {
            analyze_expr!(exp, DataType::Any);
            DataType::Text
        }
        Expression::Neg(_, exp) => {
            analyze_expr!(exp, DataType::Union(vec![DataType::Number, DataType::Int])).exp_ty
        }
        Expression::Not(_, exp) => {
            analyze_expr!(exp, DataType::Boolean);
            DataType::Boolean
        }
        Expression::Parentheses(exp) => analyze_expr!(exp, DataType::Any).exp_ty,
        Expression::Range(exp1, exp2) => {
            analyze_binop_codep!(exp1, DataType::Int, exp2);
            DataType::Array(Box::new(DataType::Int))
        }
        Expression::Ternary(exp1, _, exp2, _, exp3) => {
            analyze_expr!(exp1, DataType::Boolean);
            let if_true = analyze_expr!(exp2, expected_type.clone()).exp_ty;
            let if_false = analyze_expr!(exp3, expected_type.clone()).exp_ty;
            make_union_type(vec![if_true, if_false])
        }
        Expression::Text(int_text) => {
            int_text.iter().for_each(|(text, _)| {
                if let InterpolatedText::Expression(exp) = text {
                    analyze_expr!(exp, DataType::Any);
                }
            });
            DataType::Text
        }
        Expression::Number(_) => DataType::Number,
        Expression::Int(_) => DataType::Int,
        Expression::Boolean(_) => DataType::Boolean,
        Expression::Null => DataType::Null,
        Expression::Status => {
            ctx.symbol_table.symbols.insert(
                exp_span_inclusive,
                SymbolInfo {
                    name: "status".to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                    data_type: DataType::Int,
                    is_definition: false,
                    undefined: false,
                    span: *exp_span,
                    contexts: contexts.to_vec(),
                },
            );
            DataType::Int
        }
        Expression::Error => DataType::Any,
    };

    // Final type check against expected type
    if !matches_type_pure(&expected_type, &ty, scoped_generic_types) {
        ctx.report_error(
            &format!(
                "Expected type `{}`, found type `{}`",
                scoped_generic_types.to_display_string(&expected_type),
                scoped_generic_types.to_display_string(&ty),
            ),
            *exp_span,
        );
    } else if let DataType::Generic(id) = ty {
        scoped_generic_types.constrain_generic_type(id, expected_type.clone());
    }

    ExpAnalysisResult {
        exp_ty: ty,
        is_propagating_failure,
        return_ty: if return_types.is_empty() {
            None
        } else {
            Some(make_union_type(return_types))
        },
    }
}

// ─── Helpers ──────────────────────────────────────────────────────────────────

fn propagate_nested_generics_pure(ty: &DataType, from: &PureGenericsMap, to: &mut PureGenericsMap) {
    match ty {
        DataType::Generic(id) => {
            let inner_ty = from.get(*id);
            to.constrain_generic_type(*id, inner_ty.clone());
            to.mark_as_inferred(*id);
            propagate_nested_generics_pure(&inner_ty, from, to);
        }
        DataType::Array(inner) => propagate_nested_generics_pure(inner, from, to),
        DataType::Failable(inner) => propagate_nested_generics_pure(inner, from, to),
        DataType::Union(types) => {
            for t in types {
                propagate_nested_generics_pure(t, from, to);
            }
        }
        _ => {}
    }
}

fn get_constrain_ty_for_compare_pure(constrain: DataType, generics: &PureGenericsMap) -> DataType {
    match constrain.clone() {
        DataType::Generic(id) => get_constrain_ty_for_compare_pure(generics.get(id), generics),
        DataType::Int => DataType::Union(vec![DataType::Int, DataType::Number]),
        DataType::Number => DataType::Union(vec![DataType::Int, DataType::Number]),
        DataType::Union(types) => DataType::Union(
            types
                .iter()
                .map(|ty| get_constrain_ty_for_compare_pure(ty.clone(), generics))
                .collect(),
        ),
        ty => ty,
    }
}

fn handle_doc_strings(docs: &String, contexts: &mut Vec<Context>) -> StmntAnalysisResult {
    match contexts.last() {
        Some(Context::DocString(doc_string)) => {
            let new_doc_string = format!("{doc_string}\n{docs}");
            *contexts.last_mut().unwrap() = Context::DocString(new_doc_string);
        }
        _ => {
            contexts.push(Context::DocString(docs.clone()));
        }
    }

    StmntAnalysisResult {
        is_propagating_failure: false,
        return_ty: None,
    }
}

fn get_stmnt_analysis_result(
    stmnt_analysis: Vec<StmntAnalysisResult>,
    exp_analysis: Vec<ExpAnalysisResult>,
) -> StmntAnalysisResult {
    let mut is_propagating_failure = false;
    let mut return_ty = vec![];

    for stmnt in stmnt_analysis {
        if stmnt.is_propagating_failure {
            is_propagating_failure = true;
        }
        if let Some(ty) = stmnt.return_ty {
            return_ty.push(ty);
        }
    }

    for exp in exp_analysis {
        if exp.is_propagating_failure {
            is_propagating_failure = true;
        }
        if let Some(ty) = exp.return_ty {
            return_ty.push(ty);
        }
    }

    StmntAnalysisResult {
        is_propagating_failure,
        return_ty: if !return_ty.is_empty() {
            Some(make_union_type(return_ty))
        } else {
            None
        },
    }
}

// ─── Utility ──────────────────────────────────────────────────────────────────

/// Generate a deterministic file key from a path string.
pub fn file_key_for_path(path: &str) -> (FileId, FileVersion) {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{
        Hash,
        Hasher,
    };
    let mut h = DefaultHasher::new();
    path.hash(&mut h);
    (FileId(h.finish() as usize), FileVersion(0))
}
