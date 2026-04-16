use std::collections::HashMap;

use crate::alpha060::exp::ExpAnalysisResult;
use crate::alpha060::stmnts::is_terminating_statement;
use crate::files::FileVersion;
use crate::stdlib::is_builtin_file;
use crate::types::{
    make_union_type,
    matches_type,
    DataType,
    GenericsMap,
};
use crate::{
    import_symbol,
    insert_symbol_definition,
    map_import_path,
    AnalysisHost,
    BlockContext,
    Context,
    FunctionContext,
    FunctionSymbol,
    ImportContext,
    SymbolInfo,
    SymbolLocation,
    SymbolType,
    VariableSymbol,
};
use amber_grammar::alpha060::{
    Block,
    FunctionArgument,
    GlobalStatement,
    ImportContent,
    Statement,
    VariableInitType,
};
use amber_grammar::{
    CommandModifier,
    CompilerFlag,
    Span,
    Spanned,
};
use amber_types::paths::FileId;

use super::exp::analyze_exp;
use super::stmnts::{
    analyze_stmnt,
    StmntAnalysisResult,
};

/// Walk a type tree and propagate any nested `Generic(id)` entries from `from` into `to`,
/// marking them as inferred so they can be resolved at call sites.
///
/// Only propagates generics that are **not yet inferred** in `to`.  A generic
/// that was already marked inferred belongs to a different function definition's
/// scope (e.g. a builtin function's parameter) and must not be overwritten with the
/// constraint that was accumulated in the current scope.
fn propagate_nested_generics(ty: &DataType, from: &GenericsMap, to: &GenericsMap) {
    match ty {
        DataType::Generic(id) => {
            // Skip generics already established by their own function's analysis.
            if to.is_inferred(*id) {
                return;
            }
            let inner_ty = from.get(*id);
            to.constrain_generic_type(*id, inner_ty.clone());
            to.mark_as_inferred(*id);
            propagate_nested_generics(&inner_ty, from, to);
        }
        DataType::Array(inner) => propagate_nested_generics(inner, from, to),
        DataType::Failable(inner) => propagate_nested_generics(inner, from, to),
        DataType::Union(types) => {
            types
                .iter()
                .for_each(|ty| propagate_nested_generics(ty, from, to));
        }
        _ => {}
    }
}

#[tracing::instrument(skip_all)]
pub async fn analyze_global_stmnt(
    file_id: FileId,
    file_version: FileVersion,
    ast: &[Spanned<GlobalStatement>],
    backend: &impl AnalysisHost,
) {
    let uri = backend.get_files().lookup(&file_id);

    let mut default_imports = vec![];

    if !is_builtin_file(&uri, backend.get_amber_version().clone()) {
        default_imports.push((
            GlobalStatement::Import(
                (false, Span::from(0..0)),
                ("import".to_string(), Span::from(0..0)),
                (ImportContent::ImportAll, Span::from(0..0)),
                ("from".to_string(), Span::from(0..0)),
                ("builtin".to_string(), Span::from(0..0)),
            ),
            Span::from(0..0),
        ))
    }

    // Phase 1: Hoist all function declarations so they are visible throughout
    // the file. This enables recursive calls and forward references.
    for (global, _span) in default_imports.iter().chain(ast.iter()) {
        if let GlobalStatement::FunctionDefinition(
            compiler_flags,
            (is_pub, _),
            _,
            (name, name_span),
            args,
            declared_return_ty,
            _body,
        ) = global
        {
            let scoped_generics_map = backend.get_files().generic_types.clone();
            let mut hoisted_generic_types = vec![];

            let hoisted_args: Vec<Spanned<crate::FunctionArgument>> = args
                .iter()
                .filter_map(|(arg, arg_span)| match arg {
                    FunctionArgument::Generic((is_ref, _), (arg_name, _)) => {
                        let generic_id = scoped_generics_map.new_generic_id();
                        scoped_generics_map.constrain_generic_type(generic_id, DataType::Any);
                        hoisted_generic_types.push(generic_id);
                        Some((
                            crate::FunctionArgument {
                                name: arg_name.clone(),
                                data_type: DataType::Generic(generic_id),
                                is_optional: false,
                                default_value_type: None,
                                is_ref: *is_ref,
                            },
                            *arg_span,
                        ))
                    }
                    FunctionArgument::Typed((is_ref, _), (arg_name, _), (ty, _)) => Some((
                        crate::FunctionArgument {
                            name: arg_name.clone(),
                            data_type: ty.clone(),
                            is_optional: false,
                            default_value_type: None,
                            is_ref: *is_ref,
                        },
                        *arg_span,
                    )),
                    FunctionArgument::Optional((is_ref, _), (arg_name, _), ty, _exp) => {
                        let data_type = match ty {
                            Some((ty, _)) => ty.clone(),
                            None => {
                                let generic_id = scoped_generics_map.new_generic_id();
                                scoped_generics_map
                                    .constrain_generic_type(generic_id, DataType::Any);
                                hoisted_generic_types.push(generic_id);
                                DataType::Generic(generic_id)
                            }
                        };
                        Some((
                            crate::FunctionArgument {
                                name: arg_name.clone(),
                                data_type,
                                is_optional: true,
                                default_value_type: None,
                                is_ref: *is_ref,
                            },
                            *arg_span,
                        ))
                    }
                    FunctionArgument::Error => None,
                })
                .collect();

            // Propagate hoisted generics to the global generics map
            hoisted_generic_types.iter().for_each(|generic_id| {
                let ty = scoped_generics_map.get(*generic_id);
                backend
                    .get_files()
                    .generic_types
                    .constrain_generic_type(*generic_id, ty.clone());
                backend
                    .get_files()
                    .generic_types
                    .mark_as_inferred(*generic_id);
            });

            let return_type = match declared_return_ty {
                Some((ty, _)) => ty.clone(),
                None => DataType::Any,
            };

            let mut symbol_table = backend
                .get_files()
                .symbol_table
                .entry((file_id, file_version))
                .or_default();

            insert_symbol_definition(
                &mut symbol_table,
                &SymbolInfo {
                    name: name.to_string(),
                    symbol_type: SymbolType::Function(FunctionSymbol {
                        arguments: hoisted_args,
                        is_public: *is_pub,
                        compiler_flags: compiler_flags
                            .iter()
                            .map(|(flag, _)| flag.clone())
                            .collect(),
                        docs: None,
                    }),
                    data_type: return_type,
                    is_definition: true,
                    undefined: false,
                    span: *name_span,
                    contexts: vec![],
                },
                (file_id, file_version),
                0..=usize::MAX,
                *is_pub,
            );
        }
    }

    // Phase 2: Full analysis of all global statements.
    // When a function with no declared return type calls a forward-referenced
    // function that hasn't been analysed yet, its return type is inferred as
    // `Any` (the hoisted placeholder).  After the first pass all functions have
    // concrete return types so a second pass resolves those forward references.
    let mut is_reanalysis = false;
    let mut seen_test_names: HashMap<String, Span> = HashMap::new();
    'analysis: loop {
        let mut contexts = vec![];
        for (global, span) in default_imports.iter().chain(ast.iter()) {
            match global {
                GlobalStatement::Import(..) if is_reanalysis => continue,
                GlobalStatement::FunctionDefinition(
                    compiler_flags,
                    (is_pub, _),
                    _,
                    (name, name_span),
                    args,
                    declared_return_ty,
                    body,
                ) => {
                    // We create scoped generics map, to not overwrite other generics, not defined here
                    let scoped_generics_map = backend.get_files().generic_types.clone();

                    let mut new_generic_types = vec![];
                    let mut prev_arg_optional = false;
                    let mut seen_param_names = std::collections::HashSet::new();

                    args.iter().for_each(|(arg, _)| {
                        let (name, ty, name_span) = match arg {
                            FunctionArgument::Generic(_, (name, span)) => {
                                let generic_id = scoped_generics_map.new_generic_id();

                                scoped_generics_map
                                    .constrain_generic_type(generic_id, DataType::Any);
                                new_generic_types.push(generic_id);

                                if prev_arg_optional {
                                    backend.get_files().report_error(
                                        &(file_id, file_version),
                                        "Optional argument must be the last one",
                                        *span,
                                    );
                                }

                                (name, DataType::Generic(generic_id), span)
                            }
                            FunctionArgument::Typed(_, (name, span), (ty, _)) => {
                                if prev_arg_optional {
                                    backend.get_files().report_error(
                                        &(file_id, file_version),
                                        "Optional argument must be the last one",
                                        *span,
                                    );
                                }

                                (name, ty.clone(), span)
                            }
                            FunctionArgument::Optional((is_ref, _), (name, span), ty, exp) => {
                                prev_arg_optional = true;

                                if *is_ref {
                                    backend.get_files().report_error(
                                        &(file_id, file_version),
                                        "Optional argument cannot be a reference",
                                        *span,
                                    );
                                }

                                (
                                    name,
                                    match ty {
                                        Some((ty, _)) => {
                                            analyze_exp(
                                                file_id,
                                                file_version,
                                                exp,
                                                ty.clone(),
                                                backend.get_files(),
                                                &backend.get_files().generic_types.clone(),
                                                &vec![],
                                            );

                                            ty.clone()
                                        }
                                        None => {
                                            let generic_id = scoped_generics_map.new_generic_id();

                                            scoped_generics_map
                                                .constrain_generic_type(generic_id, DataType::Any);
                                            new_generic_types.push(generic_id);

                                            DataType::Generic(generic_id)
                                        }
                                    },
                                    span,
                                )
                            }
                            FunctionArgument::Error => return,
                        };

                        if !seen_param_names.insert(name.clone()) {
                            backend.get_files().report_error(
                                &(file_id, file_version),
                                &format!("Duplicate parameter name '{name}'"),
                                *name_span,
                            );
                        }

                        let mut symbol_table = match backend
                            .get_files()
                            .symbol_table
                            .get_mut(&(file_id, file_version))
                        {
                            Some(symbol_table) => symbol_table,
                            None => {
                                tracing::warn!(
                                    "Symbol table not found for file: {:?}, version: {}",
                                    file_id,
                                    file_version.0,
                                );
                                return;
                            }
                        };

                        insert_symbol_definition(
                            &mut symbol_table,
                            &SymbolInfo {
                                name: name.to_string(),
                                symbol_type: SymbolType::Variable(VariableSymbol {
                                    is_const: false,
                                    is_public: false,
                                }),
                                data_type: ty,
                                is_definition: true,
                                undefined: false,
                                span: *name_span,
                                contexts: vec![],
                            },
                            (file_id, file_version),
                            name_span.end..=span.end,
                            false,
                        );
                    });

                    let mut return_types = vec![];
                    let mut is_propagating = false;

                    let mut function_contexts = vec![Context::Function(FunctionContext {
                        compiler_flags: vec![],
                    })];

                    let mut terminator_seen = false;

                    body.iter().for_each(|stmnt| {
                        if terminator_seen
                            && !matches!(stmnt.0, Statement::Comment(_) | Statement::Error)
                        {
                            backend.get_files().report_unused(
                                &(file_id, file_version),
                                "Unreachable code",
                                stmnt.1,
                            );
                        }

                        let StmntAnalysisResult {
                            return_ty,
                            is_propagating_failure,
                        } = analyze_stmnt(
                            file_id,
                            file_version,
                            stmnt,
                            backend.get_files(),
                            span.end,
                            &scoped_generics_map,
                            &mut function_contexts,
                        );

                        is_propagating |= is_propagating_failure;
                        return_types.extend(return_ty);

                        // Mark that we've seen a terminator in this block
                        if !terminator_seen && is_terminating_statement(&stmnt.0) {
                            terminator_seen = true;
                        }
                    });

                    new_generic_types.iter().for_each(|generic_id| {
                        let ty = scoped_generics_map.get(*generic_id);
                        backend
                            .get_files()
                            .generic_types
                            .constrain_generic_type(*generic_id, ty.clone());
                        backend
                            .get_files()
                            .generic_types
                            .mark_as_inferred(*generic_id);
                        // Also propagate any nested generics created during body analysis
                        // (e.g. inner element-type generics from array indexing).
                        propagate_nested_generics(
                            &ty,
                            &scoped_generics_map,
                            &backend.get_files().generic_types,
                        );
                    });

                    // When return type is not declared, the hoisted placeholder
                    // type is `Any`.  Recursive (and forward) calls resolve to
                    // that placeholder, injecting spurious `Any` into the
                    // collected return types (e.g. `Int | Any` instead of `Int`).
                    // Filter those out so the inferred type is based only on
                    // concrete return expressions.
                    if declared_return_ty.is_none() {
                        let concrete: Vec<DataType> = return_types
                            .iter()
                            .filter(|t| !matches!(t, DataType::Any))
                            .cloned()
                            .collect();
                        if !concrete.is_empty() {
                            return_types = concrete;
                        }
                    }

                    let mut inferred_return_type = match return_types.len() {
                        0 => DataType::Null,
                        _ => {
                            if !terminator_seen {
                                return_types.push(DataType::Null);
                            }
                            make_union_type(return_types)
                        }
                    };

                    if is_propagating && !matches!(inferred_return_type, DataType::Failable(_)) {
                        inferred_return_type = DataType::Failable(Box::new(inferred_return_type));
                    }

                    let data_type = match declared_return_ty {
                        Some((ty, ty_span)) => {
                            if !matches_type(
                                ty,
                                &inferred_return_type,
                                &backend.get_files().generic_types,
                            ) {
                                backend.get_files().report_error(
                                &(file_id, file_version),
                                &format!(
                                    "Function returns type {inferred_return_type:?}, but expected {ty:?}",
                                ),
                                *ty_span,
                            );
                            }

                            if is_propagating && !matches!(ty, DataType::Failable(_)) {
                                backend.get_files().report_error(
                                &(file_id, file_version),
                                "Function is propagating an error, but return type is not failable",
                                *ty_span,
                            );
                            }

                            if !is_propagating && matches!(ty, DataType::Failable(_)) {
                                backend.get_files().report_error(
                                &(file_id, file_version),
                                "Return type is declared as failable, but the function body does not propagate any failures",
                                *ty_span,
                            );
                            }

                            ty.clone()
                        }
                        None => inferred_return_type,
                    };

                    let mut symbol_table = backend
                        .get_files()
                        .symbol_table
                        .entry((file_id, file_version))
                        .or_default();

                    insert_symbol_definition(
                        &mut symbol_table,
                        &SymbolInfo {
                            name: name.to_string(),
                            symbol_type: SymbolType::Function(FunctionSymbol {
                                arguments: args
                                    .iter()
                                    .filter_map(|(arg, span)| match arg {
                                        FunctionArgument::Generic((is_ref, _), (name, _)) => {
                                            Some((
                                                crate::FunctionArgument {
                                                    name: name.clone(),
                                                    data_type: DataType::Generic(
                                                        new_generic_types.remove(0),
                                                    ),
                                                    is_optional: false,
                                                    default_value_type: None,
                                                    is_ref: *is_ref,
                                                },
                                                *span,
                                            ))
                                        }
                                        FunctionArgument::Typed(
                                            (is_ref, _),
                                            (name, _),
                                            (ty, _),
                                        ) => Some((
                                            crate::FunctionArgument {
                                                name: name.clone(),
                                                data_type: ty.clone(),
                                                is_optional: false,
                                                default_value_type: None,
                                                is_ref: *is_ref,
                                            },
                                            *span,
                                        )),
                                        FunctionArgument::Optional(
                                            (is_ref, _),
                                            (name, _),
                                            ty,
                                            exp,
                                        ) => Some((
                                            crate::FunctionArgument {
                                                name: name.clone(),
                                                data_type: match ty {
                                                    Some((ty, _)) => ty.clone(),
                                                    None => DataType::Generic(
                                                        new_generic_types.remove(0),
                                                    ),
                                                },
                                                is_optional: true,
                                                default_value_type: {
                                                    let ExpAnalysisResult { exp_ty, .. } =
                                                        analyze_exp(
                                                            file_id,
                                                            file_version,
                                                            exp,
                                                            DataType::Any,
                                                            backend.get_files(),
                                                            &scoped_generics_map,
                                                            &function_contexts,
                                                        );

                                                    Some(exp_ty)
                                                },
                                                is_ref: *is_ref,
                                            },
                                            *span,
                                        )),
                                        FunctionArgument::Error => None,
                                    })
                                    .collect::<Vec<_>>(),
                                is_public: *is_pub,
                                compiler_flags: compiler_flags
                                    .iter()
                                    .map(|(flag, _)| flag.clone())
                                    .collect(),
                                docs: match contexts.clone().last() {
                                    Some(Context::DocString(doc)) => {
                                        contexts.pop();
                                        Some(doc.clone())
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
                        (file_id, file_version),
                        span.end..=usize::MAX,
                        *is_pub,
                    );

                    symbol_table
                        .function_body_ranges
                        .insert(name_span.start, span.end);
                }
                GlobalStatement::Import(
                    (is_public_import, _),
                    _,
                    (import_content, _),
                    _,
                    (path, path_span),
                ) => {
                    let uri = &backend.get_files().lookup(&file_id);

                    let result = backend
                        .open_document(&map_import_path(uri, path, backend).await)
                        .await;

                    {
                        let mut symbol_table = backend
                            .get_files()
                            .symbol_table
                            .entry((file_id, file_version))
                            .or_default();

                        insert_symbol_definition(
                            &mut symbol_table,
                            &SymbolInfo {
                                name: path.to_string(),
                                symbol_type: SymbolType::ImportPath,
                                data_type: DataType::Text,
                                is_definition: true,
                                undefined: false,
                                span: *path_span,
                                contexts: vec![],
                            },
                            result.clone().unwrap_or((file_id, file_version)),
                            path_span.start..=path_span.end,
                            false,
                        );
                    }

                    if result.is_err() {
                        backend.get_files().report_error(
                            &(file_id, file_version),
                            "File doesn't exist",
                            *path_span,
                        );

                        continue;
                    }

                    let imported_file = result.clone().unwrap();

                    if backend.get_files().is_depending_on(&imported_file, file_id) {
                        backend.get_files().report_error(
                            &(file_id, file_version),
                            "Circular dependency",
                            *path_span,
                        );

                        continue;
                    }

                    backend
                        .get_files()
                        .add_file_dependency(&(file_id, file_version), imported_file.0);

                    // Extract only the public definitions (Arc clone) instead of
                    // cloning the entire SymbolTable.
                    let imported_public_defs =
                        match backend.get_files().symbol_table.get(&imported_file) {
                            Some(symbol_table_ref) => symbol_table_ref.public_definitions.clone(),
                            None => continue,
                        };

                    match import_content {
                        ImportContent::ImportSpecific(ident_list) => {
                            let mut import_context = ImportContext {
                                public_definitions: imported_public_defs.clone(),
                                imported_symbols: vec![],
                                statement_span: Some(*span),
                            };

                            ident_list.iter().for_each(|(ident, span)| {
                                if import_context.imported_symbols.contains(&ident.to_string()) {
                                    backend.get_files().report_error(
                                        &(file_id, file_version),
                                        &format!("Duplicate import '{ident}'"),
                                        *span,
                                    );

                                    let mut symbol_table = backend
                                        .get_files()
                                        .symbol_table
                                        .entry((file_id, file_version))
                                        .or_default();

                                    symbol_table.symbols.insert(
                                        span.start..=span.end,
                                        SymbolInfo {
                                            name: ident.to_string(),
                                            symbol_type: SymbolType::Variable(VariableSymbol {
                                                is_const: false,
                                                is_public: false,
                                            }),
                                            data_type: DataType::Null,
                                            is_definition: false,
                                            undefined: true,
                                            span: Span::from(span.start..span.end),
                                            contexts: vec![Context::Import(import_context.clone())],
                                        },
                                    );
                                    return;
                                }

                                let symbol_definition = imported_public_defs.get(ident);

                                match symbol_definition {
                                    Some(definition_location) => {
                                        // Use DashMap guard directly instead of cloning entire SymbolTable.
                                        let symbol_info = {
                                            let definition_file_st = match backend
                                                .get_files()
                                                .symbol_table
                                                .get(&definition_location.file)
                                            {
                                                Some(st) => st,
                                                None => return,
                                            };
                                            match definition_file_st
                                                .symbols
                                                .get(&definition_location.start)
                                            {
                                                Some(si) => si.clone(),
                                                None => return,
                                            }
                                        };

                                        let mut symbol_table = backend
                                            .get_files()
                                            .symbol_table
                                            .entry((file_id, file_version))
                                            .or_default();

                                        import_symbol(
                                            &mut symbol_table,
                                            &SymbolInfo {
                                                is_definition: false,
                                                contexts: vec![Context::Import(
                                                    import_context.clone(),
                                                )],
                                                ..symbol_info
                                            },
                                            Some(span.start..=span.end),
                                            definition_location,
                                            *is_public_import,
                                        );

                                        import_context.imported_symbols.push(ident.to_string());
                                    }
                                    None => {
                                        backend.get_files().report_error(
                                            &(file_id, file_version),
                                            &format!("Could not resolve '{ident}'"),
                                            *span,
                                        );

                                        let mut symbol_table = backend
                                            .get_files()
                                            .symbol_table
                                            .entry((file_id, file_version))
                                            .or_default();

                                        symbol_table.symbols.insert(
                                            span.start..=span.end,
                                            SymbolInfo {
                                                name: ident.to_string(),
                                                symbol_type: SymbolType::Variable(VariableSymbol {
                                                    is_const: false,
                                                    is_public: false,
                                                }),
                                                data_type: DataType::Null,
                                                is_definition: false,
                                                undefined: true,
                                                span: Span::from(span.start..span.end),
                                                contexts: vec![Context::Import(
                                                    import_context.clone(),
                                                )],
                                            },
                                        );
                                    }
                                };
                            });
                        }
                        ImportContent::ImportAll => {
                            let mut all_imported: Vec<(String, SymbolLocation)> = Vec::new();

                            imported_public_defs.iter().for_each(|(name, location)| {
                                // Use DashMap guard directly instead of cloning entire SymbolTable.
                                let symbol_info = {
                                    let definition_file_st = match backend
                                        .get_files()
                                        .symbol_table
                                        .get(&location.file)
                                    {
                                        Some(st) => st,
                                        None => return,
                                    };
                                    match definition_file_st.symbols.get(&location.start) {
                                        Some(si) => si.clone(),
                                        None => return,
                                    }
                                };

                                let mut symbol_table = backend
                                    .get_files()
                                    .symbol_table
                                    .entry((file_id, file_version))
                                    .or_default();

                                import_symbol(
                                    &mut symbol_table,
                                    &SymbolInfo {
                                        is_definition: false,
                                        contexts: vec![Context::Import(ImportContext {
                                            public_definitions: imported_public_defs.clone(),
                                            imported_symbols: vec![],
                                            statement_span: Some(*span),
                                        })],
                                        ..symbol_info
                                    },
                                    None,
                                    location,
                                    *is_public_import,
                                );

                                all_imported.push((name.clone(), location.clone()));
                            });

                            if !*is_public_import {
                                let mut symbol_table = backend
                                    .get_files()
                                    .symbol_table
                                    .entry((file_id, file_version))
                                    .or_default();

                                symbol_table
                                    .import_all_statements
                                    .push((*span, all_imported));
                            }
                        }
                    }
                }
                GlobalStatement::Main(_, args, (body, body_span)) => {
                    if let Some((args, args_span)) = args {
                        let mut symbol_table = backend
                            .get_files()
                            .symbol_table
                            .entry((file_id, file_version))
                            .or_default();

                        insert_symbol_definition(
                            &mut symbol_table,
                            &SymbolInfo {
                                name: args.to_string(),
                                symbol_type: SymbolType::Variable(VariableSymbol {
                                    is_const: false,
                                    is_public: false,
                                }),
                                data_type: DataType::Array(Box::new(DataType::Text)),
                                is_definition: true,
                                undefined: false,
                                span: *args_span,
                                contexts: vec![],
                            },
                            (file_id, file_version),
                            args_span.end..=span.end,
                            false,
                        );
                    }

                    let mut contexts = vec![Context::Main];

                    match body {
                        Block::Block(..) => {
                            analyze_stmnt(
                                file_id,
                                file_version,
                                &(Statement::Block((body.clone(), *body_span)), *body_span),
                                backend.get_files(),
                                body_span.end,
                                &backend.get_files().generic_types.clone(),
                                &mut contexts,
                            );
                        }
                        Block::Singleline(stmnt) => {
                            analyze_stmnt(
                                file_id,
                                file_version,
                                stmnt,
                                backend.get_files(),
                                body_span.end,
                                &backend.get_files().generic_types.clone(),
                                &mut contexts,
                            );
                        }
                        Block::Error => {}
                    }
                }
                GlobalStatement::Statement(stmnt) => {
                    analyze_stmnt(
                        file_id,
                        file_version,
                        stmnt,
                        backend.get_files(),
                        usize::MAX,
                        &backend.get_files().generic_types.clone(),
                        &mut contexts,
                    );
                }
                GlobalStatement::TestBlock(_, name, body) => {
                    // Validate unique test names
                    if let Some((test_name, test_name_span)) = name {
                        if !test_name.is_empty() {
                            let already_seen = seen_test_names.contains_key(test_name);
                            if already_seen {
                                backend.get_files().report_error(
                                    &(file_id, file_version),
                                    &format!("Duplicate test name '{test_name}'"),
                                    *test_name_span,
                                );
                            } else {
                                seen_test_names.insert(test_name.clone(), *test_name_span);
                            }
                        }
                    }

                    let mut test_contexts = vec![Context::Block(BlockContext {
                        modifiers: vec![CommandModifier::Trust],
                    })];
                    body.iter().for_each(|stmnt| {
                        analyze_stmnt(
                            file_id,
                            file_version,
                            stmnt,
                            backend.get_files(),
                            span.end,
                            &backend.get_files().generic_types.clone(),
                            &mut test_contexts,
                        );
                    });
                }
                GlobalStatement::PublicConstInit((_is_pub, _), _, (name, name_span), value) => {
                    let exp = analyze_exp(
                        file_id,
                        file_version,
                        value,
                        DataType::Any,
                        backend.get_files(),
                        &backend.get_files().generic_types.clone(),
                        &vec![],
                    );

                    let var_type = match exp.exp_ty {
                        DataType::Failable(ty) => backend.get_files().generic_types.deref_type(&ty),
                        ty => backend.get_files().generic_types.deref_type(&ty),
                    };

                    let mut symbol_table = backend
                        .get_files()
                        .symbol_table
                        .entry((file_id, file_version))
                        .or_default();

                    insert_symbol_definition(
                        &mut symbol_table,
                        &SymbolInfo {
                            name: name.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol {
                                is_const: true,
                                is_public: true,
                            }),
                            data_type: var_type,
                            is_definition: true,
                            undefined: false,
                            span: *name_span,
                            contexts: vec![],
                        },
                        (file_id, file_version),
                        span.end..=usize::MAX,
                        true,
                    );
                }
                GlobalStatement::PublicVarInit(
                    compiler_flags,
                    (_is_pub, pub_span),
                    _,
                    (name, name_span),
                    (value, _),
                ) => {
                    let has_allow_public_mutable = compiler_flags
                        .iter()
                        .any(|(flag, _)| *flag == CompilerFlag::AllowPublicMutable);

                    if !has_allow_public_mutable {
                        backend.get_files().report_error(
                            &(file_id, file_version),
                            "Public mutable variables require `#[allow_public_mutable]`",
                            *pub_span,
                        );
                    }

                    let exp = match value {
                        VariableInitType::Expression(exp) => analyze_exp(
                            file_id,
                            file_version,
                            exp,
                            DataType::Any,
                            backend.get_files(),
                            &backend.get_files().generic_types.clone(),
                            &vec![],
                        ),
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
                        DataType::Failable(ty) => backend.get_files().generic_types.deref_type(&ty),
                        ty => backend.get_files().generic_types.deref_type(&ty),
                    };

                    let mut symbol_table = backend
                        .get_files()
                        .symbol_table
                        .entry((file_id, file_version))
                        .or_default();

                    insert_symbol_definition(
                        &mut symbol_table,
                        &SymbolInfo {
                            name: name.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol {
                                is_const: false,
                                is_public: true,
                            }),
                            data_type: var_type,
                            is_definition: true,
                            undefined: false,
                            span: *name_span,
                            contexts: vec![],
                        },
                        (file_id, file_version),
                        span.end..=usize::MAX,
                        true,
                    );
                }
            }
        }

        // After the first pass, check whether any function without a declared
        // return type was inferred as `Any` – this indicates an unresolved
        // forward reference.  If so, clear diagnostics (which may contain stale
        // type-mismatch messages based on the placeholder `Any`) and re-analyse
        // the entire file so that the now-known callee return types propagate.
        if !is_reanalysis {
            let has_unresolved_forward_refs = ast.iter().any(|(global, _)| {
                if let GlobalStatement::FunctionDefinition(
                    _,
                    _,
                    _,
                    (_name, name_span),
                    _,
                    None,
                    _,
                ) = global
                {
                    let symbol_table = backend
                        .get_files()
                        .symbol_table
                        .get(&(file_id, file_version));
                    if let Some(st) = symbol_table {
                        if let Some(sym) = st.symbols.get(&name_span.start) {
                            return matches!(sym.data_type, DataType::Any);
                        }
                    }
                    false
                } else {
                    false
                }
            });
            if has_unresolved_forward_refs {
                is_reanalysis = true;
                seen_test_names.clear();
                backend.get_files().errors.remove(&(file_id, file_version));
                backend
                    .get_files()
                    .warnings
                    .remove(&(file_id, file_version));
                backend
                    .get_files()
                    .unused_diagnostics
                    .remove(&(file_id, file_version));
                continue 'analysis;
            }
        }

        break;
    } // 'analysis loop
}
