use crate::analysis::types::{
    make_union_type,
    matches_type,
    DataType,
};
use crate::analysis::{
    self,
    import_symbol,
    insert_symbol_definition,
    map_import_path,
    Context,
    FunctionContext,
    FunctionSymbol,
    ImportContext,
    SymbolInfo,
    SymbolType,
    VariableSymbol,
};
use crate::backend::Backend;
use crate::files::FileVersion;
use crate::grammar::alpha035::{
    FunctionArgument,
    GlobalStatement,
    ImportContent,
};
use crate::grammar::{
    Span,
    Spanned,
};
use crate::paths::FileId;

use super::exp::analyze_exp;
use super::stmnts::{
    analyze_stmnt,
    StmntAnalysisResult,
};

#[tracing::instrument(skip_all)]
pub async fn analyze_global_stmnt(
    file_id: FileId,
    file_version: FileVersion,
    ast: &[Spanned<GlobalStatement>],
    backend: &Backend,
) {
    let mut contexts = vec![];

    for (global, span) in ast.iter() {
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
                // We create scoped generics map, to not overwrite other generics, not defined here
                let scoped_generics_map = backend.files.generic_types.clone();

                let mut new_generic_types = vec![];
                let mut prev_arg_optional = false;

                args.iter().for_each(|(arg, _)| {
                    let (name, ty, name_span) = match arg {
                        FunctionArgument::Generic(_, (name, span)) => {
                            let generic_id = scoped_generics_map.new_generic_id();

                            scoped_generics_map.constrain_generic_type(generic_id, DataType::Any);
                            new_generic_types.push(generic_id);

                            if prev_arg_optional {
                                backend.files.report_error(
                                    &(file_id, file_version),
                                    "Optional argument must be the last one",
                                    *span,
                                );
                            }

                            (name, DataType::Generic(generic_id), span)
                        }
                        FunctionArgument::Typed(_, (name, span), (ty, _)) => {
                            if prev_arg_optional {
                                backend.files.report_error(
                                    &(file_id, file_version),
                                    "Optional argument must be the last one",
                                    *span,
                                );
                            }

                            (name, ty.clone(), span)
                        }
                        FunctionArgument::Optional(_, (name, span), ty, exp) => {
                            prev_arg_optional = true;
                            (
                                name,
                                match ty {
                                    Some((ty, _)) => {
                                        analyze_exp(
                                            file_id,
                                            file_version,
                                            exp,
                                            ty.clone(),
                                            &backend.files,
                                            &backend.files.generic_types.clone(),
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

                    let mut symbol_table =
                        match backend.files.symbol_table.get_mut(&(file_id, file_version)) {
                            Some(symbol_table) => symbol_table,
                            None => {
                                tracing::warn!(
                                    "Symbol table not found for file: {:?} version: {}",
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
                            symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
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

                body.iter().for_each(|stmnt| {
                    let StmntAnalysisResult {
                        return_ty,
                        is_propagating_failure,
                    } = analyze_stmnt(
                        file_id,
                        file_version,
                        stmnt,
                        &backend.files,
                        span.end,
                        &scoped_generics_map,
                        &mut function_contexts,
                    );

                    is_propagating |= is_propagating_failure;
                    return_types.extend(return_ty);
                });

                new_generic_types.iter().for_each(|generic_id| {
                    backend
                        .files
                        .generic_types
                        .constrain_generic_type(*generic_id, scoped_generics_map.get(*generic_id));
                    backend.files.generic_types.mark_as_inferred(*generic_id);
                });

                let mut inferred_return_type = match return_types.len() {
                    0 => DataType::Null,
                    _ => make_union_type(return_types),
                };

                if is_propagating && !matches!(inferred_return_type, DataType::Failable(_)) {
                    inferred_return_type = DataType::Failable(Box::new(inferred_return_type));
                }

                let data_type = match declared_return_ty {
                    Some((ty, ty_span)) => {
                        if !matches_type(ty, &inferred_return_type, &backend.files.generic_types) {
                            backend.files.report_error(
                                &(file_id, file_version),
                                &format!(
                                    "Function returns type {inferred_return_type:?}, but expected {ty:?}",
                                ),
                                *ty_span,
                            );
                        }

                        if is_propagating && !matches!(ty, DataType::Failable(_)) {
                            backend.files.report_error(
                                &(file_id, file_version),
                                "Function is propagating an error, but return type is not failable",
                                *ty_span,
                            );
                        }

                        ty.clone()
                    }
                    None => inferred_return_type,
                };

                let mut symbol_table = backend
                    .files
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
                                    FunctionArgument::Generic((is_ref, _), (name, _)) => Some((
                                        analysis::FunctionArgument {
                                            name: name.clone(),
                                            data_type: DataType::Generic(
                                                new_generic_types.remove(0),
                                            ),
                                            is_optional: false,
                                            is_ref: *is_ref,
                                        },
                                        *span,
                                    )),
                                    FunctionArgument::Typed((is_ref, _), (name, _), (ty, _)) => {
                                        Some((
                                            analysis::FunctionArgument {
                                                name: name.clone(),
                                                data_type: ty.clone(),
                                                is_optional: false,
                                                is_ref: *is_ref,
                                            },
                                            *span,
                                        ))
                                    }
                                    FunctionArgument::Optional((is_ref, _), (name, _), ty, _) => {
                                        Some((
                                            analysis::FunctionArgument {
                                                name: name.clone(),
                                                data_type: match ty {
                                                    Some((ty, _)) => ty.clone(),
                                                    None => DataType::Generic(
                                                        new_generic_types.remove(0),
                                                    ),
                                                },
                                                is_optional: true,
                                                is_ref: *is_ref,
                                            },
                                            *span,
                                        ))
                                    }
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
                        data_type,
                        is_definition: true,
                        undefined: false,
                        span: *name_span,
                        contexts: vec![],
                    },
                    (file_id, file_version),
                    span.end..=usize::MAX,
                    *is_pub,
                );
            }
            GlobalStatement::Import(
                (is_public_import, _),
                _,
                (import_content, _),
                _,
                (path, path_span),
            ) => {
                let uri = &backend.files.lookup(&file_id);

                let result = backend
                    .open_document(&map_import_path(uri, path, backend).await)
                    .await;

                {
                    let mut symbol_table = backend
                        .files
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
                    backend.files.report_error(
                        &(file_id, file_version),
                        "File doesn't exist",
                        *path_span,
                    );

                    continue;
                }

                let imported_file = result.clone().unwrap();

                if backend.files.is_depending_on(&imported_file, file_id) {
                    backend.files.report_error(
                        &(file_id, file_version),
                        "Circular dependency",
                        *path_span,
                    );

                    continue;
                }

                backend
                    .files
                    .add_file_dependency(&(file_id, file_version), imported_file.0);

                let imported_file_symbol_table =
                    match backend.files.symbol_table.get(&imported_file) {
                        Some(symbol_table_ref) => symbol_table_ref.clone(),
                        None => continue,
                    };

                match import_content {
                    ImportContent::ImportSpecific(ident_list) => {
                        let mut import_context = ImportContext {
                            public_definitions: imported_file_symbol_table
                                .public_definitions
                                .clone(),
                            imported_symbols: vec![],
                        };

                        ident_list.iter().for_each(|(ident, span)| {
                            if import_context.imported_symbols.contains(&ident.to_string()) {
                                backend.files.report_error(
                                    &(file_id, file_version),
                                    &format!("Duplicate import '{ident}'"),
                                    *span,
                                );

                                let mut symbol_table = backend
                                    .files
                                    .symbol_table
                                    .entry((file_id, file_version))
                                    .or_default();

                                symbol_table.symbols.insert(
                                    span.start..=span.end,
                                    SymbolInfo {
                                        name: ident.to_string(),
                                        symbol_type: SymbolType::Variable(VariableSymbol {
                                            is_const: false,
                                        }),
                                        data_type: DataType::Null,
                                        is_definition: false,
                                        undefined: true,
                                        span: Span::new(span.start, span.end),
                                        contexts: vec![Context::Import(import_context.clone())],
                                    },
                                );
                                return;
                            }

                            let symbol_definition =
                                imported_file_symbol_table.public_definitions.get(ident);

                            match symbol_definition {
                                Some(definition_location) => {
                                    let definition_file_symbol_table = match backend
                                        .files
                                        .symbol_table
                                        .get(&definition_location.file)
                                    {
                                        Some(symbol_table) => symbol_table.clone(),
                                        None => return,
                                    };

                                    let symbol_info = match definition_file_symbol_table
                                        .symbols
                                        .get(&definition_location.start)
                                    {
                                        Some(symbol_info) => symbol_info,
                                        None => return,
                                    };

                                    let mut symbol_table = backend
                                        .files
                                        .symbol_table
                                        .entry((file_id, file_version))
                                        .or_default();

                                    import_symbol(
                                        &mut symbol_table,
                                        &SymbolInfo {
                                            is_definition: false,
                                            contexts: vec![Context::Import(import_context.clone())],
                                            ..symbol_info.clone()
                                        },
                                        Some(span.start..=span.end),
                                        definition_location,
                                        *is_public_import,
                                    );

                                    import_context.imported_symbols.push(ident.to_string());
                                }
                                None => {
                                    backend.files.report_error(
                                        &(file_id, file_version),
                                        &format!("Could not resolve '{ident}'"),
                                        *span,
                                    );

                                    let mut symbol_table = backend
                                        .files
                                        .symbol_table
                                        .entry((file_id, file_version))
                                        .or_default();

                                    symbol_table.symbols.insert(
                                        span.start..=span.end,
                                        SymbolInfo {
                                            name: ident.to_string(),
                                            symbol_type: SymbolType::Variable(VariableSymbol {
                                                is_const: false,
                                            }),
                                            data_type: DataType::Null,
                                            is_definition: false,
                                            undefined: true,
                                            span: Span::new(span.start, span.end),
                                            contexts: vec![Context::Import(import_context.clone())],
                                        },
                                    );
                                }
                            };
                        });
                    }
                    ImportContent::ImportAll => imported_file_symbol_table
                        .public_definitions
                        .iter()
                        .for_each(|(_, location)| {
                            let definition_file_symbol_table =
                                match backend.files.symbol_table.get(&location.file) {
                                    Some(symbol_table) => symbol_table.clone(),
                                    None => return,
                                };

                            let symbol_info =
                                match definition_file_symbol_table.symbols.get(&location.start) {
                                    Some(symbol_info) => symbol_info,
                                    None => return,
                                };

                            let mut symbol_table = backend
                                .files
                                .symbol_table
                                .entry((file_id, file_version))
                                .or_default();

                            import_symbol(
                                &mut symbol_table,
                                &SymbolInfo {
                                    is_definition: false,
                                    contexts: vec![Context::Import(ImportContext {
                                        public_definitions: imported_file_symbol_table
                                            .public_definitions
                                            .clone(),
                                        imported_symbols: vec![],
                                    })],
                                    ..symbol_info.clone()
                                },
                                None,
                                location,
                                *is_public_import,
                            );
                        }),
                }
            }
            GlobalStatement::Main(_, args, body) => {
                if let Some((args, args_span)) = args {
                    let mut symbol_table = backend
                        .files
                        .symbol_table
                        .entry((file_id, file_version))
                        .or_default();

                    insert_symbol_definition(
                        &mut symbol_table,
                        &SymbolInfo {
                            name: args.to_string(),
                            symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
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

                body.iter().for_each(|stmnt| {
                    analyze_stmnt(
                        file_id,
                        file_version,
                        stmnt,
                        &backend.files,
                        span.end,
                        &backend.files.generic_types.clone(),
                        &mut vec![Context::Main],
                    );
                });
            }
            GlobalStatement::Statement(stmnt) => {
                analyze_stmnt(
                    file_id,
                    file_version,
                    stmnt,
                    &backend.files,
                    usize::MAX,
                    &backend.files.generic_types.clone(),
                    &mut contexts,
                );
            }
        }
    }
}
