use crate::{
    analysis::{
        self, import_symbol, insert_symbol_definition, map_import_path,
        types::{make_union_type, matches_type, DataType},
        Context, FunctionContext, FunctionSymbol, ImportContext, SymbolInfo, SymbolLocation,
        SymbolType,
    },
    backend::Backend,
    files::FileVersion,
    grammar::{
        alpha034::{FunctionArgument, GlobalStatement, ImportContent},
        Span, Spanned,
    },
    paths::FileId,
};

use super::stmnts::analyze_stmnt;

#[tracing::instrument(skip_all)]
pub async fn analyze_global_stmnt(
    file_id: FileId,
    file_version: FileVersion,
    ast: &Vec<Spanned<GlobalStatement>>,
    backend: &Backend,
) {
    for (global, span) in ast.iter() {
        match global {
            GlobalStatement::FunctionDefinition(
                compiler_flags,
                (is_pub, _),
                _,
                (name, name_span),
                args,
                ty,
                body,
            ) => {
                // We create scoped generics map, to not overwrite other generics, not defined here
                let scoped_generics_map = backend.files.generic_types.clone();

                let mut new_generic_types = vec![];
                args.iter().for_each(|(arg, _)| {
                    let (name, ty, name_span) = match arg {
                        FunctionArgument::Generic((name, span)) => {
                            let generic_id = scoped_generics_map.new_generic_id();

                            scoped_generics_map.constrain_generic_type(generic_id, DataType::Any);
                            new_generic_types.push(generic_id);

                            (name, DataType::Generic(generic_id), span)
                        }
                        FunctionArgument::Typed((name, span), (ty, _)) => (name, ty.clone(), span),
                        _ => return,
                    };

                    let mut symbol_table = backend
                        .files
                        .symbol_table
                        .get_mut(&(file_id, file_version))
                        .unwrap();

                    insert_symbol_definition(
                        &mut symbol_table,
                        name,
                        name_span.end..=span.end,
                        &SymbolLocation {
                            file: (file_id, file_version),
                            start: name_span.start,
                            end: name_span.end,
                        },
                        ty,
                        SymbolType::Variable,
                        false,
                        &vec![],
                    );
                });

                let mut return_types = vec![];

                body.iter().for_each(|stmnt| {
                    if let Some(ty) = analyze_stmnt(
                        file_id,
                        file_version,
                        stmnt,
                        &backend.files,
                        span.end,
                        &scoped_generics_map,
                        &vec![Context::Function(FunctionContext {
                            compiler_flags: vec![],
                        })],
                    ) {
                        return_types.push(ty);
                    }
                });

                new_generic_types.iter().for_each(|generic_id| {
                    backend
                        .files
                        .generic_types
                        .constrain_generic_type(*generic_id, scoped_generics_map.get(*generic_id));
                    backend.files.generic_types.mark_as_inferred(*generic_id);
                });

                let return_type = match return_types.len() {
                    0 => DataType::Null,
                    _ => make_union_type(return_types),
                };

                let data_type = match ty {
                    Some((ty, ty_span)) => {
                        if !matches_type(ty, &return_type, &backend.files.generic_types) {
                            backend.files.report_error(
                                &(file_id, file_version),
                                &format!(
                                    "Function returns type {:?}, but expected {:?}",
                                    return_type, ty
                                ),
                                *ty_span,
                            );
                        }

                        ty.clone()
                    }
                    None => return_type,
                };

                let mut symbol_table = backend
                    .files
                    .symbol_table
                    .entry((file_id, file_version))
                    .or_insert_with(|| Default::default());

                insert_symbol_definition(
                    &mut symbol_table,
                    name,
                    span.end..=usize::MAX,
                    &SymbolLocation {
                        file: (file_id, file_version),
                        start: name_span.start,
                        end: name_span.end,
                    },
                    data_type,
                    SymbolType::Function(FunctionSymbol {
                        arguments: args
                            .iter()
                            .filter_map(|(arg, span)| match arg {
                                FunctionArgument::Generic((name, _)) => Some((
                                    analysis::FunctionArgument {
                                        name: name.clone(),
                                        data_type: DataType::Generic(new_generic_types.remove(0)),
                                        is_optional: false,
                                    },
                                    span.clone(),
                                )),
                                FunctionArgument::Typed((name, _), (ty, _)) => Some((
                                    analysis::FunctionArgument {
                                        name: name.clone(),
                                        data_type: ty.clone(),
                                        is_optional: false,
                                    },
                                    span.clone(),
                                )),
                                _ => None,
                            })
                            .collect::<Vec<_>>(),
                        is_public: *is_pub,
                        compiler_flags: compiler_flags
                            .iter()
                            .map(|(flag, _)| flag.clone())
                            .collect(),
                        docs: None,
                    }),
                    *is_pub,
                    &vec![],
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
                    .open_document(&map_import_path(uri, path, &backend).await)
                    .await;

                {
                    let mut symbol_table = backend
                        .files
                        .symbol_table
                        .entry((file_id, file_version))
                        .or_insert_with(|| Default::default());

                    insert_symbol_definition(
                        &mut symbol_table,
                        &path,
                        path_span.start..=path_span.end,
                        &SymbolLocation {
                            file: result.clone().unwrap_or((file_id, file_version)),
                            start: path_span.start,
                            end: path_span.end,
                        },
                        DataType::Text,
                        SymbolType::ImportPath,
                        false,
                        &vec![],
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
                        Some(symbol_table_ref) => {
                            let symbol_table = symbol_table_ref.clone();
                            symbol_table
                        }
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
                                    &format!("Duplicate import '{}'", ident),
                                    *span,
                                );

                                let mut symbol_table = backend
                                    .files
                                    .symbol_table
                                    .entry((file_id, file_version))
                                    .or_insert_with(|| Default::default());

                                symbol_table.symbols.insert(
                                    span.start..=span.end,
                                    SymbolInfo {
                                        name: ident.to_string(),
                                        symbol_type: SymbolType::Variable,
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
                                        .or_insert_with(|| Default::default());

                                    import_symbol(
                                        &mut symbol_table,
                                        ident,
                                        Some(span.start..=span.end),
                                        definition_location,
                                        symbol_info.data_type.clone(),
                                        symbol_info.symbol_type.clone(),
                                        *is_public_import,
                                        &import_context,
                                    );

                                    import_context.imported_symbols.push(ident.to_string());
                                }
                                None => {
                                    backend.files.report_error(
                                        &(file_id, file_version),
                                        &format!("Could not resolve '{}'", ident),
                                        *span,
                                    );

                                    let mut symbol_table = backend
                                        .files
                                        .symbol_table
                                        .entry((file_id, file_version))
                                        .or_insert_with(|| Default::default());

                                    symbol_table.symbols.insert(
                                        span.start..=span.end,
                                        SymbolInfo {
                                            name: ident.to_string(),
                                            symbol_type: SymbolType::Variable,
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
                                .or_insert_with(|| Default::default());

                            import_symbol(
                                &mut symbol_table,
                                &symbol_info.name,
                                None,
                                location,
                                symbol_info.data_type.clone(),
                                symbol_info.symbol_type.clone(),
                                *is_public_import,
                                &ImportContext {
                                    public_definitions: imported_file_symbol_table
                                        .public_definitions
                                        .clone(),
                                    imported_symbols: vec![],
                                },
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
                        .or_insert_with(|| Default::default());

                    insert_symbol_definition(
                        &mut symbol_table,
                        args,
                        args_span.end..=span.end,
                        &SymbolLocation {
                            file: (file_id, file_version),
                            start: args_span.start,
                            end: args_span.end,
                        },
                        DataType::Array(Box::new(DataType::Text)),
                        SymbolType::Variable,
                        false,
                        &vec![],
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
                        &vec![Context::Main],
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
                    &vec![],
                );
            }
        }
    }
}
