use rangemap::RangeInclusiveMap;

use crate::{
    backend::Backend,
    grammar::{
        alpha034::{DataType, FunctionArgument, GlobalStatement, ImportContent},
        Spanned,
    },
    paths::FileId,
    symbol_table::{
        insert_symbol_definition,
        types::{make_union_type, matches_type},
        FunctionSymbol, SymbolInfo, SymbolLocation, SymbolType, VarSymbol,
    },
};

use super::{map_import_path, stmnts::analyze_stmnt};

#[tracing::instrument]
pub fn analyze_global_stmnt(
    file_id: &FileId,
    ast: &Vec<Spanned<GlobalStatement>>,
    backend: &Backend,
) {
    backend.generic_types.clean(file_id);

    for (global, span) in ast.iter() {
        match global {
            GlobalStatement::FunctionDefinition(
                (is_pub, _),
                _,
                (name, name_span),
                args,
                ty,
                body,
            ) => {
                // We create scoped generics map, to not overwrite other generics, not defined here
                let scoped_generics_map = backend.generic_types.clone();

                let last_arg_span = args
                    .last()
                    .map(|(_, span)| span.end)
                    .unwrap_or(name_span.end);

                let mut new_generic_types = vec![];
                args.iter().for_each(|(arg, _)| {
                    let (name, ty, name_span) = match arg {
                        FunctionArgument::Generic((name, span)) => {
                            let generic_id = DataType::new_generic_id();
                            scoped_generics_map.constrain_generic_type(
                                (file_id.clone(), generic_id),
                                DataType::Any,
                            );

                            new_generic_types.push(generic_id);
                            (name, DataType::Generic(file_id.clone(), generic_id), span)
                        }
                        FunctionArgument::Typed((name, span), (ty, _)) => (name, ty.clone(), span),
                        _ => return,
                    };

                    let mut symbol_table = backend.symbol_table.get_mut(file_id).unwrap();

                    insert_symbol_definition(
                        &mut symbol_table,
                        name,
                        last_arg_span..=span.end,
                        &SymbolLocation {
                            file: *file_id,
                            start: name_span.start,
                            end: name_span.end,
                            is_public: false,
                        },
                        ty,
                        SymbolType::Variable(VarSymbol {}),
                    );
                });

                let mut return_types = vec![];

                body.iter().for_each(|stmnt| {
                    if let Some(ty) =
                        analyze_stmnt(&file_id, stmnt, backend, span.end, &scoped_generics_map)
                    {
                        return_types.push(ty);
                    }
                });

                new_generic_types.iter().for_each(|generic_id| {
                    backend.generic_types.constrain_generic_type(
                        (*file_id, *generic_id),
                        scoped_generics_map.get(*file_id, *generic_id),
                    );
                    backend
                        .generic_types
                        .mark_as_inferred((*file_id, *generic_id));
                });

                let return_type = match return_types.len() {
                    0 => DataType::Null,
                    _ => make_union_type(return_types),
                };

                let data_type = match ty {
                    Some((ty, ty_span)) => {
                        if !matches_type(ty, &return_type, &backend.generic_types) {
                            backend.report_error(
                                file_id,
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

                let mut symbol_table = backend.symbol_table.get_mut(file_id).unwrap();

                insert_symbol_definition(
                    &mut symbol_table,
                    name,
                    span.end..=usize::MAX,
                    &SymbolLocation {
                        file: *file_id,
                        start: name_span.start,
                        end: name_span.end,
                        is_public: *is_pub,
                    },
                    data_type,
                    SymbolType::Function(FunctionSymbol {
                        arguments: args
                            .iter()
                            .filter_map(|(arg, _)| match arg {
                                FunctionArgument::Generic((name, _)) => Some((
                                    name.clone(),
                                    DataType::Generic(*file_id, new_generic_types.remove(0)),
                                )),
                                FunctionArgument::Typed((name, _), (ty, _)) => {
                                    Some((name.clone(), ty.clone()))
                                }
                                _ => None,
                            })
                            .collect(),
                        is_public: *is_pub,
                    }),
                );
            }
            GlobalStatement::Import((is_pub, _), _, (import_content, _), _, (path, path_span)) => {
                let uri = &backend.paths.lookup(file_id);

                let result = backend.open_document(&map_import_path(uri, path, backend));

                if result.is_err() {
                    backend.report_error(file_id, "File doesn't exist", *path_span);

                    continue;
                }

                let import_file_id = result.unwrap();

                let imported_file_symbol_table = match backend.symbol_table.get(&import_file_id) {
                    Some(symbol_table_ref) => {
                        let symbol_table = symbol_table_ref.clone();
                        symbol_table
                    }
                    None => continue,
                };

                match import_content {
                    ImportContent::ImportSpecific(ident_list) => {
                        ident_list.iter().for_each(|(ident, span)| {
                            let symbol_definition =
                                match imported_file_symbol_table.definitions.get(ident) {
                                    Some(symbol_definitions) => symbol_definitions
                                        .clone()
                                        .into_iter()
                                        .find(|(_, location)| location.is_public),
                                    None => None,
                                };

                            match symbol_definition {
                                Some((_, symbol_definition)) => {
                                    let symbol_info = imported_file_symbol_table
                                        .symbols
                                        .get(&symbol_definition.start)
                                        .unwrap();

                                    let mut symbol_table =
                                        backend.symbol_table.get_mut(file_id).unwrap();

                                    symbol_table.symbols.insert(
                                        span.start..=span.end,
                                        SymbolInfo {
                                            is_definition: false,
                                            ..symbol_info.clone()
                                        },
                                    );

                                    let mut range_map = RangeInclusiveMap::new();

                                    range_map.insert(
                                        span.start..=usize::MAX,
                                        SymbolLocation {
                                            file: symbol_definition.file.clone(),
                                            start: symbol_definition.start,
                                            end: symbol_definition.end,
                                            is_public: *is_pub,
                                        },
                                    );

                                    symbol_table.definitions.insert(ident.clone(), range_map);
                                }
                                None => {
                                    let mut symbol_table =
                                        backend.symbol_table.get_mut(file_id).unwrap();
                                    symbol_table.symbols.insert(
                                        span.start..=span.end,
                                        SymbolInfo {
                                            name: ident.clone(),
                                            symbol_type: SymbolType::Function(FunctionSymbol {
                                                arguments: vec![],
                                                is_public: *is_pub,
                                            }),
                                            data_type: DataType::Null,
                                            is_definition: false,
                                            undefined: true,
                                        },
                                    );
                                }
                            };
                        });
                    }
                    ImportContent::ImportAll => imported_file_symbol_table
                        .definitions
                        .iter()
                        .for_each(|(_, definition)| {
                            definition.iter().for_each(|(_, location)| {
                                if !location.is_public {
                                    return;
                                }

                                let symbol_info =
                                    match imported_file_symbol_table.symbols.get(&location.start) {
                                        Some(symbol_info) => symbol_info,
                                        None => return,
                                    };

                                let mut symbol_table =
                                    backend.symbol_table.get_mut(file_id).unwrap();

                                insert_symbol_definition(
                                    &mut symbol_table,
                                    &symbol_info.name,
                                    location.start..=usize::MAX,
                                    location,
                                    symbol_info.data_type.clone(),
                                    symbol_info.symbol_type.clone(),
                                );
                            });
                        }),
                }
            }
            GlobalStatement::Main(_, body) => {
                body.iter().for_each(|stmnt| {
                    analyze_stmnt(
                        &file_id,
                        stmnt,
                        backend,
                        stmnt.1.end,
                        &backend.generic_types.clone(),
                    );
                });
            }
            GlobalStatement::Statement(stmnt) => {
                analyze_stmnt(
                    &file_id,
                    stmnt,
                    backend,
                    usize::MAX,
                    &backend.generic_types.clone(),
                );
            }
        }
    }
}
