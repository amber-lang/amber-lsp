use rangemap::RangeInclusiveMap;
use std::{collections::HashMap, ops::RangeInclusive};
use tower_lsp_server::{lsp_types::Uri, UriExt};
use types::{DataType, GenericsMap};

use crate::{
    backend::{AmberVersion, Backend},
    files::{FileVersion, Files},
    grammar::{CommandModifier, CompilerFlag, Span, Spanned},
    paths::FileId,
    stdlib::resolve,
};

pub mod alpha034;
pub mod alpha035;
pub mod alpha040;
pub mod types;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionSymbol {
    pub arguments: Vec<Spanned<FunctionArgument>>,
    pub is_public: bool,
    pub compiler_flags: Vec<CompilerFlag>,
    pub docs: Option<String>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionArgument {
    pub name: String,
    pub data_type: DataType,
    pub is_optional: bool,
    pub is_ref: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SymbolType {
    Function(FunctionSymbol),
    Variable(VariableSymbol),
    ImportPath,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VariableSymbol {
    pub is_const: bool,
}

/// Information about a symbol in the document.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SymbolInfo {
    pub name: String,
    pub symbol_type: SymbolType,
    pub data_type: DataType,
    pub is_definition: bool,
    pub undefined: bool,
    pub span: Span,
    pub contexts: Vec<Context>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Context {
    Import(ImportContext),
    Function(FunctionContext),
    Block(BlockContext),
    Main,
    Loop,
    DocString(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ImportContext {
    pub public_definitions: HashMap<String, SymbolLocation>,
    pub imported_symbols: Vec<String>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionContext {
    pub compiler_flags: Vec<CompilerFlag>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BlockContext {
    pub modifiers: Vec<CommandModifier>,
}

impl SymbolInfo {
    pub fn to_string(&self, generics_map: &GenericsMap) -> String {
        match &self.symbol_type {
            SymbolType::Function(FunctionSymbol {
                is_public,
                arguments,
                compiler_flags,
                ..
            }) => {
                let compiler_flags_str = compiler_flags
                    .iter()
                    .map(|flag| flag.to_string())
                    .collect::<Vec<String>>()
                    .join("\n");

                format!(
                    "{}{}fun {}({}): {}",
                    if compiler_flags_str.is_empty() {
                        "".to_string()
                    } else {
                        format!("{compiler_flags_str}\n")
                    },
                    if *is_public { "pub " } else { "" },
                    self.name,
                    arguments
                        .iter()
                        .map(
                            |(
                                FunctionArgument {
                                    name,
                                    data_type,
                                    is_optional,
                                    is_ref,
                                },
                                _,
                            )| format!(
                                "{}{}{}: {}",
                                if *is_ref { "ref " } else { "" },
                                name,
                                if *is_optional { "?" } else { "" },
                                data_type.to_string(generics_map)
                            )
                        )
                        .collect::<Vec<String>>()
                        .join(", "),
                    self.data_type.to_string(generics_map)
                )
            }
            SymbolType::ImportPath => format!("import \"{}\"", self.name),
            _ => format!("{}: {}", self.name, self.data_type.to_string(generics_map)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SymbolLocation {
    pub file: (FileId, FileVersion),
    pub start: usize,
    pub end: usize,
}

/// A symbol table that contains all the symbols in a document.
/// Symbols are stored in a RangeMap data structure for fast
/// range queries.
///
/// `definitions` map contains definition of each symbol. RangeMap is used to store the scope of each symbol definition.
///
/// `references` map contains references to each symbol.
///
/// `symbols` range map contains information about symbols in the document.
#[derive(Clone, Debug)]
pub struct SymbolTable {
    pub symbols: RangeInclusiveMap<usize, SymbolInfo>,
    pub definitions: HashMap<String, RangeInclusiveMap<usize, SymbolLocation>>,
    pub references: HashMap<String, Vec<SymbolLocation>>,
    pub public_definitions: HashMap<String, SymbolLocation>,
    pub fun_call_arg_scope: RangeInclusiveMap<usize, SymbolInfo>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self {
            symbols: RangeInclusiveMap::new(),
            definitions: HashMap::new(),
            references: HashMap::new(),
            public_definitions: HashMap::new(),
            fun_call_arg_scope: RangeInclusiveMap::new(),
        }
    }
}

#[tracing::instrument(skip_all)]
pub fn insert_symbol_definition(
    symbol_table: &mut SymbolTable,
    symbol_info: &SymbolInfo,
    file: (FileId, FileVersion),
    definition_scope: RangeInclusive<usize>,
    is_public: bool,
) {
    if definition_scope.is_empty() {
        return;
    }

    if symbol_info.span.into_iter().is_empty() {
        return;
    }

    symbol_table.symbols.insert(
        symbol_info.span.start..=symbol_info.span.end,
        symbol_info.clone(),
    );

    let symbol_definitions = match symbol_table.definitions.get_mut(&symbol_info.name) {
        Some(symbol_definitions) => symbol_definitions,
        None => {
            symbol_table
                .definitions
                .insert(symbol_info.name.to_string(), RangeInclusiveMap::new());

            match symbol_table.definitions.get_mut(&symbol_info.name) {
                Some(definitions) => definitions,
                None => {
                    tracing::error!("Failed to insert symbol definition");
                    return;
                }
            }
        }
    };

    let definition_location = SymbolLocation {
        file,
        start: symbol_info.span.start,
        end: symbol_info.span.end,
    };

    symbol_definitions.insert(definition_scope, definition_location.clone());

    if is_public {
        symbol_table
            .public_definitions
            .insert(symbol_info.name.to_string(), definition_location.clone());
    }
}

#[tracing::instrument(skip_all)]
pub fn import_symbol(
    symbol_table: &mut SymbolTable,
    symbol_info: &SymbolInfo,
    symbol_span: Option<RangeInclusive<usize>>,
    definition_location: &SymbolLocation,
    is_public: bool,
) {
    let definition_location_span = definition_location.start..=definition_location.end;

    if definition_location_span.is_empty() {
        return;
    }

    let symbol = &symbol_info.name;

    if let Some(symbol_span) = symbol_span {
        if symbol_span.is_empty() {
            return;
        }

        symbol_table
            .symbols
            .insert(symbol_span.clone(), symbol_info.clone());
    }

    let symbol_definitions = match symbol_table.definitions.get_mut(symbol) {
        Some(symbol_definitions) => symbol_definitions,
        None => {
            symbol_table
                .definitions
                .insert(symbol.to_string(), RangeInclusiveMap::new());

            match symbol_table.definitions.get_mut(symbol) {
                Some(definitions) => definitions,
                None => {
                    tracing::error!("Failed to insert symbol definition");
                    return;
                }
            }
        }
    };

    symbol_definitions.insert(0..=usize::MAX, definition_location.clone());

    if is_public {
        symbol_table
            .public_definitions
            .insert(symbol.to_string(), definition_location.clone());
    }
}

#[tracing::instrument(skip_all)]
pub fn insert_symbol_reference(
    symbol: &str,
    files: &Files,
    reference_location: &SymbolLocation,
    scoped_generics: &GenericsMap,
    contexts: &[Context],
) {
    let span = reference_location.start..=reference_location.end;

    if span.is_empty() {
        return;
    }

    let symbol_info = get_symbol_definition_info(
        files,
        symbol,
        &reference_location.file,
        reference_location.start,
    );

    match symbol_info {
        Some(symbol_info) => {
            let mut current_file_symbol_table =
                match files.symbol_table.get_mut(&reference_location.file) {
                    Some(symbol_table) => symbol_table,
                    None => {
                        tracing::error!(
                            "Symbol table for file {:?} not found",
                            reference_location.file
                        );
                        return;
                    }
                };

            // If generic is already inferred, use the inferred type
            // if not, use generic as a pointer to the inferred type in the map
            let data_type = match symbol_info.data_type {
                DataType::Generic(id) if scoped_generics.is_inferred(id) => {
                    scoped_generics.get_recursive(id)
                }
                DataType::Union(types) => DataType::Union(
                    types
                        .iter()
                        .map(|ty| scoped_generics.deref_type(ty))
                        .collect(),
                ),
                ty => ty,
            };

            let symbol_type = match symbol_info.symbol_type {
                SymbolType::Function(FunctionSymbol {
                    arguments,
                    is_public,
                    compiler_flags,
                    docs,
                }) => SymbolType::Function(FunctionSymbol {
                    arguments: arguments
                        .iter()
                        .map(|(arg, span)| {
                            (
                                FunctionArgument {
                                    name: arg.name.clone(),
                                    data_type: scoped_generics.deref_type(&arg.data_type),
                                    is_optional: arg.is_optional,
                                    is_ref: arg.is_ref,
                                },
                                *span,
                            )
                        })
                        .collect(),
                    is_public,
                    compiler_flags,
                    docs: docs.clone(),
                }),
                symbol => symbol,
            };

            current_file_symbol_table.symbols.insert(
                span.clone(),
                SymbolInfo {
                    name: symbol.to_string(),
                    symbol_type,
                    data_type,
                    is_definition: false,
                    undefined: false,
                    span: Span::new(*span.start(), *span.end()),
                    contexts: contexts.to_vec(),
                },
            );
        }
        None => {
            files.report_error(
                &reference_location.file,
                &format!("\"{symbol}\" is not defined"),
                (reference_location.start..reference_location.end).into(),
            );

            let mut current_file_symbol_table =
                match files.symbol_table.get_mut(&reference_location.file) {
                    Some(symbol_table) => symbol_table,
                    None => {
                        tracing::error!(
                            "Symbol table for file {:?} not found",
                            reference_location.file
                        );
                        return;
                    }
                };

            current_file_symbol_table.symbols.insert(
                span.clone(),
                SymbolInfo {
                    name: symbol.to_string(),
                    symbol_type: SymbolType::Variable(VariableSymbol { is_const: false }),
                    data_type: DataType::Null,
                    is_definition: false,
                    undefined: true,
                    span: Span::new(*span.start(), *span.end()),
                    contexts: contexts.to_vec(),
                },
            );
        }
    }

    let mut current_file_symbol_table = match files.symbol_table.get_mut(&reference_location.file) {
        Some(symbol_table) => symbol_table,
        None => {
            tracing::error!(
                "Symbol table for file {:?} not found",
                reference_location.file
            );
            return;
        }
    };

    let symbol_references = match current_file_symbol_table.references.get_mut(symbol) {
        Some(symbol_references) => symbol_references,
        None => {
            current_file_symbol_table
                .references
                .insert(symbol.to_string(), vec![]);

            match current_file_symbol_table.references.get_mut(symbol) {
                Some(references) => references,
                None => {
                    tracing::error!("Failed to insert symbol reference");
                    return;
                }
            }
        }
    };

    symbol_references.push(reference_location.clone());
}

#[tracing::instrument(skip_all)]
pub fn get_symbol_definition_info(
    files: &Files,
    symbol: &str,
    file: &(FileId, FileVersion),
    position: usize,
) -> Option<SymbolInfo> {
    let current_file_symbol_table = match files.symbol_table.get(file) {
        Some(symbol_table) => symbol_table.clone(),
        None => return None,
    };

    let symbol_definition = match current_file_symbol_table.definitions.get(symbol) {
        Some(symbol_definitions) => symbol_definitions.get(&position).cloned(),
        None => return None,
    };

    match symbol_definition {
        Some(definition) => {
            if definition.file == *file {
                current_file_symbol_table
                    .symbols
                    .get(&definition.start)
                    .cloned()
            } else {
                let definition_file_symbol_table = match files
                    .symbol_table
                    .get_mut(&definition.file)
                {
                    Some(symbol_table) => symbol_table,
                    None => {
                        tracing::error!("Symbol table for file {:?} not found", definition.file);
                        return None;
                    }
                };

                definition_file_symbol_table
                    .symbols
                    .get(&definition.start)
                    .cloned()
            }
        }
        None => None,
    }
}

#[tracing::instrument(skip_all)]
pub async fn map_import_path(uri: &Uri, path: &str, backend: &Backend) -> Uri {
    if path.starts_with("std/") || path == "std" || path == "builtin" {
        match backend.amber_version {
            AmberVersion::Alpha034 if path == "std" => {
                if let Some(uri) = resolve(backend, "std/main".to_string()).await {
                    return uri;
                }
            }
            _ => {
                if let Some(uri) = resolve(backend, path.to_string()).await {
                    return uri;
                }
            }
        }
    }

    let path = uri.to_file_path().unwrap().parent().unwrap().join(path);

    Uri::from_file_path(path).unwrap()
}
