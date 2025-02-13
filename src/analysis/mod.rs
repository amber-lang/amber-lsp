use rangemap::RangeInclusiveMap;
use std::{collections::HashMap, ops::RangeInclusive};
use types::GenericsMap;

use crate::{backend::Backend, grammar::alpha034::DataType, paths::FileId};

pub mod alpha034;
pub mod types;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionSymbol {
    pub arguments: Vec<(String, DataType)>,
    pub is_public: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarSymbol {}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SymbolType {
    Function(FunctionSymbol),
    Variable(VarSymbol),
}

/// Information about a symbol in the document.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SymbolInfo {
    pub name: String,
    pub symbol_type: SymbolType,
    pub data_type: DataType,
    pub is_definition: bool,
    pub undefined: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SymbolLocation {
    pub file: FileId,
    pub start: usize,
    pub end: usize,
    pub is_public: bool,
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
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self {
            symbols: RangeInclusiveMap::new(),
            definitions: HashMap::new(),
            references: HashMap::new(),
        }
    }
}

#[tracing::instrument]
pub fn insert_symbol_definition(
    symbol_table: &mut SymbolTable,
    symbol: &str,
    definition_scope: RangeInclusive<usize>,
    definition_location: &SymbolLocation,
    data_type: DataType,
    symbol_type: SymbolType,
) {
    symbol_table.symbols.insert(
        definition_location.start..=definition_location.end,
        SymbolInfo {
            name: symbol.to_string(),
            symbol_type,
            data_type,
            is_definition: true,
            undefined: false,
        },
    );

    let symbol_definitions = match symbol_table.definitions.get_mut(symbol) {
        Some(symbol_definitions) => symbol_definitions,
        None => {
            symbol_table
                .definitions
                .insert(symbol.to_string(), RangeInclusiveMap::new());

            symbol_table.definitions.get_mut(symbol).unwrap()
        }
    };

    symbol_definitions.insert(definition_scope, definition_location.clone());
}

#[tracing::instrument]
pub fn insert_symbol_reference(
    symbol: &str,
    backend: &Backend,
    reference_location: &SymbolLocation,
    scoped_generics: &GenericsMap,
) {
    let span = reference_location.start..=reference_location.end;

    let symbol_info = get_symbol_definition_info(
        backend,
        symbol,
        &reference_location.file,
        reference_location.start,
    );

    match symbol_info {
        Some(symbol_info) => {
            let mut current_file_symbol_table = backend
                .symbol_table
                .get_mut(&reference_location.file)
                .unwrap();

            // If generic is already inferred, use the inferred type
            // if not, use generic as a pointer to the inferred type in the map
            let data_type = match symbol_info.data_type {
                DataType::Generic(file_id, id) if scoped_generics.is_inferred((file_id, id)) => {
                    scoped_generics.get_recursive(file_id, id)
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
                }) => SymbolType::Function(FunctionSymbol {
                    arguments: arguments
                        .iter()
                        .map(|(name, ty)| (name.clone(), scoped_generics.deref_type(ty)))
                        .collect(),
                    is_public,
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
                },
            );
        }
        None => {
            backend.report_error(
                &reference_location.file,
                &format!("\"{}\" is not defined", symbol),
                (reference_location.start..reference_location.end).into(),
            );

            let mut current_file_symbol_table = backend
                .symbol_table
                .get_mut(&reference_location.file)
                .unwrap();

            current_file_symbol_table.symbols.insert(
                span.clone(),
                SymbolInfo {
                    name: symbol.to_string(),
                    symbol_type: SymbolType::Variable(VarSymbol {}),
                    data_type: DataType::Null,
                    is_definition: false,
                    undefined: true,
                },
            );
        }
    }

    let mut current_file_symbol_table = backend
        .symbol_table
        .get_mut(&reference_location.file)
        .unwrap();

    let symbol_references = match current_file_symbol_table.references.get_mut(symbol) {
        Some(symbol_references) => symbol_references,
        None => {
            current_file_symbol_table
                .references
                .insert(symbol.to_string(), vec![]);

            current_file_symbol_table
                .references
                .get_mut(symbol)
                .unwrap()
        }
    };

    symbol_references.push(reference_location.clone());
}

pub fn get_symbol_definition_info(
    backend: &Backend,
    symbol: &str,
    file_id: &FileId,
    position: usize,
) -> Option<SymbolInfo> {
    let current_file_symbol_table = match backend.symbol_table.get(&file_id) {
        Some(symbol_table) => symbol_table,
        None => return None,
    };

    let symbol_definition = match current_file_symbol_table.definitions.get(symbol) {
        Some(symbol_definitions) => symbol_definitions.get(&position).cloned(),
        None => return None,
    };

    match symbol_definition {
        Some(definition) => {
            if definition.file == *file_id {
                current_file_symbol_table
                    .symbols
                    .get(&definition.start)
                    .cloned()
            } else {
                let definition_file_symbol_table =
                    backend.symbol_table.get_mut(&definition.file).unwrap();

                definition_file_symbol_table
                    .symbols
                    .get(&definition.start)
                    .cloned()
            }
        }
        None => None,
    }
}
