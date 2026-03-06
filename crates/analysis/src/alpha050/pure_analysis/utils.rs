//! Utility functions shared across the pure analysis pipeline.

use amber_types::paths::FileId;
use amber_types::{
    DataType,
    PureGenericsMap,
};

use crate::files::FileVersion;
use crate::types::make_union_type;
use crate::Context;

use super::{
    ExpAnalysisResult,
    StmntAnalysisResult,
};

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

/// Recursively propagate generic constraints from one generics map to another.
pub(crate) fn propagate_nested_generics_pure(
    ty: &DataType,
    from: &PureGenericsMap,
    to: &mut PureGenericsMap,
) {
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

/// Get the comparison constraint type for a given type.
///
/// Used in comparison operators to determine type compatibility.
pub(crate) fn get_constrain_ty_for_compare_pure(
    constrain: DataType,
    generics: &PureGenericsMap,
) -> DataType {
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

/// Handle doc string comments by accumulating them in the context stack.
pub(crate) fn handle_doc_strings(
    docs: &String,
    contexts: &mut Vec<Context>,
) -> StmntAnalysisResult {
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

/// Aggregate results from multiple statement/expression analyses.
pub(crate) fn get_stmnt_analysis_result(
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

/// Find the full SymbolInfo for a definition location, searching both the
/// imported table and any transitively-available file tables.
pub(crate) fn find_symbol_info(
    ctx: &super::context::PureContext,
    imported_table: &crate::SymbolTable,
    location: &crate::SymbolLocation,
) -> Option<crate::SymbolInfo> {
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
