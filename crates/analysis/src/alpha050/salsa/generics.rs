//! Per-function generics derivation query.
//!
//! Memoized independently from file-level analysis — changing code outside
//! a function body does not re-derive its generics.

use amber_types::GenericsSnapshot;

use super::types::FunctionId;
use super::Db;

/// Derive generic constraints for a single function.
///
/// Memoized independently — changing code outside a function body
/// does not re-derive its generics.
///
/// For now, returns an empty snapshot. Will be populated as Phase 3.
#[salsa::tracked]
pub fn derive_function_generics(db: &dyn Db, func: FunctionId<'_>) -> GenericsSnapshot {
    // TODO (Phase 3): Extract function AST, analyze body with PureGenericsMap.
    let _ = (db, func);
    GenericsSnapshot::default()
}
