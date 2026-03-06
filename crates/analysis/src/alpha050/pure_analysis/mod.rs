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

pub mod context;
pub mod exp;
pub mod global;
pub mod stmnts;
pub mod utils;

use amber_types::paths::FileId;

use crate::files::FileVersion;
use crate::SymbolTable;

// Re-export the result types from existing alpha050 modules.
pub use super::exp::ExpAnalysisResult;
pub use super::stmnts::StmntAnalysisResult;

// Re-export public API.
pub use context::PureContext;
pub use global::analyze_global_pure;
pub use utils::file_key_for_path;

/// Pre-resolved import: the imported file's symbol table and its file key.
#[derive(Clone, Debug, PartialEq)]
pub struct ResolvedImport {
    pub symbol_table: SymbolTable,
    pub file_key: (FileId, FileVersion),
}

// SAFETY: `ResolvedImport` is a plain data struct whose fields all implement
// `Clone` and `PartialEq`. We use simple equality-based update semantics:
// if the old and new values are equal, report no change; otherwise replace.
unsafe impl salsa::Update for ResolvedImport {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old_ref = unsafe { &mut *old_pointer };
        if *old_ref == new_value {
            false
        } else {
            *old_ref = new_value;
            true
        }
    }
}
