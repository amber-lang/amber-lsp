pub mod exp;
pub mod global;
pub mod pure_analysis;
pub mod salsa;
pub mod stmnts;

/// Backward-compatible re-export of the salsa module as `salsa_db`.
///
/// External consumers can continue using `crate::alpha050::salsa_db::*`
/// while we migrate them to `crate::alpha050::salsa::*`.
pub mod salsa_db {
    pub use super::salsa::*;
}
