//! Dual validation tests: run the same source code through both the legacy
//! imperative pipeline and the Salsa incremental pipeline, then compare outputs.
//!
//! These tests validate that the Salsa pipeline produces equivalent results
//! to the existing Backend analysis for Alpha050 code.

use std::path::Path;
use std::sync::Arc;

use amber_analysis::AnalysisHost;
use amber_lsp::backend::{
    AmberVersion,
    Backend,
};
use amber_lsp::salsa_shadow::SalsaShadow;
use amber_types::fs::MemoryFS;
use amber_types::paths::FileId;
use tokio::test;
use tower_lsp_server::lsp_types::Uri;
use tower_lsp_server::UriExt;

/// Helper: run the legacy pipeline on the given source code and return
/// (errors, warnings, symbol names).
async fn run_legacy(code: &str) -> (Vec<String>, Vec<String>, Vec<String>) {
    let (service, _) = tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha050,
            Some(Arc::new(MemoryFS::new())),
        )
    });

    let backend = service.inner();
    let vfs = &backend.files.fs;

    let file = {
        #[cfg(windows)]
        {
            Path::new("C:\\main.ab")
        }
        #[cfg(unix)]
        {
            Path::new("/main.ab")
        }
    };
    let uri = Uri::from_file_path(file).unwrap();

    vfs.write(&uri.to_file_path().unwrap(), code).await.unwrap();

    let file_key = backend.open_document(&uri).await.unwrap();

    let errors: Vec<String> = backend
        .files
        .errors
        .get(&file_key)
        .map(|e| e.iter().map(|(msg, _)| msg.clone()).collect())
        .unwrap_or_default();

    let warnings: Vec<String> = backend
        .files
        .warnings
        .get(&file_key)
        .map(|w| w.iter().map(|(msg, _)| msg.clone()).collect())
        .unwrap_or_default();

    let symbols: Vec<String> = backend
        .files
        .symbol_table
        .get(&file_key)
        .map(|table| {
            table
                .symbols
                .iter()
                .map(|(_, info)| info.name.clone())
                .collect()
        })
        .unwrap_or_default();

    (errors, warnings, symbols)
}

/// Helper: run the Salsa pipeline on the given source code and return
/// (errors, warnings, symbol names).
fn run_salsa(code: &str) -> (Vec<String>, Vec<String>, Vec<String>) {
    use amber_analysis::alpha050::salsa_db::DiagnosticSeverity;

    let mut shadow = SalsaShadow::new(AmberVersion::Alpha050);
    let file_id = FileId(0);

    let path = {
        #[cfg(windows)]
        {
            "C:\\main.ab"
        }
        #[cfg(unix)]
        {
            "/main.ab"
        }
    };

    shadow.update_file(file_id, path, code, 1);
    let output = shadow.analyze(&file_id).expect("Analysis should succeed");

    let errors: Vec<String> = output
        .diagnostics
        .iter()
        .filter(|d| d.severity == DiagnosticSeverity::Error)
        .map(|d| d.message.clone())
        .collect();

    let warnings: Vec<String> = output
        .diagnostics
        .iter()
        .filter(|d| d.severity == DiagnosticSeverity::Warning)
        .map(|d| d.message.clone())
        .collect();

    let symbols: Vec<String> = output
        .symbol_table
        .symbols
        .iter()
        .map(|(_, info)| info.name.clone())
        .collect();

    (errors, warnings, symbols)
}

/// Macro for dual validation: runs both pipelines and compares error counts,
/// warning counts, and symbol names.
macro_rules! dual_test {
    ($name:ident, $code:expr) => {
        #[test]
        async fn $name() {
            let code = $code;

            let (legacy_errors, legacy_warnings, legacy_symbols) = run_legacy(code).await;
            let (salsa_errors, salsa_warnings, salsa_symbols) = run_salsa(code);

            // Compare error counts
            assert_eq!(
                legacy_errors.len(),
                salsa_errors.len(),
                "Error count mismatch for {}: legacy {:?} vs salsa {:?}",
                stringify!($name),
                legacy_errors,
                salsa_errors,
            );

            // Compare warning counts
            assert_eq!(
                legacy_warnings.len(),
                salsa_warnings.len(),
                "Warning count mismatch for {}: legacy {:?} vs salsa {:?}",
                stringify!($name),
                legacy_warnings,
                salsa_warnings,
            );

            // Compare symbol names (sorted for order-independence)
            let mut legacy_syms = legacy_symbols.clone();
            legacy_syms.sort();
            let mut salsa_syms = salsa_symbols.clone();
            salsa_syms.sort();

            assert_eq!(
                legacy_syms,
                salsa_syms,
                "Symbol name mismatch for {}: legacy {:?} vs salsa {:?}",
                stringify!($name),
                legacy_syms,
                salsa_syms,
            );
        }
    };
}

// ─── Dual Validation Tests ────────────────────────────────────────────────────

dual_test!(dual_simple_let, "let x = 42");

dual_test!(
    dual_simple_function,
    r#"
fun greet(name) {
    echo "Hello, {name}"
}
"#
);

dual_test!(
    dual_function_with_return,
    r#"
fun add(a, b) {
    return a + b
}
"#
);

dual_test!(
    dual_variable_assignment,
    r#"
let x = 10
x = 20
"#
);

dual_test!(
    dual_if_expression,
    r#"
let x = 5
if x > 3 {
    echo "big"
}
"#
);

dual_test!(dual_parse_error, "fun {");
