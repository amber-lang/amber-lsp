use std::path::Path;
use std::sync::Arc;

use amber_analysis::AnalysisHost;
use amber_lsp::backend::{
    AmberVersion,
    Backend,
};
use amber_types::fs::MemoryFS;
use insta::assert_debug_snapshot;
use tokio::test;
use tower_lsp_server::lsp_types::Uri;
use tower_lsp_server::UriExt;

#[test]
async fn test_simple_function() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        "
fun add(a, b) {
    return a + b
}
",
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let symbol_table = backend.files.symbol_table.get(&file_id).unwrap();
    let generic_types = backend.files.generic_types.clone();

    assert_debug_snapshot!(symbol_table.symbols);
    assert_debug_snapshot!(symbol_table
        .symbols
        .iter()
        .map(|(_, symbol_info)| symbol_info.to_string(&generic_types))
        .collect::<Vec<String>>());
    assert_debug_snapshot!(backend.files.errors);
}

#[test]
async fn test_comparison_with_text_literal() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        "
fun starts_with_a(word) {
    return word < \"b\"
}
",
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let symbol_table = backend.files.symbol_table.get(&file_id).unwrap();
    let generic_types = backend.files.generic_types.clone();

    // The parameter 'word' should be constrained to Text due to comparison with "b" (Text)
    assert_debug_snapshot!(symbol_table.symbols);
    assert_debug_snapshot!(symbol_table
        .symbols
        .iter()
        .map(|(_, symbol_info)| symbol_info.to_string(&generic_types))
        .collect::<Vec<String>>());
    assert_debug_snapshot!(backend.files.errors);
}

#[test]
async fn test_comparison_with_array_literal() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        "
fun compare_arrays(arr) {
    return arr > [1, 2, 3]
}
",
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let symbol_table = backend.files.symbol_table.get(&file_id).unwrap();
    let generic_types = backend.files.generic_types.clone();

    // The parameter 'arr' should be constrained to [Int | Num] due to comparison with [1, 2, 3]
    assert_debug_snapshot!(symbol_table.symbols);
    assert_debug_snapshot!(symbol_table
        .symbols
        .iter()
        .map(|(_, symbol_info)| symbol_info.to_string(&generic_types))
        .collect::<Vec<String>>());
    assert_debug_snapshot!(backend.files.errors);
}

#[test]
async fn test_two_generics_compared() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        "
fun max_value(a, b) {
    if a > b: return a
    return b
}
",
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let symbol_table = backend.files.symbol_table.get(&file_id).unwrap();
    let generic_types = backend.files.generic_types.clone();

    // When both operands are unknown generics, they should get the broad constraint
    assert_debug_snapshot!(symbol_table.symbols);
    assert_debug_snapshot!(symbol_table
        .symbols
        .iter()
        .map(|(_, symbol_info)| symbol_info.to_string(&generic_types))
        .collect::<Vec<String>>());
    assert_debug_snapshot!(backend.files.errors);
}

#[test]
async fn test_number_compared() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        "
fun math_abs(num) {
    if num < 0: return -num
    return num
}
",
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let symbol_table = backend.files.symbol_table.get(&file_id).unwrap();
    let generic_types = backend.files.generic_types.clone();

    // When both operands are unknown generics, they should get the broad constraint
    assert_debug_snapshot!(symbol_table.symbols);
    assert_debug_snapshot!(symbol_table
        .symbols
        .iter()
        .map(|(_, symbol_info)| symbol_info.to_string(&generic_types))
        .collect::<Vec<String>>());
    assert_debug_snapshot!(backend.files.errors);
}

#[test]
async fn test_complex_function() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        r#"
fun foo(a = 0) {
    if (a == 0) {
        fail 1
    }

    return a
}

let x = foo() exited(code) {}
let y = foo("text") exited(code) {}
"#,
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let symbol_table = backend.files.symbol_table.get(&file_id).unwrap();
    let generic_types = backend.files.generic_types.clone();

    // When both operands are unknown generics, they should get the broad constraint
    assert_debug_snapshot!(symbol_table.symbols);
    assert_debug_snapshot!(symbol_table
        .symbols
        .iter()
        .map(|(_, symbol_info)| symbol_info.to_string(&generic_types))
        .collect::<Vec<String>>());
    assert_debug_snapshot!(backend.files.errors);
}

#[test]
async fn test_array_first_generic() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        r#"
fun array_first(array) {
    fail 1
    return array[0]
}

let x = array_first([1, 23]) exited(code) {}
let y = array_first([1, 23])
"#,
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let symbol_table = backend.files.symbol_table.get(&file_id).unwrap();
    let generic_types = backend.files.generic_types.clone();

    // The parameter 'array' should be inferred as [Any]
    // The return type should be Any? (Failable(Any))
    // Variable 'x' (with failure handler) should be Int
    // Variable 'y' (without failure handler) should produce an error
    assert_debug_snapshot!(symbol_table.symbols);
    assert_debug_snapshot!(symbol_table
        .symbols
        .iter()
        .map(|(_, symbol_info)| symbol_info.to_string(&generic_types))
        .collect::<Vec<String>>());
    assert_debug_snapshot!(backend.files.errors);
}

#[test]
async fn test_failure_handlers() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        r#"
$$ failed(code) {
    echo code
} succeeded {
    echo "success"
} exited(code) {
    echo code
}
"#,
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let symbol_table = backend.files.symbol_table.get(&file_id).unwrap();
    let generic_types = backend.files.generic_types.clone();

    // When both operands are unknown generics, they should get the broad constraint
    assert_debug_snapshot!(symbol_table.symbols);
    assert_debug_snapshot!(symbol_table
        .symbols
        .iter()
        .map(|(_, symbol_info)| symbol_info.to_string(&generic_types))
        .collect::<Vec<String>>());
    assert_debug_snapshot!(backend.files.errors);
}

#[test]
async fn test_array_index_set() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        r#"
let arr = [1, 2, 3]
arr[0] = 10
"#,
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let symbol_table = backend.files.symbol_table.get(&file_id).unwrap();
    let generic_types = backend.files.generic_types.clone();

    assert_debug_snapshot!(symbol_table.symbols);
    assert_debug_snapshot!(symbol_table
        .symbols
        .iter()
        .map(|(_, symbol_info)| symbol_info.to_string(&generic_types))
        .collect::<Vec<String>>());
    assert_debug_snapshot!(backend.files.errors);
}

#[test]
async fn test_array_index_set_const_error() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        r#"
const arr = [1, 2, 3]
arr[0] = 10
"#,
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let symbol_table = backend.files.symbol_table.get(&file_id).unwrap();
    let generic_types = backend.files.generic_types.clone();

    assert_debug_snapshot!(symbol_table.symbols);
    assert_debug_snapshot!(symbol_table
        .symbols
        .iter()
        .map(|(_, symbol_info)| symbol_info.to_string(&generic_types))
        .collect::<Vec<String>>());
    // Should contain an error about assigning to a constant
    assert_debug_snapshot!(backend.files.errors);
}

#[test]
async fn test_array_index_set_non_array_error() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        r#"
let x = 5
x[0] = 10
"#,
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let symbol_table = backend.files.symbol_table.get(&file_id).unwrap();
    let generic_types = backend.files.generic_types.clone();

    assert_debug_snapshot!(symbol_table.symbols);
    assert_debug_snapshot!(symbol_table
        .symbols
        .iter()
        .map(|(_, symbol_info)| symbol_info.to_string(&generic_types))
        .collect::<Vec<String>>());
    // Should contain an error about indexing into a non-array type
    assert_debug_snapshot!(backend.files.errors);
}

#[test]
async fn test_array_index_set_type_mismatch_error() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        r#"
let arr = [1, 2, 3]
arr[0] = "hello"
"#,
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let symbol_table = backend.files.symbol_table.get(&file_id).unwrap();
    let generic_types = backend.files.generic_types.clone();

    assert_debug_snapshot!(symbol_table.symbols);
    assert_debug_snapshot!(symbol_table
        .symbols
        .iter()
        .map(|(_, symbol_info)| symbol_info.to_string(&generic_types))
        .collect::<Vec<String>>());
    // Should contain a type mismatch error
    assert_debug_snapshot!(backend.files.errors);
}

#[test]
async fn test_array_index_set_generic() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        r#"
fun set_first(arr, value) {
    arr[0] = value
}

set_first([1, 2, 3], 42)
"#,
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let symbol_table = backend.files.symbol_table.get(&file_id).unwrap();
    let generic_types = backend.files.generic_types.clone();

    // In symbol_table.symbols the definition of set_first shows generic
    // params 'arr: [Any]' and 'value: Any', but at the call site
    // set_first([1, 2, 3], 42) they are constrained to [Int] and Int.
    assert_debug_snapshot!(symbol_table.symbols);
    assert_debug_snapshot!(symbol_table
        .symbols
        .iter()
        .map(|(_, symbol_info)| symbol_info.to_string(&generic_types))
        .collect::<Vec<String>>());
    assert_debug_snapshot!(backend.files.errors);
}

#[test]
async fn test_unused_import() {
    let (service, _) = tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha050,
            Some(Arc::new(MemoryFS::new())),
        )
    });

    let backend = service.inner();
    let vfs = &backend.files.fs;

    // Create a library file with a public function
    let lib_file = {
        #[cfg(windows)]
        {
            Path::new("C:\\mylib")
        }
        #[cfg(unix)]
        {
            Path::new("/mylib")
        }
    };
    let lib_uri = Uri::from_file_path(lib_file).unwrap();

    vfs.write(
        &lib_uri.to_file_path().unwrap(),
        r#"
pub fun helper(a) {
    return a + 1
}

pub fun unused_helper(b) {
    return b + 2
}
"#,
    )
    .await
    .unwrap();

    // Create main file that imports but does not use one of them
    let main_file = {
        #[cfg(windows)]
        {
            Path::new("C:\\main.ab")
        }
        #[cfg(unix)]
        {
            Path::new("/main.ab")
        }
    };
    let main_uri = Uri::from_file_path(main_file).unwrap();

    vfs.write(
        &main_uri.to_file_path().unwrap(),
        r#"
import { helper, unused_helper } from "mylib"

let result = helper(5)
echo result
"#,
    )
    .await
    .unwrap();

    backend.open_document(&main_uri).await.unwrap();

    // unused_helper is imported but never used, so it should produce an
    // "Unused import" diagnostic. helper IS used, so no diagnostic for it.
    assert_debug_snapshot!(backend.files.unused_diagnostics);
}

#[test]
async fn test_used_import_no_warning() {
    let (service, _) = tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha050,
            Some(Arc::new(MemoryFS::new())),
        )
    });

    let backend = service.inner();
    let vfs = &backend.files.fs;

    // Create a library file with a public function
    let lib_file = {
        #[cfg(windows)]
        {
            Path::new("C:\\mylib")
        }
        #[cfg(unix)]
        {
            Path::new("/mylib")
        }
    };
    let lib_uri = Uri::from_file_path(lib_file).unwrap();

    vfs.write(
        &lib_uri.to_file_path().unwrap(),
        r#"
pub fun helper(a) {
    return a + 1
}
"#,
    )
    .await
    .unwrap();

    // Create main file that imports AND uses the function
    let main_file = {
        #[cfg(windows)]
        {
            Path::new("C:\\main.ab")
        }
        #[cfg(unix)]
        {
            Path::new("/main.ab")
        }
    };
    let main_uri = Uri::from_file_path(main_file).unwrap();

    vfs.write(
        &main_uri.to_file_path().unwrap(),
        r#"
import { helper } from "mylib"

let result = helper(5)
echo result
"#,
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&main_uri).await.unwrap();

    // helper is used, so the main file should have no unused import diagnostics
    let unused = backend.files.unused_diagnostics.get(&file_id);
    let has_unused_imports = unused
        .map(|u| u.iter().any(|(msg, _)| msg.contains("Unused import")))
        .unwrap_or(false);
    assert!(
        !has_unused_imports,
        "No unused import warning expected when symbol is used"
    );
}

#[test]
async fn test_unused_variable_shadowing() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        r#"
let a = 23
{
    let a = 22
    echo a
}
"#,
    )
    .await
    .unwrap();

    backend.open_document(&uri).await.unwrap();

    // The outer `a` is shadowed by the inner `a` and never used.
    // Only the outer `a` should be reported as unused.
    assert_debug_snapshot!(backend.files.unused_diagnostics);
}

#[test]
async fn test_unused_import_all_symbols_unused() {
    let (service, _) = tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha050,
            Some(Arc::new(MemoryFS::new())),
        )
    });

    let backend = service.inner();
    let vfs = &backend.files.fs;

    let lib_file = {
        #[cfg(windows)]
        {
            Path::new("C:\\mylib")
        }
        #[cfg(unix)]
        {
            Path::new("/mylib")
        }
    };
    let lib_uri = Uri::from_file_path(lib_file).unwrap();

    vfs.write(
        &lib_uri.to_file_path().unwrap(),
        r#"
pub fun helper(a) {
    return a + 1
}

pub fun another(b) {
    return b + 2
}
"#,
    )
    .await
    .unwrap();

    let main_file = {
        #[cfg(windows)]
        {
            Path::new("C:\\main.ab")
        }
        #[cfg(unix)]
        {
            Path::new("/main.ab")
        }
    };
    let main_uri = Uri::from_file_path(main_file).unwrap();

    vfs.write(
        &main_uri.to_file_path().unwrap(),
        r#"
import { helper, another } from "mylib"

echo "hello"
"#,
    )
    .await
    .unwrap();

    backend.open_document(&main_uri).await.unwrap();

    // Both helper and another are unused → the whole import statement
    // should be flagged as "Unused import", not individual symbols.
    assert_debug_snapshot!(backend.files.unused_diagnostics);
}

#[test]
async fn test_unused_import_star() {
    let (service, _) = tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha050,
            Some(Arc::new(MemoryFS::new())),
        )
    });

    let backend = service.inner();
    let vfs = &backend.files.fs;

    let lib_file = {
        #[cfg(windows)]
        {
            Path::new("C:\\mylib")
        }
        #[cfg(unix)]
        {
            Path::new("/mylib")
        }
    };
    let lib_uri = Uri::from_file_path(lib_file).unwrap();

    vfs.write(
        &lib_uri.to_file_path().unwrap(),
        r#"
pub fun helper(a) {
    return a + 1
}

pub fun another(b) {
    return b + 2
}
"#,
    )
    .await
    .unwrap();

    let main_file = {
        #[cfg(windows)]
        {
            Path::new("C:\\main.ab")
        }
        #[cfg(unix)]
        {
            Path::new("/main.ab")
        }
    };
    let main_uri = Uri::from_file_path(main_file).unwrap();

    vfs.write(
        &main_uri.to_file_path().unwrap(),
        r#"
import * from "mylib"

echo "hello"
"#,
    )
    .await
    .unwrap();

    backend.open_document(&main_uri).await.unwrap();

    // import * brings in helper and another, but neither is used →
    // the whole statement should be flagged as "Unused import".
    assert_debug_snapshot!(backend.files.unused_diagnostics);
}

#[test]
async fn test_import_star_used_no_warning() {
    let (service, _) = tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha050,
            Some(Arc::new(MemoryFS::new())),
        )
    });

    let backend = service.inner();
    let vfs = &backend.files.fs;

    let lib_file = {
        #[cfg(windows)]
        {
            Path::new("C:\\mylib")
        }
        #[cfg(unix)]
        {
            Path::new("/mylib")
        }
    };
    let lib_uri = Uri::from_file_path(lib_file).unwrap();

    vfs.write(
        &lib_uri.to_file_path().unwrap(),
        r#"
pub fun helper(a) {
    return a + 1
}

pub fun another(b) {
    return b + 2
}
"#,
    )
    .await
    .unwrap();

    let main_file = {
        #[cfg(windows)]
        {
            Path::new("C:\\main.ab")
        }
        #[cfg(unix)]
        {
            Path::new("/main.ab")
        }
    };
    let main_uri = Uri::from_file_path(main_file).unwrap();

    vfs.write(
        &main_uri.to_file_path().unwrap(),
        r#"
import * from "mylib"

let result = helper(5)
echo result
"#,
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&main_uri).await.unwrap();

    // helper is used, so the import * statement should NOT produce
    // an unused-import diagnostic.
    let unused = backend.files.unused_diagnostics.get(&file_id);
    let has_unused_imports = unused
        .map(|u| u.iter().any(|(msg, _)| msg.contains("Unused import")))
        .unwrap_or(false);
    assert!(
        !has_unused_imports,
        "No unused import warning expected when symbol from import * is used"
    );
}

#[test]
async fn test_unreachable_code_after_return() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        "
fun add(a, b) {
    return a + b
    echo \"unreachable\"
    let x = 5
}
",
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let warnings = backend.files.unused_diagnostics.get(&file_id).unwrap();
    let unreachable_warnings: Vec<_> = warnings
        .iter()
        .filter(|(msg, _)| msg == "Unreachable code")
        .collect();

    assert_eq!(
        unreachable_warnings.len(),
        2,
        "Expected 2 unreachable code warnings after return"
    );
}

#[test]
async fn test_unreachable_code_after_fail() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        "
main {
    fail 1
    echo \"unreachable\"
}
",
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let warnings = backend.files.unused_diagnostics.get(&file_id).unwrap();
    let unreachable_warnings: Vec<_> = warnings
        .iter()
        .filter(|(msg, _)| msg == "Unreachable code")
        .collect();

    assert_eq!(
        unreachable_warnings.len(),
        1,
        "Expected 1 unreachable code warning after fail"
    );
}

#[test]
async fn test_unreachable_code_after_break() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        "
main {
    loop {
        break
        echo \"unreachable\"
    }
}
",
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let warnings = backend.files.unused_diagnostics.get(&file_id).unwrap();
    let unreachable_warnings: Vec<_> = warnings
        .iter()
        .filter(|(msg, _)| msg == "Unreachable code")
        .collect();

    assert_eq!(
        unreachable_warnings.len(),
        1,
        "Expected 1 unreachable code warning after break"
    );
}

#[test]
async fn test_unreachable_code_after_continue() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        "
main {
    loop {
        continue
        echo \"unreachable\"
    }
}
",
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let warnings = backend.files.unused_diagnostics.get(&file_id).unwrap();
    let unreachable_warnings: Vec<_> = warnings
        .iter()
        .filter(|(msg, _)| msg == "Unreachable code")
        .collect();

    assert_eq!(
        unreachable_warnings.len(),
        1,
        "Expected 1 unreachable code warning after continue"
    );
}

#[test]
async fn test_no_unreachable_warning_without_terminator() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        "
fun add(a, b) {
    let result = a + b
    echo result
    return result
}
",
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let warnings = backend.files.unused_diagnostics.get(&file_id);
    let unreachable_count = warnings
        .map(|w| {
            w.iter()
                .filter(|(msg, _)| msg == "Unreachable code")
                .count()
        })
        .unwrap_or(0);

    assert_eq!(
        unreachable_count, 0,
        "No unreachable code warnings expected when return is the last statement"
    );
}

#[test]
async fn test_unreachable_code_comments_ignored() {
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

    vfs.write(
        &uri.to_file_path().unwrap(),
        "
fun add(a, b) {
    return a + b
    // this is a comment
}
",
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let warnings = backend.files.unused_diagnostics.get(&file_id);
    let unreachable_count = warnings
        .map(|w| {
            w.iter()
                .filter(|(msg, _)| msg == "Unreachable code")
                .count()
        })
        .unwrap_or(0);

    assert_eq!(
        unreachable_count, 0,
        "Comments after return should not trigger unreachable code warnings"
    );
}

#[test]
async fn test_auto_import_completion_updates_existing_import() {
    use tower_lsp_server::lsp_types::*;
    use tower_lsp_server::LanguageServer;

    let (service, _) = tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha050,
            Some(Arc::new(MemoryFS::new())),
        )
    });

    let backend = service.inner();
    let vfs = &backend.files.fs;

    // Create a library file with two public functions
    let lib_file = {
        #[cfg(windows)]
        {
            Path::new("C:\\mylib")
        }
        #[cfg(unix)]
        {
            Path::new("/mylib")
        }
    };
    let lib_uri = Uri::from_file_path(lib_file).unwrap();

    vfs.write(
        &lib_uri.to_file_path().unwrap(),
        r#"
pub fun join(list: [Text], delimiter: Text): Text {
    return ""
}

pub fun trim(text: Text): Text {
    return text
}
"#,
    )
    .await
    .unwrap();

    // Create main file that imports only `join` from the library
    let main_file = {
        #[cfg(windows)]
        {
            Path::new("C:\\main.ab")
        }
        #[cfg(unix)]
        {
            Path::new("/main.ab")
        }
    };
    let main_uri = Uri::from_file_path(main_file).unwrap();

    // Source text: import { join } from "mylib"
    // then on line 2: `trim` (the user is typing this function name)
    let source = "import { join } from \"mylib\"\ntrim";
    vfs.write(&main_uri.to_file_path().unwrap(), source)
        .await
        .unwrap();

    backend.open_document(&main_uri).await.unwrap();

    // Request completion at the end of "trim" (line 1, character 4)
    let completion_params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier {
                uri: main_uri.clone(),
            },
            position: Position {
                line: 1,
                character: 4,
            },
        },
        work_done_progress_params: WorkDoneProgressParams {
            work_done_token: None,
        },
        partial_result_params: PartialResultParams {
            partial_result_token: None,
        },
        context: None,
    };

    let result = backend.completion(completion_params).await.unwrap();

    let completions = match result {
        Some(CompletionResponse::Array(items)) => items,
        _ => panic!("Expected CompletionResponse::Array"),
    };

    // Find the `trim` completion item with auto-import
    let trim_completion = completions
        .iter()
        .find(|item| item.label == "trim" && item.additional_text_edits.is_some())
        .expect("Should have a 'trim' completion item with additional_text_edits for auto-import");

    // Verify the additional text edit adds ", trim" to the import statement
    let edits = trim_completion.additional_text_edits.as_ref().unwrap();
    assert_eq!(
        edits.len(),
        1,
        "Should have exactly one additional text edit"
    );

    let edit = &edits[0];
    assert_eq!(
        edit.new_text, ", trim",
        "Should insert ', trim' into the import list"
    );

    // The insertion point should be right after "join" in "import { join }"
    // "join" ends at offset 13 (0-indexed) in "import { join } from \"mylib\""
    // which is line 0, character 13
    assert_eq!(edit.range.start.line, 0, "Edit should be on line 0");
    assert_eq!(
        edit.range.start, edit.range.end,
        "Edit range should be a zero-width insertion"
    );

    // Verify the completion includes snippet text with arguments
    assert!(
        trim_completion.insert_text.is_some(),
        "Should have insert_text"
    );
    let insert_text = trim_completion.insert_text.as_ref().unwrap();
    assert!(
        insert_text.contains("trim("),
        "insert_text should contain function call: got {}",
        insert_text
    );

    // Verify label details indicate auto-import
    let label_details = trim_completion.label_details.as_ref().unwrap();
    assert!(
        label_details
            .description
            .as_ref()
            .unwrap()
            .contains("auto import"),
        "label details should mention auto import"
    );
}

#[test]
async fn test_auto_import_completion_from_stdlib_no_existing_import() {
    use tower_lsp_server::lsp_types::*;
    use tower_lsp_server::LanguageServer;

    let (service, _) = tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha050,
            Some(Arc::new(MemoryFS::new())),
        )
    });

    let backend = service.inner();
    let vfs = &backend.files.fs;

    // Save stdlib resources so the modules can be discovered and opened
    amber_analysis::stdlib::save_resources(backend).await;

    // Create a file with NO import statements at all – just a function call
    let main_file = {
        #[cfg(windows)]
        {
            Path::new("C:\\main.ab")
        }
        #[cfg(unix)]
        {
            Path::new("/main.ab")
        }
    };
    let main_uri = Uri::from_file_path(main_file).unwrap();

    // The user has typed `trim` with no imports anywhere in the file
    let source = "trim";
    vfs.write(&main_uri.to_file_path().unwrap(), source)
        .await
        .unwrap();

    backend.open_document(&main_uri).await.unwrap();

    // Request completion at the end of "trim" (line 0, character 4)
    let completion_params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier {
                uri: main_uri.clone(),
            },
            position: Position {
                line: 0,
                character: 4,
            },
        },
        work_done_progress_params: WorkDoneProgressParams {
            work_done_token: None,
        },
        partial_result_params: PartialResultParams {
            partial_result_token: None,
        },
        context: None,
    };

    let result = backend.completion(completion_params).await.unwrap();

    let completions = match result {
        Some(CompletionResponse::Array(items)) => items,
        _ => panic!("Expected CompletionResponse::Array"),
    };

    // Find the `trim` completion item that auto-imports from std/text
    let trim_completion = completions
        .iter()
        .find(|item| item.label == "trim" && item.additional_text_edits.is_some())
        .expect(
            "Should have a 'trim' completion from stdlib with additional_text_edits for auto-import",
        );

    // Verify the additional text edit inserts a new import line at the top
    let edits = trim_completion.additional_text_edits.as_ref().unwrap();
    assert_eq!(
        edits.len(),
        1,
        "Should have exactly one additional text edit"
    );

    let edit = &edits[0];
    assert!(
        edit.new_text.contains("import"),
        "Should insert an import statement, got: {}",
        edit.new_text
    );
    assert!(
        edit.new_text.contains("trim"),
        "Import statement should mention 'trim', got: {}",
        edit.new_text
    );
    assert!(
        edit.new_text.contains("std/text"),
        "Import should be from 'std/text', got: {}",
        edit.new_text
    );
    // The edit should be at the very top of the file
    assert_eq!(
        edit.range.start,
        Position {
            line: 0,
            character: 0
        },
        "Import line should be inserted at top of file"
    );

    // Verify the completion includes snippet text with arguments
    let insert_text = trim_completion.insert_text.as_ref().unwrap();
    assert!(
        insert_text.contains("trim("),
        "insert_text should contain function call: got {}",
        insert_text
    );

    // Verify label details indicate auto-import from std/text
    let label_details = trim_completion.label_details.as_ref().unwrap();
    let desc = label_details.description.as_ref().unwrap();
    assert!(
        desc.contains("auto import") && desc.contains("std/text"),
        "label details should mention auto import from std/text, got: {}",
        desc
    );
}

#[test]
async fn test_echo_not_marked_as_unused_import() {
    let (service, _) = tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha050,
            Some(Arc::new(MemoryFS::new())),
        )
    });

    let backend = service.inner();
    let vfs = &backend.files.fs;

    // Save stdlib resources so the builtin auto-import resolves
    amber_analysis::stdlib::save_resources(backend).await;

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

    vfs.write(&uri.to_file_path().unwrap(), "echo 4\n")
        .await
        .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let unused = backend.files.unused_diagnostics.get(&file_id);
    let has_unused_import = unused
        .as_ref()
        .map(|u| u.iter().any(|(msg, _)| msg.contains("Unused import")))
        .unwrap_or(false);

    assert!(
        !has_unused_import,
        "Builtin auto-import should not be flagged as unused, diagnostics: {:?}",
        unused
    );
}
