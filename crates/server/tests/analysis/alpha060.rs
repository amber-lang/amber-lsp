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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
    echo(\"unreachable\")
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
            AmberVersion::Alpha060,
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
    echo(\"unreachable\")
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
            AmberVersion::Alpha060,
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
        echo(\"unreachable\")
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
            AmberVersion::Alpha060,
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
        echo(\"unreachable\")
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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
            AmberVersion::Alpha060,
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

/// Helper: request completions for a source file at a given position and return
/// only the keyword-kind completion labels.
async fn keyword_labels_at(source: &str, line: u32, character: u32) -> Vec<String> {
    use tower_lsp_server::lsp_types::*;
    use tower_lsp_server::LanguageServer;

    let (service, _) = tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha060,
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

    vfs.write(&uri.to_file_path().unwrap(), source)
        .await
        .unwrap();

    backend.open_document(&uri).await.unwrap();

    let completion_params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri },
            position: Position { line, character },
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
    match result {
        Some(CompletionResponse::Array(items)) => items
            .into_iter()
            .filter(|item| item.kind == Some(CompletionItemKind::KEYWORD))
            .map(|item| item.label)
            .collect(),
        _ => vec![],
    }
}

/// Helper: request completions and return full keyword CompletionItems.
async fn keyword_completions_at(
    source: &str,
    line: u32,
    character: u32,
) -> Vec<(String, Option<String>)> {
    use tower_lsp_server::lsp_types::*;
    use tower_lsp_server::LanguageServer;

    let (service, _) = tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha060,
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

    vfs.write(&uri.to_file_path().unwrap(), source)
        .await
        .unwrap();

    backend.open_document(&uri).await.unwrap();

    let completion_params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri },
            position: Position { line, character },
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
    match result {
        Some(CompletionResponse::Array(items)) => items
            .into_iter()
            .filter(|item| item.kind == Some(CompletionItemKind::KEYWORD))
            .map(|item| (item.label, item.insert_text))
            .collect(),
        _ => vec![],
    }
}

#[test]
async fn test_keyword_break_continue_in_loop() {
    // `bre` inside a loop should offer `break` and `continue`
    let keywords = keyword_labels_at(
        "loop {\n    bre\n}",
        1,
        7, // end of "bre"
    )
    .await;

    assert!(
        keywords.contains(&"break".to_string()),
        "break should be offered inside a loop, got: {:?}",
        keywords
    );
    assert!(
        keywords.contains(&"continue".to_string()),
        "continue should be offered inside a loop, got: {:?}",
        keywords
    );
}

#[test]
async fn test_keyword_break_continue_outside_loop() {
    // `bre` outside a loop should NOT offer `break` or `continue`
    let keywords = keyword_labels_at("bre", 0, 3).await;

    assert!(
        !keywords.contains(&"break".to_string()),
        "break should NOT be offered outside a loop, got: {:?}",
        keywords
    );
    assert!(
        !keywords.contains(&"continue".to_string()),
        "continue should NOT be offered outside a loop, got: {:?}",
        keywords
    );
}

#[test]
async fn test_keyword_return_in_function() {
    // `ret` inside a function should offer `return`
    let keywords = keyword_labels_at(
        "fun foo() {\n    ret\n}",
        1,
        7, // end of "ret"
    )
    .await;

    assert!(
        keywords.contains(&"return".to_string()),
        "return should be offered inside a function, got: {:?}",
        keywords
    );
}

#[test]
async fn test_keyword_return_outside_function() {
    // `ret` at top-level should NOT offer `return`
    let keywords = keyword_labels_at("ret", 0, 3).await;

    assert!(
        !keywords.contains(&"return".to_string()),
        "return should NOT be offered outside a function, got: {:?}",
        keywords
    );
}

#[test]
async fn test_keyword_fail_in_function() {
    // `fai` inside a function should offer `fail`
    let keywords = keyword_labels_at("fun foo() {\n    fai\n}", 1, 7).await;

    assert!(
        keywords.contains(&"fail".to_string()),
        "fail should be offered inside a function, got: {:?}",
        keywords
    );
}

#[test]
async fn test_keyword_fail_in_main() {
    // `fai` inside main block should offer `fail`
    let keywords = keyword_labels_at("main {\n    fai\n}", 1, 7).await;

    assert!(
        keywords.contains(&"fail".to_string()),
        "fail should be offered inside main block, got: {:?}",
        keywords
    );
}

#[test]
async fn test_keyword_fail_outside_function_and_main() {
    // `fai` at top-level should NOT offer `fail`
    let keywords = keyword_labels_at("fai", 0, 3).await;

    assert!(
        !keywords.contains(&"fail".to_string()),
        "fail should NOT be offered outside function/main, got: {:?}",
        keywords
    );
}

#[test]
async fn test_keyword_failed_after_command() {
    // `fai` after a command expression should offer `failed`, `succeeded`, `exited`
    let keywords = keyword_labels_at("trust $ echo hello $ fai", 0, 24).await;

    assert!(
        keywords.contains(&"failed".to_string()),
        "failed should be offered after a command expression, got: {:?}",
        keywords
    );
    assert!(
        keywords.contains(&"succeeded".to_string()),
        "succeeded should be offered after a command expression, got: {:?}",
        keywords
    );
    assert!(
        keywords.contains(&"exited".to_string()),
        "exited should be offered after a command expression, got: {:?}",
        keywords
    );
}

#[test]
async fn test_keyword_failed_always_available() {
    // `fai` at top-level should still offer failable handler keywords
    let keywords = keyword_labels_at("fai", 0, 3).await;

    assert!(
        keywords.contains(&"failed".to_string()),
        "failed should be offered, got: {:?}",
        keywords
    );
    assert!(
        keywords.contains(&"succeeded".to_string()),
        "succeeded should be offered, got: {:?}",
        keywords
    );
    assert!(
        keywords.contains(&"exited".to_string()),
        "exited should be offered, got: {:?}",
        keywords
    );
}

#[test]
async fn test_keyword_general_always_available() {
    // General keywords should always be available at statement position
    let keywords = keyword_labels_at("i", 0, 1).await;

    for kw in [
        "if",
        "loop",
        "for",
        "let",
        "const",
        "fun",
        "pub",
        "import",
        "main",
        "true",
        "false",
        "null",
        "unsafe",
        "silent",
        "trust",
        "sudo",
        "failed",
        "succeeded",
        "exited",
    ] {
        assert!(
            keywords.contains(&kw.to_string()),
            "keyword '{}' should always be available, got: {:?}",
            kw,
            keywords
        );
    }
}

#[test]
async fn test_keyword_break_in_nested_loop() {
    // `bre` inside a nested loop (for inside function) should offer `break`
    let keywords = keyword_labels_at(
        "fun foo() {\n    for x in [1, 2, 3] {\n        bre\n    }\n}",
        2,
        11,
    )
    .await;

    assert!(
        keywords.contains(&"break".to_string()),
        "break should be offered inside a nested loop, got: {:?}",
        keywords
    );
    assert!(
        keywords.contains(&"return".to_string()),
        "return should also be offered inside a function, got: {:?}",
        keywords
    );
}

#[test]
async fn test_keyword_failed_after_function_call() {
    // `fai` after a function call should offer failable handler keywords
    let keywords = keyword_labels_at("fun foo() {}\ntrust foo() fai", 1, 15).await;

    assert!(
        keywords.contains(&"failed".to_string()),
        "failed should be offered after a function call, got: {:?}",
        keywords
    );
}

#[test]
async fn test_keyword_snippets() {
    // Verify that structure keywords have snippet insert text
    let completions = keyword_completions_at("i", 0, 1).await;

    let find = |label: &str| -> Option<String> {
        completions
            .iter()
            .find(|(l, _)| l == label)
            .and_then(|(_, snippet)| snippet.clone())
    };

    // `if` should expand to a block with cursor on condition
    let if_snippet = find("if").expect("if should have a snippet");
    assert!(
        if_snippet.contains("${1:condition}") && if_snippet.contains('{'),
        "if snippet should have condition placeholder and block, got: {}",
        if_snippet
    );

    // `loop` should expand to a block
    let loop_snippet = find("loop").expect("loop should have a snippet");
    assert!(
        loop_snippet.contains('{'),
        "loop snippet should have a block, got: {}",
        loop_snippet
    );

    // `for` should expand with item in iterable
    let for_snippet = find("for").expect("for should have a snippet");
    assert!(
        for_snippet.contains("${1:item}") && for_snippet.contains("${2:iterable}"),
        "for snippet should have item and iterable placeholders, got: {}",
        for_snippet
    );

    // `fun` should expand to function definition
    let fun_snippet = find("fun").expect("fun should have a snippet");
    assert!(
        fun_snippet.contains("${1:name}") && fun_snippet.contains('('),
        "fun snippet should have name placeholder and parens, got: {}",
        fun_snippet
    );

    // `import` should focus on path first ($1), then names ($2)
    let import_snippet = find("import").expect("import should have a snippet");
    assert!(
        import_snippet.contains("\"$1\"") && import_snippet.contains("$2"),
        "import snippet should focus on path ($1) then names ($2), got: {}",
        import_snippet
    );

    // `let` / `const` should have name placeholder
    let let_snippet = find("let").expect("let should have a snippet");
    assert!(
        let_snippet.contains("${1:name}"),
        "let snippet should have name placeholder, got: {}",
        let_snippet
    );

    // `main` should expand to a block
    let main_snippet = find("main").expect("main should have a snippet");
    assert!(
        main_snippet.contains('{'),
        "main snippet should have a block, got: {}",
        main_snippet
    );

    // `true`, `false`, `null` should NOT have snippets
    assert!(find("true").is_none(), "true should not have a snippet");
    assert!(find("false").is_none(), "false should not have a snippet");
    assert!(find("null").is_none(), "null should not have a snippet");
}

#[test]
async fn test_echo_not_marked_as_unused_import() {
    let (service, _) = tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha060,
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

#[test]
async fn test_imported_generic_not_narrowed_across_calls() {
    let (service, _) = tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha060,
            Some(Arc::new(MemoryFS::new())),
        )
    });

    let backend = service.inner();
    let vfs = &backend.files.fs;

    // Create a library file with a generic function (like builtin len)
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
        r#"pub fun my_len(value): Int {}
"#,
    )
    .await
    .unwrap();

    // Create a second library that imports my_len, calls it with a generic
    // param, AND indexes into the same param. This triggers the bug:
    // array indexing constrains the generic chain (G_arr → G_len) in the
    // scoped map and propagate_nested_generics leaks the scoped constraint
    // on G_len back to the global map, permanently narrowing len's param.
    let lib2_file = {
        #[cfg(windows)]
        {
            Path::new("C:\\mylib2")
        }
        #[cfg(unix)]
        {
            Path::new("/mylib2")
        }
    };
    let lib2_uri = Uri::from_file_path(lib2_file).unwrap();

    vfs.write(
        &lib2_uri.to_file_path().unwrap(),
        r#"import { my_len } from "mylib"

pub fun first(array) {
    if my_len(array) == 0:
        fail 1
    return array[0]
}
"#,
    )
    .await
    .unwrap();

    // Create main file that imports from both libs and calls my_len with different types
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
        r#"import { my_len } from "mylib"
import { first } from "mylib2"

my_len("asd")
my_len([1, 2])
"#,
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&main_uri).await.unwrap();

    // There should be no type-mismatch errors: my_len's parameter is generic (Any)
    // and should accept both Text and [Int] without narrowing across call sites.
    let errors = backend.files.errors.get(&file_id);
    let error_list: Vec<_> = errors
        .iter()
        .flat_map(|e| e.iter().cloned())
        .filter(|(msg, _)| msg.contains("Expected type"))
        .collect();
    assert!(
        error_list.is_empty(),
        "Expected no type-mismatch errors when calling imported generic with different types, got: {:?}",
        error_list,
    );
}

#[test]
async fn test_local_generic_not_narrowed_across_calls() {
    let (service, _) = tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha060,
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
        r#"fun my_len(value): Int {}

my_len("asd")
my_len([1, 2])
"#,
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    // There should be no type-mismatch errors on the function calls.
    // (there may be a "returns Null but expected Int" on the empty body - ignore it)
    let errors = backend.files.errors.get(&file_id);
    let error_list: Vec<_> = errors
        .iter()
        .flat_map(|e| e.iter().cloned())
        .filter(|(msg, _)| msg.contains("Expected type"))
        .collect();
    assert!(
        error_list.is_empty(),
        "Expected no type-mismatch errors when calling local generic with different types, got: {:?}",
        error_list,
    );
}

#[test]
async fn test_builtin_generic_not_narrowed_inside_function_body() {
    let (service, _) = tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha060,
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
        r#"pub fun foo() {
    len("asd")
    len([1, 2, 3])
}
"#,
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    // The second len() call should NOT get a type error: len's generic
    // param must be reset between calls so it accepts both Text and [Int].
    let errors = backend.files.errors.get(&file_id);
    let error_list: Vec<_> = errors
        .iter()
        .flat_map(|e| e.iter().cloned())
        .filter(|(msg, _)| msg.contains("Expected type"))
        .collect();
    assert!(
        error_list.is_empty(),
        "Expected no type-mismatch errors for len() with different types inside a function body, got: {:?}",
        error_list,
    );
}

#[test]
async fn test_user_generic_not_narrowed_inside_function_body() {
    let (service, _) = tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha060,
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
        r#"fun my_len(value): Int {}

pub fun foo() {
    my_len("asd")
    my_len([1, 2, 3])
}
"#,
    )
    .await
    .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let errors = backend.files.errors.get(&file_id);
    let error_list: Vec<_> = errors
        .iter()
        .flat_map(|e| e.iter().cloned())
        .filter(|(msg, _)| msg.contains("Expected type"))
        .collect();
    assert!(
        error_list.is_empty(),
        "Expected no type-mismatch errors for user generic function with different types inside a function body, got: {:?}",
        error_list,
    );
}

// Helper to collect error messages from a single-file source
async fn errors_from_source(source: &str) -> Vec<String> {
    let (service, _) = tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha060,
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

    vfs.write(&uri.to_file_path().unwrap(), source)
        .await
        .unwrap();

    let file_id = backend.open_document(&uri).await.unwrap();

    let errors = backend.files.errors.get(&file_id);
    errors
        .iter()
        .flat_map(|e| e.iter().cloned())
        .map(|(msg, _)| msg)
        .collect()
}

#[test]
async fn test_error_duplicate_parameter_name() {
    let errors = errors_from_source(
        r#"fun foo(a, b, a) {
}
"#,
    )
    .await;
    assert!(
        errors
            .iter()
            .any(|e| e.contains("Duplicate parameter name")),
        "Expected duplicate parameter name error, got: {:?}",
        errors,
    );
}

#[test]
async fn test_no_error_unique_parameter_names() {
    let errors = errors_from_source(
        r#"fun foo(a, b, c) {
}
"#,
    )
    .await;
    assert!(
        !errors
            .iter()
            .any(|e| e.contains("Duplicate parameter name")),
        "Expected no duplicate parameter name error, got: {:?}",
        errors,
    );
}

#[test]
async fn test_error_failable_return_type_but_no_propagation() {
    let errors = errors_from_source(
        r#"fun foo(): Int? {
    return 42
}
"#,
    )
    .await;
    assert!(
        errors
            .iter()
            .any(|e| e.contains("Return type is declared as failable")),
        "Expected failable return type error, got: {:?}",
        errors,
    );
}

#[test]
async fn test_no_error_failable_return_type_with_propagation() {
    let errors = errors_from_source(
        r#"fun foo(): Text? {
    return $echo "hello"$?
}
"#,
    )
    .await;
    assert!(
        !errors
            .iter()
            .any(|e| e.contains("Return type is declared as failable")),
        "Expected no failable return type error, got: {:?}",
        errors,
    );
}

#[test]
async fn test_error_duplicate_command_modifier() {
    let errors = errors_from_source(
        r#"main {
    trust trust $echo "hello"$ failed {
        echo "error"
    }
}
"#,
    )
    .await;
    assert!(
        errors
            .iter()
            .any(|e| e.contains("Duplicate command modifier")),
        "Expected duplicate command modifier error, got: {:?}",
        errors,
    );
}

#[test]
async fn test_no_error_different_command_modifiers() {
    let errors = errors_from_source(
        r#"main {
    silent trust $echo "hello"$ failed {
        echo "error"
    }
}
"#,
    )
    .await;
    assert!(
        !errors
            .iter()
            .any(|e| e.contains("Duplicate command modifier")),
        "Expected no duplicate command modifier error, got: {:?}",
        errors,
    );
}

#[test]
async fn test_error_duplicate_failed_handler() {
    let errors = errors_from_source(
        r#"main {
    $echo "hello"$ failed {
        echo "error1"
    } failed {
        echo "error2"
    }
}
"#,
    )
    .await;
    assert!(
        errors
            .iter()
            .any(|e| e.contains("Duplicate 'failed' handler")),
        "Expected duplicate failed handler error, got: {:?}",
        errors,
    );
}

#[test]
async fn test_error_duplicate_succeeded_handler() {
    let errors = errors_from_source(
        r#"main {
    $echo "hello"$ failed {
        echo "error"
    } succeeded {
        echo "ok1"
    } succeeded {
        echo "ok2"
    }
}
"#,
    )
    .await;
    assert!(
        errors
            .iter()
            .any(|e| e.contains("Duplicate 'succeeded' handler")),
        "Expected duplicate succeeded handler error, got: {:?}",
        errors,
    );
}

#[test]
async fn test_error_ternary_type_mismatch() {
    let errors = errors_from_source(
        r#"let x = true then 42 else "hello"
"#,
    )
    .await;
    assert!(
        errors
            .iter()
            .any(|e| e.contains("Ternary operation must evaluate to one type")),
        "Expected ternary type mismatch error, got: {:?}",
        errors,
    );
}

#[test]
async fn test_no_error_ternary_same_type() {
    let errors = errors_from_source(
        r#"let x = true then 42 else 99
"#,
    )
    .await;
    assert!(
        !errors
            .iter()
            .any(|e| e.contains("Ternary operation must evaluate to one type")),
        "Expected no ternary type mismatch error, got: {:?}",
        errors,
    );
}

#[test]
async fn test_error_propagate_with_trust() {
    let errors = errors_from_source(
        r#"main {
    trust $echo "hello"$?
}
"#,
    )
    .await;
    assert!(
        errors
            .iter()
            .any(|e| e.contains("'trust' modifier cannot be used with failure handlers")),
        "Expected trust with handler error, got: {:?}",
        errors,
    );
}

#[test]
async fn test_error_trust_with_failed_handler() {
    let errors = errors_from_source(
        r#"main {
    trust $echo "hello"$ failed {
        echo "error"
    }
}
"#,
    )
    .await;
    assert!(
        errors
            .iter()
            .any(|e| e.contains("'trust' modifier cannot be used with failure handlers")),
        "Expected trust with handler error, got: {:?}",
        errors,
    );
}

#[test]
async fn test_error_trust_with_succeeded_handler() {
    let errors = errors_from_source(
        r#"main {
    trust $echo "hello"$ succeeded {
        echo "ok"
    }
}
"#,
    )
    .await;
    assert!(
        errors
            .iter()
            .any(|e| e.contains("'trust' modifier cannot be used with failure handlers")),
        "Expected trust with handler error, got: {:?}",
        errors,
    );
}

#[test]
async fn test_error_propagate_with_other_handlers() {
    let errors = errors_from_source(
        r#"main {
    $echo "hello"$? succeeded {
        echo "ok"
    }
}
"#,
    )
    .await;
    assert!(
        errors
            .iter()
            .any(|e| e.contains("'?' operator cannot be used with other failure handlers")),
        "Expected propagate with other handlers error, got: {:?}",
        errors,
    );
}
