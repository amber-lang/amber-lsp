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
