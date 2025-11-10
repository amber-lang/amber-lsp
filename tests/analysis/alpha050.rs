use std::{path::Path, sync::Arc};

use amber_lsp::{
    backend::{AmberVersion, Backend},
    fs::MemoryFS,
};
use insta::assert_debug_snapshot;
use tokio::test;
use tower_lsp_server::{lsp_types::Uri, UriExt};

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
