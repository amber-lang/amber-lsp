use amber_lsp::{
    backend::{AmberVersion, Backend}, fs::MemoryFS, grammar::alpha034::DataType, paths::FileId, symbol_table::types::{matches_type, GenericsMap}
};
use insta::assert_debug_snapshot;
use tower_lsp::{lsp_types::Url, LspService};

#[test]
fn test_function_definition() {
    let (service, _) = LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha034,
            Some(Box::new(MemoryFS::new())),
        )
    });

    let backend = service.inner();

    let vfs = &backend.fs;

    let file = "/main.ab";
    vfs.write(
        file,
        "
    fun foo(a, b) {
        let b = a

        return b
    }
    
    foo(1, 2)
    ",
    )
    .unwrap();

    let uri = Url::from_file_path(file).unwrap();
    let file_id = backend.open_document(&uri).unwrap();

    let symbol_table = backend.symbol_table.get(&file_id).unwrap();

    let foo_defs = symbol_table.definitions.get("foo").unwrap();
    let a_defs = symbol_table.definitions.get("a").unwrap();
    let b_defs = symbol_table.definitions.get("b").unwrap();

    assert_debug_snapshot!(foo_defs.get(&usize::MAX)); // in socpe
    assert_debug_snapshot!(foo_defs.get(&60)); // in body - out of scope

    assert_debug_snapshot!(a_defs.get(&63)); // out of scope
    assert_debug_snapshot!(a_defs.get(&17)); // in scope

    assert_debug_snapshot!(b_defs.get(&63)); // out of scope
    assert_debug_snapshot!(b_defs.get(&17)); // in scope
    assert_debug_snapshot!(b_defs.get(&60)); // in scope - variable shadowing

    assert_debug_snapshot!(symbol_table.symbols.get(&11)); // foo
}

#[test]
fn test_variable_definition() {
    let (service, _) = LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha034,
            Some(Box::new(MemoryFS::new())),
        )
    });

    let backend = service.inner();

    let vfs = &backend.fs;

    let file = "/main.ab";
    vfs.write(
        file,
        "
    let a = 1
    
    let a = 2 + a;

    echo a
    ",
    )
    .unwrap();

    let uri = Url::from_file_path(file).unwrap();
    let file_id = backend.open_document(&uri).unwrap();

    let symbol_table = backend.symbol_table.get(&file_id).unwrap();

    let a_defs = symbol_table.definitions.get("a").unwrap();

    assert_debug_snapshot!(a_defs.get(&37)); // in scope - second var init
    assert_debug_snapshot!(a_defs.get(&38)); // in scope - second var overshadowing
}

#[test]
fn test_variable_scope() {
    let (service, _) = LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha034,
            Some(Box::new(MemoryFS::new())),
        )
    });

    let backend = service.inner();

    let vfs = &backend.fs;

    let file = "/main.ab";
    vfs.write(
        file,
        "
    let a = 1;

    {
        let a = 2;
    }

    if true {
        let a = 3;
    }

    main {
        let a = 4;
    }

    fun foo() {
        let a = 5;
    }

    loop var, idx in 0..2 {
        let a = 6;
    }

    if {
        true {
            let a = 7;
        }
        false {
            let a = 8;
        }
        else {
            let a = 9;
        }
    }
    ",
    )
    .unwrap();

    let uri = Url::from_file_path(file).unwrap();
    let file_id = backend.open_document(&uri).unwrap();

    let symbol_table = backend.symbol_table.get(&file_id).unwrap();

    let a_defs = symbol_table.definitions.get("a").unwrap();

    assert_debug_snapshot!(a_defs.get(&16)); // in scope - global
    assert_debug_snapshot!(a_defs.get(&usize::MAX)); // in scope - global

    assert_debug_snapshot!(a_defs.get(&47));
    assert_debug_snapshot!(a_defs.get(&87));
    assert_debug_snapshot!(a_defs.get(&118));
    assert_debug_snapshot!(a_defs.get(&166));
    assert_debug_snapshot!(a_defs.get(&220));
    assert_debug_snapshot!(a_defs.get(&278));
}

#[test]
fn test_symbol_reference_in_expression() {
    let (service, _) = LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha034,
            Some(Box::new(MemoryFS::new())),
        )
    });

    let backend = service.inner();

    let vfs = &backend.fs;

    let file = "/main.ab";
    vfs.write(
        file,
        r#"
    let a = 1;
    let b = a + 1;

    echo b
    return b
    echo a and b or a
    echo a + b - a * b / a % b
    echo a as Num
    echo a > b
    echo a..b
    echo a then b else a
    echo !a
    echo [a, b]
    echo $cmd {a}$
    echo (a)
    echo "{a}"
    "#,
    )
    .unwrap();

    let uri = Url::from_file_path(file).unwrap();
    let file_id = backend.open_document(&uri).unwrap();

    let symbol_table = backend.symbol_table.get(&file_id).unwrap();

    let a_refs = symbol_table.references.get("a").unwrap();
    let b_refs = symbol_table.references.get("b").unwrap();

    assert_debug_snapshot!(a_refs);
    assert_debug_snapshot!(b_refs);
}

#[test]
fn test_public_definitions() {
    let (service, _) = LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha034,
            Some(Box::new(MemoryFS::new())),
        )
    });

    let backend = service.inner();

    let vfs = &backend.fs;

    let file = "/main.ab";
    vfs.write(file, r#"pub fun foo() {}"#).unwrap();

    let uri = Url::from_file_path(file).unwrap();
    let file_id = backend.open_document(&uri).unwrap();

    let symbol_table = backend.symbol_table.get(&file_id).unwrap();

    let foo_def = symbol_table.definitions.get("foo").unwrap();

    assert_debug_snapshot!(foo_def.get(&usize::MAX));
    assert_debug_snapshot!(symbol_table.symbols.get(&11));
}

#[test]
fn test_import_specific_symbols() {
    let (service, _) = LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha034,
            Some(Box::new(MemoryFS::new())),
        )
    });

    let backend = service.inner();

    let vfs = &backend.fs;

    let src_file = "/src.ab";
    let main_file = "/main.ab";
    vfs.write(src_file, r#"pub fun foo(a, b) {
        return a + b
    }"#).unwrap();
    vfs.write(
        main_file,
        r#"
    import { foo } from "src.ab"

    foo()
    "#,
    )
    .unwrap();

    let src_uri = Url::from_file_path(src_file).unwrap();
    let src_file_id = backend.open_document(&src_uri).unwrap();

    let main_uri = Url::from_file_path(main_file).unwrap();
    let main_file_id = backend.open_document(&main_uri).unwrap();

    let src_symbol_table = backend.symbol_table.get(&src_file_id).unwrap();
    let main_symbol_table = backend.symbol_table.get(&main_file_id).unwrap();

    let foo_def = src_symbol_table.definitions.get("foo").unwrap();
    let foo_def1 = main_symbol_table.definitions.get("foo").unwrap();
    let foo_refs = main_symbol_table.references.get("foo").unwrap();

    assert_debug_snapshot!(foo_def.get(&usize::MAX));
    assert_debug_snapshot!(foo_refs);
    assert_debug_snapshot!(foo_def1.get(&42));
}

#[test]
fn test_import_all_symbols() {
    let (service, _) = LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha034,
            Some(Box::new(MemoryFS::new())),
        )
    });

    let backend = service.inner();

    let vfs = &backend.fs;

    let src_file = "/src.ab";
    let main_file = "/main.ab";
    vfs.write(src_file, r#"pub fun foo() {}"#).unwrap();
    vfs.write(
        main_file,
        r#"
    import * from "src.ab"

    foo()
    "#,
    )
    .unwrap();

    let src_uri = Url::from_file_path(src_file).unwrap();
    let src_file_id = backend.open_document(&src_uri).unwrap();

    let main_uri = Url::from_file_path(main_file).unwrap();
    let main_file_id = backend.open_document(&main_uri).unwrap();

    let src_symbol_table = backend.symbol_table.get(&src_file_id).unwrap();
    let main_symbol_table = backend.symbol_table.get(&main_file_id).unwrap();

    let foo_def = src_symbol_table.definitions.get("foo").unwrap();
    let foo_def1 = main_symbol_table.definitions.get("foo").unwrap();
    let foo_refs = main_symbol_table.references.get("foo").unwrap();

    assert_debug_snapshot!(foo_def.get(&usize::MAX));
    assert_debug_snapshot!(foo_refs);
    assert_debug_snapshot!(foo_def1.get(&42));
}
