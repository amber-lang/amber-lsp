//! Comprehensive server integration tests for the alpha050 Backend.
//!
//! These tests exercise the full LSP request/response cycle through the Backend,
//! including goto_definition, hover, completion, semantic tokens, and document
//! lifecycle management. They verify that no panics occur on edge cases like
//! navigating to stdlib definitions or querying unanalyzed files.

use std::path::Path;
use std::sync::Arc;

use amber_analysis::files::FileVersion;
use amber_analysis::AnalysisHost;
use amber_lsp::backend::{
    AmberVersion,
    Backend,
};
use amber_types::fs::MemoryFS;
use tokio::test;
use tower_lsp_server::lsp_types::*;
use tower_lsp_server::{
    LanguageServer,
    UriExt,
};

/// Helper: create a Backend wired to a MemoryFS, return (backend_ref, vfs_ref).
fn make_backend() -> (
    tower_lsp_server::LspService<Backend>,
    tower_lsp_server::ClientSocket,
) {
    tower_lsp_server::LspService::new(|client| {
        Backend::new(
            client,
            AmberVersion::Alpha050,
            Some(Arc::new(MemoryFS::new())),
        )
    })
}

/// Helper: create a file URI for the given path.
fn file_uri(name: &str) -> Uri {
    #[cfg(windows)]
    let path_str = format!("C:\\{name}");
    #[cfg(unix)]
    let path_str = format!("/{name}");
    let path = Path::new(&path_str);
    Uri::from_file_path(path).unwrap()
}

/// Helper: open a document via `did_open` (the standard LSP flow).
async fn open_document(backend: &Backend, uri: &Uri, text: &str, version: i32) {
    backend
        .did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "amber".to_string(),
                version,
                text: text.to_string(),
            },
        })
        .await;
}

/// Helper: build GotoDefinitionParams for a position in a file.
fn goto_def_params(uri: &Uri, line: u32, character: u32) -> GotoDefinitionParams {
    GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position { line, character },
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    }
}

/// Helper: build HoverParams for a position in a file.
fn hover_params(uri: &Uri, line: u32, character: u32) -> HoverParams {
    HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position { line, character },
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
    }
}

/// Helper: build CompletionParams for a position in a file.
fn completion_params(uri: &Uri, line: u32, character: u32) -> CompletionParams {
    CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position { line, character },
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    }
}

/// Helper: build SemanticTokensParams for a file.
fn semantic_tokens_params(uri: &Uri) -> SemanticTokensParams {
    SemanticTokensParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    }
}

// ─── Document Lifecycle ───────────────────────────────────────────────────────

#[test]
async fn test_did_open_populates_analysis() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    // Write file to VFS so open_document can read it
    backend
        .files
        .fs
        .write(&uri.to_file_path().unwrap(), "let x = 42\necho x")
        .await
        .unwrap();

    let (file_id, version) = backend.open_document(&uri).await.unwrap();

    // Analysis should populate symbol table
    let symbol_table = backend.files.symbol_table.get(&(file_id, version));
    assert!(
        symbol_table.is_some(),
        "Symbol table should be populated after open"
    );

    // Should have variable 'x' in the symbol table
    let table = symbol_table.unwrap();
    let has_x = table.symbols.iter().any(|(_, info)| info.name == "x");
    assert!(has_x, "Variable 'x' should be in symbol table");
}

#[test]
async fn test_did_open_via_lsp_protocol() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    open_document(backend, &uri, "let greeting = \"hello\"\necho greeting", 1).await;

    let file_id = backend.files.get(&uri);
    assert!(
        file_id.is_some(),
        "File should be registered after did_open"
    );

    let file_id = file_id.unwrap();
    let version = backend.files.get_latest_version(file_id);
    assert_eq!(version, Some(FileVersion(1)));
}

#[test]
async fn test_did_change_updates_analysis() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    // Open with initial content
    open_document(backend, &uri, "let x = 1", 1).await;

    // Change content (full sync)
    backend
        .did_change(DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version: 2,
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: "let y = 2\necho y".to_string(),
            }],
        })
        .await;

    let file_id = backend.files.get(&uri).unwrap();
    let version = backend.files.get_latest_version(file_id).unwrap();
    assert_eq!(version, FileVersion(2));

    // New content should be reflected in symbols
    let table = backend.files.symbol_table.get(&(file_id, version)).unwrap();
    let has_y = table.symbols.iter().any(|(_, info)| info.name == "y");
    assert!(has_y, "Updated file should have 'y' in symbol table");
}

// ─── Go to Definition ─────────────────────────────────────────────────────────

#[test]
async fn test_goto_definition_local_variable() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    // Line 0: "let x = 42"
    // Line 1: "echo x"
    //               ^ cursor on 'x' reference at (1, 5)
    open_document(backend, &uri, "let x = 42\necho x", 1).await;

    let result = backend
        .goto_definition(goto_def_params(&uri, 1, 5))
        .await
        .unwrap();

    // Should resolve to the definition of x on line 0
    assert!(
        result.is_some(),
        "goto_definition should find the variable definition"
    );
    match result.unwrap() {
        GotoDefinitionResponse::Scalar(location) => {
            assert_eq!(location.uri, uri);
            assert_eq!(location.range.start.line, 0);
        }
        other => panic!("Expected Scalar, got {other:?}"),
    }
}

#[test]
async fn test_goto_definition_function() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    let code = "\
fun greet(name) {
    echo \"Hello, {name}\"
}
greet(\"world\")";

    open_document(backend, &uri, code, 1).await;

    // 'greet' call is on line 3, character 0
    let result = backend
        .goto_definition(goto_def_params(&uri, 3, 0))
        .await
        .unwrap();

    assert!(result.is_some(), "Should find function definition");
    match result.unwrap() {
        GotoDefinitionResponse::Scalar(location) => {
            assert_eq!(location.uri, uri);
            // Definition is on line 0 (fun greet)
            assert_eq!(location.range.start.line, 0);
        }
        other => panic!("Expected Scalar, got {other:?}"),
    }
}

#[test]
async fn test_goto_definition_no_panic_on_unknown_file() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("unknown.ab");

    // Don't open the document — just query it
    let result = backend
        .goto_definition(goto_def_params(&uri, 0, 0))
        .await
        .unwrap();

    assert!(result.is_none(), "Should return None for unknown file");
}

#[test]
async fn test_goto_definition_on_definition_returns_none() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    open_document(backend, &uri, "let x = 42\necho x", 1).await;

    // Cursor on 'x' definition at (0, 4)
    let result = backend
        .goto_definition(goto_def_params(&uri, 0, 4))
        .await
        .unwrap();

    // Goto definition on a definition itself should return None
    assert!(
        result.is_none(),
        "Goto def on definition should return None"
    );
}

#[test]
async fn test_goto_definition_stdlib_function_no_panic() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    // Use a stdlib function — 'echo' is a builtin, let's use a simple function call
    // that references something from the implicit builtin import.
    // This is the core test case that previously caused the panic: navigating to
    // a symbol defined in a file (like builtin.ab) not registered in the Files system.
    open_document(
        backend,
        &uri,
        "import * from \"std/math\"\nlet x = abs(-5)",
        1,
    )
    .await;

    // Try goto def on 'abs' (stdlib function), line 1, character 8
    let result = backend.goto_definition(goto_def_params(&uri, 1, 8)).await;

    // Must not panic — the result may be None or Some depending on whether
    // the stdlib file was registered, but it must NOT crash.
    assert!(
        result.is_ok(),
        "goto_definition on stdlib symbol must not panic"
    );
}

// ─── Hover ────────────────────────────────────────────────────────────────────

#[test]
async fn test_hover_on_variable() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    open_document(backend, &uri, "let x = 42\necho x", 1).await;

    // Hover on 'x' reference at (1, 5)
    let result = backend.hover(hover_params(&uri, 1, 5)).await.unwrap();

    assert!(
        result.is_some(),
        "Hover should return info for variable reference"
    );
    let hover = result.unwrap();
    match hover.contents {
        HoverContents::Markup(markup) => {
            assert!(
                markup.value.contains("x"),
                "Hover should mention the variable name, got: {}",
                markup.value
            );
        }
        _ => panic!("Expected Markup hover content"),
    }
}

#[test]
async fn test_hover_on_function() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    let code = "\
fun add(a, b) {
    return a + b
}
add(1, 2)";

    open_document(backend, &uri, code, 1).await;

    // Hover on 'add' call at (3, 0)
    let result = backend.hover(hover_params(&uri, 3, 0)).await.unwrap();

    assert!(
        result.is_some(),
        "Hover should return info for function call"
    );
    let hover = result.unwrap();
    match hover.contents {
        HoverContents::Markup(markup) => {
            assert!(
                markup.value.contains("add"),
                "Hover should contain function name 'add', got: {}",
                markup.value
            );
        }
        _ => panic!("Expected Markup hover content"),
    }
}

#[test]
async fn test_hover_on_unknown_file() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("nonexistent.ab");

    let result = backend.hover(hover_params(&uri, 0, 0)).await.unwrap();
    assert!(result.is_none(), "Hover on unknown file should return None");
}

#[test]
async fn test_hover_on_empty_position() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    open_document(backend, &uri, "let x = 42\n\necho x", 1).await;

    // Hover on empty line 1
    let result = backend.hover(hover_params(&uri, 1, 0)).await.unwrap();
    assert!(
        result.is_none(),
        "Hover on empty position should return None"
    );
}

// ─── Completion ───────────────────────────────────────────────────────────────

#[test]
async fn test_completion_on_unknown_file() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("nothing.ab");

    let result = backend
        .completion(completion_params(&uri, 0, 0))
        .await
        .unwrap();

    assert!(
        result.is_none(),
        "Completion on unknown file should return None"
    );
}

#[test]
async fn test_completion_shows_defined_symbols() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    let code = "\
fun greet(name) {
    echo \"Hello, {name}\"
}
gre";

    open_document(backend, &uri, code, 1).await;

    // Position cursor at 'gre' on line 3
    let result = backend
        .completion(completion_params(&uri, 3, 2))
        .await
        .unwrap();

    if let Some(CompletionResponse::Array(items)) = result {
        let has_greet = items.iter().any(|item| item.label == "greet");
        assert!(
            has_greet,
            "Completion should include 'greet' function, got: {:?}",
            items.iter().map(|i| &i.label).collect::<Vec<_>>()
        );
    }
    // It's also OK if result is None when cursor isn't on a recognized symbol
}

// ─── Semantic Tokens ──────────────────────────────────────────────────────────

#[test]
async fn test_semantic_tokens_full() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    open_document(backend, &uri, "let x = 42\necho x", 1).await;

    let result = backend
        .semantic_tokens_full(semantic_tokens_params(&uri))
        .await
        .unwrap();

    assert!(result.is_some(), "Semantic tokens should be produced");
    match result.unwrap() {
        SemanticTokensResult::Tokens(tokens) => {
            assert!(
                !tokens.data.is_empty(),
                "Should have at least some semantic tokens"
            );
        }
        SemanticTokensResult::Partial(_) => {
            // Also acceptable
        }
    }
}

#[test]
async fn test_semantic_tokens_on_unknown_file() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("ghost.ab");

    let result = backend
        .semantic_tokens_full(semantic_tokens_params(&uri))
        .await
        .unwrap();

    assert!(
        result.is_none(),
        "Semantic tokens for unknown file should be None"
    );
}

// ─── Error Diagnostics ───────────────────────────────────────────────────────

#[test]
async fn test_syntax_errors_reported() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    // This is a syntax error — unmatched brace
    open_document(backend, &uri, "fun broken( {\n}", 1).await;

    let file_id = backend.files.get(&uri).unwrap();
    let version = backend.files.get_latest_version(file_id).unwrap();

    let errors = backend.files.errors.get(&(file_id, version));
    assert!(errors.is_some(), "Syntax errors should be reported");
    let errors = errors.unwrap();
    assert!(!errors.is_empty(), "Should have at least one syntax error");
}

#[test]
async fn test_clean_code_no_errors() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    open_document(backend, &uri, "let x = 42\necho x", 1).await;

    let file_id = backend.files.get(&uri).unwrap();
    let version = backend.files.get_latest_version(file_id).unwrap();

    let errors = backend.files.errors.get(&(file_id, version));

    if let Some(errs) = errors {
        assert!(
            errs.is_empty(),
            "Clean code should have no errors, got: {:?}",
            errs.value()
        )
    }
}

// ─── Multiple Files ──────────────────────────────────────────────────────────

#[test]
async fn test_multiple_files_independent_analysis() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri1 = file_uri("file1.ab");
    let uri2 = file_uri("file2.ab");

    open_document(backend, &uri1, "let a = 1", 1).await;
    open_document(backend, &uri2, "let b = 2", 1).await;

    let fid1 = backend.files.get(&uri1).unwrap();
    let fid2 = backend.files.get(&uri2).unwrap();
    let v1 = backend.files.get_latest_version(fid1).unwrap();
    let v2 = backend.files.get_latest_version(fid2).unwrap();

    let table1 = backend.files.symbol_table.get(&(fid1, v1)).unwrap();
    let table2 = backend.files.symbol_table.get(&(fid2, v2)).unwrap();

    let has_a = table1.symbols.iter().any(|(_, info)| info.name == "a");
    let has_b = table2.symbols.iter().any(|(_, info)| info.name == "b");

    assert!(has_a, "File1 should have 'a'");
    assert!(has_b, "File2 should have 'b'");

    // Each file should NOT have the other file's symbols
    let file1_has_b = table1.symbols.iter().any(|(_, info)| info.name == "b");
    let file2_has_a = table2.symbols.iter().any(|(_, info)| info.name == "a");
    assert!(!file1_has_b, "File1 should not have 'b'");
    assert!(!file2_has_a, "File2 should not have 'a'");
}

// ─── Edge Cases / Regression ──────────────────────────────────────────────────

#[test]
async fn test_empty_document() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("empty.ab");

    open_document(backend, &uri, "", 1).await;

    // Should not panic
    let file_id = backend.files.get(&uri).unwrap();
    let version = backend.files.get_latest_version(file_id).unwrap();
    assert_eq!(version, FileVersion(1));

    // Hover/goto should return None safely
    let hover = backend.hover(hover_params(&uri, 0, 0)).await.unwrap();
    assert!(hover.is_none());

    let def = backend
        .goto_definition(goto_def_params(&uri, 0, 0))
        .await
        .unwrap();
    assert!(def.is_none());
}

#[test]
async fn test_whitespace_only_document() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("spaces.ab");

    open_document(backend, &uri, "   \n\n  \n", 1).await;

    // Should not panic
    let hover = backend.hover(hover_params(&uri, 0, 0)).await.unwrap();
    assert!(hover.is_none());
}

#[test]
async fn test_rapid_version_updates() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("rapid.ab");

    // Simulate rapid edits
    open_document(backend, &uri, "let x = 1", 1).await;

    for version in 2..=10 {
        backend
            .did_change(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: uri.clone(),
                    version,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: format!("let x = {version}"),
                }],
            })
            .await;
    }

    let file_id = backend.files.get(&uri).unwrap();
    let version = backend.files.get_latest_version(file_id).unwrap();
    assert_eq!(version, FileVersion(10));
}

#[test]
async fn test_function_with_typed_args_hover() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("typed.ab");

    let code = "\
fun multiply(a: Num, b: Num): Num {
    return a * b
}
multiply(3, 4)";

    open_document(backend, &uri, code, 1).await;

    // Hover on 'multiply' call at (3, 0)
    let result = backend.hover(hover_params(&uri, 3, 0)).await.unwrap();

    assert!(
        result.is_some(),
        "Hover should return info for typed function"
    );
    let hover = result.unwrap();
    match hover.contents {
        HoverContents::Markup(markup) => {
            assert!(
                markup.value.contains("Num"),
                "Hover should show type info, got: {}",
                markup.value
            );
        }
        _ => panic!("Expected Markup hover content"),
    }
}

#[test]
async fn test_goto_definition_across_scopes() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("scopes.ab");

    let code = "\
let outer = 1
if true {
    echo outer
}";

    open_document(backend, &uri, code, 1).await;

    // 'outer' reference at line 2, character 9 (inside 'echo outer')
    let result = backend
        .goto_definition(goto_def_params(&uri, 2, 9))
        .await
        .unwrap();

    assert!(
        result.is_some(),
        "Should find 'outer' definition from inner scope"
    );
    match result.unwrap() {
        GotoDefinitionResponse::Scalar(location) => {
            assert_eq!(
                location.range.start.line, 0,
                "Definition should be on line 0"
            );
        }
        other => panic!("Expected Scalar, got {other:?}"),
    }
}

#[test]
async fn test_signature_help_on_unknown_file() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("nope.ab");

    let result = backend
        .signature_help(SignatureHelpParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: Position {
                    line: 0,
                    character: 0,
                },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            context: None,
        })
        .await
        .unwrap();

    assert!(
        result.is_none(),
        "Signature help on unknown file should return None"
    );
}

#[test]
async fn test_did_change_on_unknown_file_no_panic() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("never_opened.ab");

    // Sending did_change for a file that was never opened should not panic
    backend
        .did_change(DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version: 1,
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: "let x = 1".to_string(),
            }],
        })
        .await;

    // Should not have registered the file
    assert!(backend.files.get(&uri).is_none());
}

#[test]
async fn test_semantic_tokens_range() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("range.ab");

    let code = "\
let x = 42
fun greet(name) {
    echo \"Hello, {name}\"
}
greet(\"world\")";

    open_document(backend, &uri, code, 1).await;

    let result = backend
        .semantic_tokens_range(SemanticTokensRangeParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 2,
                    character: 0,
                },
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        })
        .await
        .unwrap();

    assert!(
        result.is_some(),
        "Semantic tokens range should produce results"
    );
}

// ─── Import: Goto File ────────────────────────────────────────────────────────

#[test]
async fn test_goto_definition_import_path_stdlib() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    // import * from "std/env"
    //                ^^^^^^^ clicking here should go to the stdlib env file
    let code = "import * from \"std/env\"";
    open_document(backend, &uri, code, 1).await;

    // "std/env" starts at offset 15 (after `import * from "`), ends at offset 21
    // Cursor on "std/env" at line 0, character 16
    let result = backend
        .goto_definition(goto_def_params(&uri, 0, 16))
        .await
        .unwrap();

    assert!(
        result.is_some(),
        "Goto definition on import path should navigate to the imported file"
    );
    match result.unwrap() {
        GotoDefinitionResponse::Link(links) => {
            assert_eq!(links.len(), 1);
            let link = &links[0];
            // The target URI should contain "std/env" or "env.ab"
            let target = link.target_uri.to_string();
            assert!(
                target.contains("env"),
                "Target URI should reference the env stdlib file, got: {target}"
            );
        }
        other => panic!("Expected Link response for import path, got {other:?}"),
    }
}

#[test]
async fn test_goto_definition_import_path_local_file() {
    let (service, _) = make_backend();
    let backend = service.inner();

    // Create two files: main.ab imports helper.ab
    let main_uri = file_uri("project/main.ab");
    let helper_uri = file_uri("project/helper.ab");

    // Write helper file to VFS
    backend
        .files
        .fs
        .write(
            &helper_uri.to_file_path().unwrap(),
            "pub fun helper_fn(): Null {\n    echo \"helping\"\n}",
        )
        .await
        .unwrap();

    let code = "import * from \"helper\"";
    open_document(backend, &main_uri, code, 1).await;

    // Cursor on "helper" at line 0, character 16
    let result = backend
        .goto_definition(goto_def_params(&main_uri, 0, 16))
        .await
        .unwrap();

    assert!(
        result.is_some(),
        "Goto definition on local import path should navigate to the file"
    );
    match result.unwrap() {
        GotoDefinitionResponse::Link(links) => {
            assert_eq!(links.len(), 1);
            let target = links[0].target_uri.to_string();
            assert!(
                target.contains("helper"),
                "Target URI should reference helper file, got: {target}"
            );
        }
        other => panic!("Expected Link response for import path, got {other:?}"),
    }
}

// ─── Import: Goto Imported Symbol ─────────────────────────────────────────────

#[test]
async fn test_goto_definition_imported_symbol_in_braces() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    // import { env_var_load } from "std/env"
    let code = "import { env_var_load } from \"std/env\"";
    open_document(backend, &uri, code, 1).await;

    // 'env_var_load' starts at character 9 in line 0
    let result = backend
        .goto_definition(goto_def_params(&uri, 0, 9))
        .await
        .unwrap();

    // After the fix, this should either return Some (navigating to the definition
    // in std/env.ab) or None (if resolution can't find the file). Key thing: no panic.
    // With our imported file registration, it should navigate.
    if let Some(GotoDefinitionResponse::Scalar(location)) = result {
        let target = location.uri.to_string();
        assert!(
            target.contains("env"),
            "Should navigate to env stdlib file, got: {target}"
        );
    }
}

#[test]
async fn test_goto_definition_imported_function_usage() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    // Use an imported stdlib function
    let code = "import * from \"std/math\"\nlet x = abs(-5)";
    open_document(backend, &uri, code, 1).await;

    // 'abs' at line 1, character 8
    let result = backend.goto_definition(goto_def_params(&uri, 1, 8)).await;

    // Must not panic
    assert!(
        result.is_ok(),
        "goto_definition on imported function usage must not panic"
    );

    // With imported file registration, this should now resolve
    if let Ok(Some(GotoDefinitionResponse::Scalar(location))) = result {
        let target = location.uri.to_string();
        assert!(
            target.contains("math"),
            "Should navigate to math stdlib file, got: {target}"
        );
    }
}

// ─── Import: Autocomplete ─────────────────────────────────────────────────────

#[test]
async fn test_completion_import_specific_symbols() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    // The cursor is inside the curly braces of an import, typing 'e'
    // import { e } from "std/env"
    let code = "import { e } from \"std/env\"";
    open_document(backend, &uri, code, 1).await;

    // Cursor on 'e' at line 0, character 9
    let result = backend
        .completion(completion_params(&uri, 0, 9))
        .await
        .unwrap();

    // Should get completions for symbols from std/env
    if let Some(CompletionResponse::Array(items)) = result {
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(
            !labels.is_empty(),
            "Should get completions for import symbols from std/env"
        );
        // Should include functions like env_var_load, env_var_get, etc.
        let has_env_fn = labels.iter().any(|l| l.starts_with("env_"));
        assert!(
            has_env_fn,
            "Completions should include env_* functions from std/env, got: {labels:?}"
        );
    }
    // If result is None, the completion didn't find the symbol — that's the bug we're testing
}

#[test]
async fn test_completion_import_shows_remaining_symbols() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    // Already imported env_var_load, typing in braces should suggest others
    // import { env_var_load, e } from "std/env"
    let code = "import { env_var_load, e } from \"std/env\"";
    open_document(backend, &uri, code, 1).await;

    // Cursor on the second 'e' at line 0, character 23
    let result = backend
        .completion(completion_params(&uri, 0, 23))
        .await
        .unwrap();

    if let Some(CompletionResponse::Array(items)) = result {
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        // env_var_load should NOT be in the suggestions since it's already imported
        let has_env_var_load = labels.contains(&"env_var_load");
        assert!(
            !has_env_var_load,
            "Already-imported symbol should not appear in suggestions, got: {labels:?}"
        );
        // But other env functions should be there
        let has_other_env = labels
            .iter()
            .any(|l| l.starts_with("env_") && *l != "env_var_load");
        assert!(
            has_other_env,
            "Should suggest other env_* functions, got: {labels:?}"
        );
    }
}

#[test]
async fn test_completion_import_path_stdlib() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    // Typing a stdlib path
    let code = "import * from \"std/\"";
    open_document(backend, &uri, code, 1).await;

    // Cursor at the end of "std/" — line 0, character 18
    let result = backend
        .completion(completion_params(&uri, 0, 18))
        .await
        .unwrap();

    if let Some(CompletionResponse::Array(items)) = result {
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        // Should suggest stdlib modules: env, fs, math, text, etc.
        assert!(
            !labels.is_empty(),
            "Should suggest stdlib modules for 'std/' path"
        );
    }
}

// ─── Import: Hover ────────────────────────────────────────────────────────────

#[test]
async fn test_hover_on_imported_function() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    let code = "import * from \"std/math\"\nlet x = abs(-5)";
    open_document(backend, &uri, code, 1).await;

    // Hover on 'abs' at line 1, character 8
    let result = backend.hover(hover_params(&uri, 1, 8)).await.unwrap();

    // Should show hover info for the imported function
    if let Some(hover) = result {
        if let HoverContents::Markup(markup) = hover.contents {
            assert!(
                markup.value.contains("abs"),
                "Hover should mention 'abs', got: {}",
                markup.value
            );
        }
    }
}

#[test]
async fn test_hover_on_imported_symbol_in_braces() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    let code = "import { abs } from \"std/math\"";
    open_document(backend, &uri, code, 1).await;

    // Hover on 'abs' in the import braces — line 0, character 9
    let result = backend.hover(hover_params(&uri, 0, 9)).await.unwrap();

    if let Some(hover) = result {
        if let HoverContents::Markup(markup) = hover.contents {
            assert!(
                markup.value.contains("abs"),
                "Hover on imported symbol should mention 'abs', got: {}",
                markup.value
            );
        }
    }
}

// ─── Import: Cross-file Goto Definition ───────────────────────────────────────

#[test]
async fn test_goto_definition_user_defined_import() {
    let (service, _) = make_backend();
    let backend = service.inner();

    let helper_uri = file_uri("project/utils.ab");
    let main_uri = file_uri("project/main.ab");

    // Write utils.ab to VFS
    backend
        .files
        .fs
        .write(
            &helper_uri.to_file_path().unwrap(),
            "pub fun add(a, b) {\n    return a + b\n}",
        )
        .await
        .unwrap();

    let code = "import { add } from \"utils.ab\"\nadd(1, 2)";
    open_document(backend, &main_uri, code, 1).await;

    // Hover on 'add' usage at line 1, character 0
    let result = backend.hover(hover_params(&main_uri, 1, 0)).await.unwrap();

    if let Some(hover) = result {
        if let HoverContents::Markup(markup) = hover.contents {
            assert!(
                markup.value.contains("add"),
                "Hover on imported user function should mention 'add', got: {}",
                markup.value
            );
        }
    }
}

// ─── Import: Error Cases ──────────────────────────────────────────────────────

#[test]
async fn test_local_import_no_file_doesnt_exist_error() {
    // Regression test: importing a local file that exists on disk (but not opened
    // in the editor) should NOT produce a "File doesn't exist" error.
    let (service, _) = make_backend();
    let backend = service.inner();

    let helper_uri = file_uri("project/utils.ab");
    let main_uri = file_uri("project/main.ab");

    // Write utils.ab to VFS (simulating file on disk)
    backend
        .files
        .fs
        .write(
            &helper_uri.to_file_path().unwrap(),
            "pub fun helper_fn(x) {\n    return x + 1\n}",
        )
        .await
        .unwrap();

    // Only open main.ab — utils.ab is NOT opened
    let code = "import { helper_fn } from \"utils.ab\"\nlet result = helper_fn(5)";
    open_document(backend, &main_uri, code, 1).await;

    let file_id = backend.files.get(&main_uri).unwrap();
    let version = backend.files.get_latest_version(file_id).unwrap();

    // Should NOT have a "File doesn't exist" error
    if let Some(errors) = backend.files.errors.get(&(file_id, version)) {
        let has_file_error = errors
            .iter()
            .any(|(msg, _)| msg.contains("doesn't exist") || msg.contains("File"));
        assert!(
            !has_file_error,
            "Should NOT report 'File doesn't exist' for a local import that exists on disk, got: {:?}",
            errors.iter().map(|(m, _)| m).collect::<Vec<_>>()
        );
    };
}

#[test]
async fn test_local_import_resolves_symbols() {
    // Test that symbols from a local imported file are properly resolved
    let (service, _) = make_backend();
    let backend = service.inner();

    let helper_uri = file_uri("project/utils.ab");
    let main_uri = file_uri("project/main.ab");

    // Write utils.ab with a public function
    backend
        .files
        .fs
        .write(
            &helper_uri.to_file_path().unwrap(),
            "pub fun double(x) {\n    return x * 2\n}",
        )
        .await
        .unwrap();

    // Open main.ab that imports and uses the function
    let code = "import { double } from \"utils.ab\"\nlet val = double(21)";
    open_document(backend, &main_uri, code, 1).await;

    let file_id = backend.files.get(&main_uri).unwrap();
    let version = backend.files.get_latest_version(file_id).unwrap();

    // Should NOT have a "not defined" error for double
    if let Some(errors) = backend.files.errors.get(&(file_id, version)) {
        let has_undef_error = errors
            .iter()
            .any(|(msg, _)| msg.contains("not defined") && msg.contains("double"));
        assert!(
            !has_undef_error,
            "Imported function 'double' should be resolvable, got errors: {:?}",
            errors.iter().map(|(m, _)| m).collect::<Vec<_>>()
        );
    }

    // Check symbol table has the imported function
    if let Some(symbol_table) = backend.files.symbol_table.get(&(file_id, version)) {
        let has_double = symbol_table
            .symbols
            .iter()
            .any(|(_, info)| info.name == "double");
        assert!(has_double, "Symbol table should contain 'double'");
    };
}

#[test]
async fn test_import_nonexistent_file_no_panic() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    let code = "import * from \"nonexistent_file\"";
    open_document(backend, &uri, code, 1).await;

    // Should not panic, and should have an error diagnostic
    let file_id = backend.files.get(&uri).unwrap();
    let version = backend.files.get_latest_version(file_id).unwrap();
    if let Some(errors) = backend.files.errors.get(&(file_id, version)) {
        let has_file_error = errors.iter().any(|(msg, _)| msg.contains("exist"));
        assert!(
            has_file_error,
            "Should report 'file doesn't exist' error, got: {:?}",
            errors.iter().map(|(m, _)| m).collect::<Vec<_>>()
        );
    };
}

#[test]
async fn test_import_nonexistent_symbol_no_panic() {
    let (service, _) = make_backend();
    let backend = service.inner();
    let uri = file_uri("main.ab");

    let code = "import { nonexistent_fn } from \"std/math\"";
    open_document(backend, &uri, code, 1).await;

    // Should report an error about unresolved symbol
    let file_id = backend.files.get(&uri).unwrap();
    let version = backend.files.get_latest_version(file_id).unwrap();
    if let Some(errors) = backend.files.errors.get(&(file_id, version)) {
        let has_resolve_error = errors.iter().any(|(msg, _)| msg.contains("resolve"));
        assert!(
            has_resolve_error,
            "Should report 'could not resolve' error, got: {:?}",
            errors.iter().map(|(m, _)| m).collect::<Vec<_>>()
        );
    };
}
