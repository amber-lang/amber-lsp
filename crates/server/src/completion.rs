use std::collections::HashSet;

use ropey::Rope;
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::lsp_types::*;
use tower_lsp_server::UriExt;

use amber_analysis::files::FileVersion;
use amber_analysis::stdlib::find_in_stdlib;
use amber_analysis::{
    get_symbol_definition_info,
    map_import_path,
    AnalysisHost,
    Context,
    FunctionSymbol,
    SymbolInfo,
    SymbolTable,
    SymbolType,
    VariableSymbol,
};
use amber_grammar::alpha050::{
    GlobalStatement as Alpha050GlobalStatement,
    ImportContent as Alpha050ImportContent,
};
use amber_grammar::Grammar;
use amber_types::paths::FileId;
use amber_types::AmberVersion;

use crate::backend::Backend;

/// Handle `textDocument/completion`.
#[tracing::instrument(skip_all)]
pub async fn handle_completion(
    backend: &Backend,
    params: CompletionParams,
) -> Result<Option<CompletionResponse>> {
    let uri = params.text_document_position.text_document.uri;

    let file_id = match backend.files.get(&uri) {
        Some(file_id) => file_id,
        None => return Ok(None),
    };

    let version = backend.files.get_latest_version(file_id);

    if !backend.files.is_file_analyzed(&(file_id, version)).await {
        return Ok(None);
    }

    let position = params.text_document_position.position;

    let symbol_info = match backend.get_symbol_at_position(file_id, position).await {
        Some((symbol_info, _)) => symbol_info,
        None => return Ok(None),
    };

    let symbol_table = match backend.files.symbol_table.get(&(file_id, version)) {
        Some(symbol_table) => symbol_table.clone(),
        None => return Ok(None),
    };

    let completions = match symbol_info.symbol_type {
        SymbolType::ImportPath => {
            import_path_completions(backend, &uri, &symbol_info, position).await
        }
        SymbolType::Variable(_) | SymbolType::Function(_) => {
            symbol_completions(
                backend,
                &uri,
                file_id,
                version,
                &symbol_info,
                &symbol_table,
                position,
            )
            .await
        }
    };

    Ok(Some(CompletionResponse::Array(completions)))
}

/// Completions for import paths (stdlib + filesystem).
async fn import_path_completions(
    backend: &Backend,
    uri: &Uri,
    symbol_info: &SymbolInfo,
    position: Position,
) -> Vec<CompletionItem> {
    let stdlib_paths = find_in_stdlib(backend, &symbol_info.name).await;

    if stdlib_paths.contains(&symbol_info.name) {
        return vec![];
    }

    let mut completions: Vec<CompletionItem> = stdlib_paths
        .iter()
        .map(|file| CompletionItem {
            label: file.clone(),
            kind: Some(CompletionItemKind::MODULE),
            ..CompletionItem::default()
        })
        .collect();

    let file_path = match uri.to_file_path().and_then(|p| p.canonicalize().ok()) {
        Some(p) => p,
        None => return completions,
    };
    let parent = match file_path.parent() {
        Some(p) => p,
        None => return completions,
    };
    let mut searched_path = parent.to_path_buf();
    searched_path.push(symbol_info.name.clone());

    if let Ok(path) = searched_path.canonicalize() {
        if path.is_file() {
            return vec![];
        }
    }

    let dir_to_search = if symbol_info.name.ends_with("/") || searched_path.is_dir() {
        searched_path.as_path()
    } else {
        match searched_path.parent() {
            Some(p) => p,
            None => return completions,
        }
    };

    for entry_path in backend.files.fs.read_dir(dir_to_search).await {
        let entry_name = match entry_path.file_name() {
            Some(name) => name.to_string_lossy().to_string(),
            None => continue,
        };

        let entry_kind = if entry_path.is_symlink() {
            match entry_path.read_link() {
                Ok(target) if target.is_dir() => CompletionItemKind::FOLDER,
                _ => CompletionItemKind::FILE,
            }
        } else if entry_path.is_dir() {
            CompletionItemKind::FOLDER
        } else {
            CompletionItemKind::FILE
        };

        let absolute_entry_path = match entry_path.canonicalize() {
            Ok(p) => p,
            Err(_) => continue,
        };

        if absolute_entry_path != file_path
            && (entry_path.is_dir()
                || entry_path.extension().and_then(|ext| ext.to_str()) == Some("ab"))
        {
            completions.push(CompletionItem {
                label: entry_name.clone(),
                kind: Some(entry_kind),
                text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                    range: Range {
                        start: Position {
                            line: position.line,
                            character: position.character
                                - symbol_info.name.split("/").last().unwrap_or("").len() as u32,
                        },
                        end: Position {
                            line: position.line,
                            character: position.character,
                        },
                    },
                    new_text: entry_name,
                })),
                ..CompletionItem::default()
            });
        }
    }

    completions
}

/// Completions for variables and functions (local scope + auto-imports).
async fn symbol_completions(
    backend: &Backend,
    uri: &Uri,
    file_id: FileId,
    version: FileVersion,
    symbol_info: &SymbolInfo,
    symbol_table: &SymbolTable,
    _position: Position,
) -> Vec<CompletionItem> {
    let mut completions = vec![];

    let import_context = symbol_info
        .contexts
        .iter()
        .find(|ctx| matches!(ctx, Context::Import(_)));

    let definitions = match import_context {
        Some(Context::Import(import_ctx)) => import_ctx
            .public_definitions
            .iter()
            .filter_map(|(name, location)| {
                if import_ctx.imported_symbols.contains(name) {
                    return None;
                }
                get_symbol_definition_info(&backend.files, name, &location.file, usize::MAX)
            })
            .collect::<Vec<SymbolInfo>>(),
        _ => symbol_table
            .definitions
            .iter()
            .filter_map(|(name, _)| {
                get_symbol_definition_info(
                    &backend.files,
                    name,
                    &(file_id, version),
                    symbol_info.span.start,
                )
            })
            .collect::<Vec<SymbolInfo>>(),
    };

    let in_scope_names: HashSet<String> = definitions.iter().map(|d| d.name.clone()).collect();

    for def in definitions.iter() {
        if let Some(item) =
            symbol_to_completion_item(backend, def, import_context.is_some(), None, None)
        {
            completions.push(item);
        }
    }

    // Keyword completions: only when NOT inside an import context.
    if import_context.is_none() && backend.amber_version == AmberVersion::Alpha050 {
        completions.extend(keyword_completions(&symbol_info.contexts))
    }

    // Auto-import: only for alpha050 and when NOT inside an import context.
    if import_context.is_none() && backend.amber_version == AmberVersion::Alpha050 {
        auto_import_existing(
            backend,
            uri,
            file_id,
            version,
            &in_scope_names,
            &mut completions,
        )
        .await;

        auto_import_stdlib(
            backend,
            uri,
            file_id,
            version,
            &in_scope_names,
            &mut completions,
        )
        .await;
    }

    completions
}

/// Offer symbols from already-imported modules that haven't been imported yet.
async fn auto_import_existing(
    backend: &Backend,
    uri: &Uri,
    file_id: FileId,
    version: FileVersion,
    in_scope_names: &HashSet<String>,
    completions: &mut Vec<CompletionItem>,
) {
    let rope = match backend.files.document_map.get(&(file_id, version)) {
        Some(r) => r.clone(),
        None => Rope::from_str(""),
    };

    let ast = match backend.files.ast_map.get(&(file_id, version)) {
        Some(ast) => ast.clone(),
        None => return,
    };

    let stmts = match ast {
        Grammar::Alpha050(Some(ref stmts)) => stmts,
        _ => return,
    };

    for (global_stmt, _stmt_span) in stmts.iter() {
        if let Alpha050GlobalStatement::Import(
            _is_pub,
            _import_kw,
            (Alpha050ImportContent::ImportSpecific(ref ident_list), ref content_span),
            _from_kw,
            (ref path, _path_span),
        ) = global_stmt
        {
            let imported_uri = map_import_path(uri, path, backend).await;

            let imported_file_id = match backend.files.get(&imported_uri) {
                Some(fid) => fid,
                None => continue,
            };
            let imported_version = backend.files.get_latest_version(imported_file_id);
            let imported_sym_table = match backend
                .files
                .symbol_table
                .get(&(imported_file_id, imported_version))
            {
                Some(st) => st.clone(),
                None => continue,
            };

            let already_imported: HashSet<String> =
                ident_list.iter().map(|(name, _)| name.clone()).collect();

            let (insert_offset, import_prefix) = if ident_list.is_empty() {
                (content_span.start + 1, " ")
            } else {
                let last_ident_span = &ident_list.last().unwrap().1;
                (last_ident_span.end, ", ")
            };
            let insert_position = backend.offset_to_position(insert_offset, &rope);

            for (pub_name, pub_location) in imported_sym_table.public_definitions.iter() {
                if already_imported.contains(pub_name) || in_scope_names.contains(pub_name) {
                    continue;
                }

                let additional_edit = TextEdit {
                    range: Range {
                        start: insert_position,
                        end: insert_position,
                    },
                    new_text: format!("{}{}", import_prefix, pub_name),
                };

                let label_desc = Some(format!("auto import from \"{}\"", path));

                let pub_sym_info = match get_symbol_definition_info(
                    &backend.files,
                    pub_name,
                    &pub_location.file,
                    usize::MAX,
                ) {
                    Some(info) => info,
                    None => continue,
                };

                if let Some(item) = symbol_to_completion_item(
                    backend,
                    &pub_sym_info,
                    false,
                    Some(additional_edit),
                    label_desc,
                ) {
                    completions.push(item);
                }
            }
        }
    }
}

/// Offer symbols from stdlib modules that have no import statement at all.
async fn auto_import_stdlib(
    backend: &Backend,
    uri: &Uri,
    file_id: FileId,
    version: FileVersion,
    in_scope_names: &HashSet<String>,
    completions: &mut Vec<CompletionItem>,
) {
    // Collect all import paths already present in the file.
    let mut already_imported_paths: HashSet<String> = HashSet::new();

    if let Some(ast) = backend.files.ast_map.get(&(file_id, version)) {
        let ast = ast.clone();
        if let Grammar::Alpha050(Some(ref stmts)) = ast {
            for (global_stmt, _) in stmts.iter() {
                match global_stmt {
                    Alpha050GlobalStatement::Import(
                        _,
                        _,
                        (Alpha050ImportContent::ImportSpecific(_), _),
                        _,
                        (ref path, _),
                    )
                    | Alpha050GlobalStatement::Import(
                        _,
                        _,
                        (Alpha050ImportContent::ImportAll, _),
                        _,
                        (ref path, _),
                    ) => {
                        already_imported_paths.insert(path.clone());
                    }
                    _ => {}
                }
            }
        }
    }

    let stdlib_modules = amber_analysis::stdlib::list_stdlib_modules(&backend.amber_version);

    for module_path in stdlib_modules {
        if already_imported_paths.contains(&module_path) {
            continue;
        }

        let module_uri = map_import_path(uri, &module_path, backend).await;

        let module_file = match backend.open_document(&module_uri).await {
            Ok(file) => file,
            Err(_) => continue,
        };

        let module_sym_table = match backend.files.symbol_table.get(&module_file) {
            Some(st) => st.clone(),
            None => continue,
        };

        for (pub_name, pub_location) in module_sym_table.public_definitions.iter() {
            if in_scope_names.contains(pub_name) {
                continue;
            }

            let additional_edit = TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: 0,
                        character: 0,
                    },
                },
                new_text: format!("import {{ {} }} from \"{}\"\n", pub_name, module_path),
            };

            let label_desc = Some(format!("auto import from \"{}\"", module_path));

            let pub_sym_info = match get_symbol_definition_info(
                &backend.files,
                pub_name,
                &pub_location.file,
                usize::MAX,
            ) {
                Some(info) => info,
                None => continue,
            };

            if let Some(item) = symbol_to_completion_item(
                backend,
                &pub_sym_info,
                false,
                Some(additional_edit),
                label_desc,
            ) {
                completions.push(item);
            }
        }
    }
}

/// Build a [`CompletionItem`] from a [`SymbolInfo`].
///
/// Returns `None` for symbol types that don't produce completions.
fn symbol_to_completion_item(
    backend: &Backend,
    symbol_info: &SymbolInfo,
    is_import_context: bool,
    additional_edit: Option<TextEdit>,
    label_description: Option<String>,
) -> Option<CompletionItem> {
    match symbol_info.symbol_type {
        SymbolType::Function(FunctionSymbol { ref arguments, .. }) => Some(CompletionItem {
            label: symbol_info.name.clone(),
            insert_text: if is_import_context {
                Some(symbol_info.name.clone())
            } else {
                Some(format!(
                    "{}({})",
                    symbol_info.name,
                    arguments
                        .iter()
                        .enumerate()
                        .map(|(idx, (arg, _))| format!("${{{}:{}}}", idx + 1, arg.name))
                        .collect::<Vec<String>>()
                        .join(", ")
                ))
            },
            kind: Some(CompletionItemKind::METHOD),
            detail: Some(symbol_info.to_string(&backend.files.generic_types)),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            command: Some(Command {
                title: "triggerParameterHints".to_string(),
                command: "editor.action.triggerParameterHints".to_string(),
                arguments: None,
            }),
            additional_text_edits: additional_edit.map(|e| vec![e]),
            label_details: label_description.map(|desc| CompletionItemLabelDetails {
                description: Some(desc),
                detail: None,
            }),
            ..CompletionItem::default()
        }),
        SymbolType::Variable(VariableSymbol { is_const }) => Some(CompletionItem {
            label: symbol_info.name.clone(),
            kind: Some(if is_const {
                CompletionItemKind::CONSTANT
            } else {
                CompletionItemKind::VARIABLE
            }),
            label_details: Some(CompletionItemLabelDetails {
                description: label_description.or_else(|| {
                    Some(
                        symbol_info
                            .data_type
                            .to_string(&backend.files.generic_types),
                    )
                }),
                detail: None,
            }),
            additional_text_edits: additional_edit.map(|e| vec![e]),
            ..CompletionItem::default()
        }),
        _ => None,
    }
}

/// Build keyword [`CompletionItem`]s appropriate for the given context stack.
fn keyword_completions(contexts: &[Context]) -> Vec<CompletionItem> {
    let in_loop = contexts.iter().any(|c| matches!(c, Context::Loop));
    let in_function = contexts.iter().any(|c| matches!(c, Context::Function(_)));
    let in_main = contexts.contains(&Context::Main);

    // (label, description, optional snippet)
    let mut keywords: Vec<(&str, &str, Option<&str>)> = vec![
        // Statement-level keywords (always available in statement position)
        (
            "if",
            "Conditional branch",
            Some("if ${1:condition} {\n\t$0\n}"),
        ),
        ("loop", "Infinite loop", Some("loop {\n\t$0\n}")),
        ("while", "While loop", Some("while $1 {\n\t$0\n}")),
        (
            "for",
            "Iterator loop",
            Some("for ${1:item} in ${2:iterable} {\n\t$0\n}"),
        ),
        ("let", "Variable declaration", Some("let ${1:name} = $0")),
        (
            "const",
            "Constant declaration",
            Some("const ${1:name} = $0"),
        ),
        ("echo", "Print to stdout", Some("echo $0")),
        ("unsafe", "Suppress failure handling", None),
        ("silent", "Suppress command output", None),
        ("trust", "Trust command result", None),
        ("sudo", "Run with elevated privileges", None),
        ("exit", "Exit the script", Some("exit $0")),
        (
            "failed",
            "Handle command failure",
            Some("failed {\n\t$0\n}"),
        ),
        (
            "succeeded",
            "Handle command success",
            Some("succeeded {\n\t$0\n}"),
        ),
        (
            "exited",
            "Handle command exit with status code",
            Some("exited(${1:code}) {\n\t$0\n}"),
        ),
        // Top-level keywords
        (
            "fun",
            "Function definition",
            Some("fun ${1:name}($2) {\n\t$0\n}"),
        ),
        ("pub", "Public modifier", None),
        ("import", "Import module", Some("import { $2 } from \"$1\"")),
        ("main", "Main block", Some("main {\n\t$0\n}")),
        // Value literals
        ("true", "Boolean true", None),
        ("false", "Boolean false", None),
        ("null", "Null value", None),
    ];

    // Loop-only keywords
    if in_loop {
        keywords.extend([
            ("break", "Exit the current loop", None),
            ("continue", "Skip to the next loop iteration", None),
        ]);
    }

    // Function-only keywords
    if in_function {
        keywords.push((
            "return",
            "Return from the current function",
            Some("return $0"),
        ));
    }

    // Fail: available in function or main context
    if in_function || in_main {
        keywords.push(("fail", "Propagate a failure", Some("fail $0")));
    }

    keywords
        .into_iter()
        .map(|(kw, desc, snippet)| CompletionItem {
            label: kw.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(desc.to_string()),
            insert_text: snippet.map(|s| s.to_string()),
            insert_text_format: snippet.map(|_| InsertTextFormat::SNIPPET),
            ..CompletionItem::default()
        })
        .collect()
}
