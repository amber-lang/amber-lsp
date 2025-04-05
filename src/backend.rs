use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use chumsky::container::Seq;
use ropey::Rope;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use tracing::info;

use crate::analysis::{
    self, get_symbol_definition_info, Context, FunctionSymbol, SymbolInfo, SymbolTable, SymbolType,
};
use crate::files::{FileVersion, Files, DEFAULT_VERSION};
use crate::fs::{LocalFs, FS};
use crate::grammar::{self, Grammar, LSPAnalysis, ParserResponse};
use crate::paths::FileId;
use crate::stdlib::find_in_stdlib;

#[derive(Clone, Debug, PartialEq)]
pub enum AmberVersion {
    Alpha034,
    Alpha035,
    Alpha040,
}

#[derive(Debug)]
pub struct Backend {
    pub client: Client,
    pub files: Files,
    /// The LSP analysis implementation.
    pub lsp_analysis: Box<dyn LSPAnalysis>,
    pub token_types: Box<[SemanticTokenType]>,
    pub amber_version: AmberVersion,
}

impl Backend {
    pub fn new(client: Client, amber_version: AmberVersion, fs: Option<Arc<dyn FS>>) -> Self {
        let fs = if let Some(fs) = fs {
            fs
        } else {
            Arc::new(LocalFs::new())
        };

        let files = Files::new(fs);

        files.generic_types.reset_counter();

        Self {
            client,
            files,
            lsp_analysis: match amber_version {
                AmberVersion::Alpha034 => Box::new(grammar::alpha034::AmberCompiler::new()),
                AmberVersion::Alpha035 => Box::new(grammar::alpha035::AmberCompiler::new()),
                AmberVersion::Alpha040 => Box::new(grammar::alpha035::AmberCompiler::new()),
            },
            token_types: match amber_version {
                AmberVersion::Alpha034 => Box::new(grammar::alpha034::semantic_tokens::LEGEND_TYPE),
                AmberVersion::Alpha035 => Box::new(grammar::alpha035::semantic_tokens::LEGEND_TYPE),
                AmberVersion::Alpha040 => Box::new(grammar::alpha035::semantic_tokens::LEGEND_TYPE),
            },
            amber_version,
        }
    }

    #[tracing::instrument(skip_all)]
    pub fn open_document<'a>(
        &'a self,
        uri: &'a Url,
    ) -> Pin<Box<dyn Future<Output = Result<(FileId, FileVersion)>> + Send + 'a>> {
        Box::pin(async move {
            if let Some(file_id) = self.files.get(uri) {
                let version = self.files.get_latest_version(file_id);
                return Ok((file_id, version));
            }

            let text = match self
                .files
                .fs
                .read(&uri.to_file_path().unwrap().to_string_lossy())
                .await
            {
                Ok(text) => Rope::from_str(&text),
                Err(_) => {
                    return Err(Error::internal_error());
                }
            };

            let file_id = self.files.insert(uri.clone(), DEFAULT_VERSION);

            self.files
                .document_map
                .insert((file_id, DEFAULT_VERSION), text);

            self.analize_document(file_id).await;

            Ok((file_id, DEFAULT_VERSION))
        })
    }

    #[tracing::instrument(skip_all)]
    pub async fn publish_diagnostics(
        &self,
        file_id: &FileId,
        diagnostics: Vec<Diagnostic>,
        version: Option<FileVersion>,
    ) {
        let uri = self.files.lookup(file_id);
        self.client
            .publish_diagnostics(uri, diagnostics, version.map(|v| v.into()))
            .await;
    }

    pub async fn publish_syntax_errors(&self, file_id: FileId, file_version: FileVersion) {
        let errors = match self.files.errors.get(&(file_id, file_version)) {
            Some(errors) => errors.clone(),
            None => return,
        };

        let (rope, version) = match self.files.get_document_latest_version(file_id) {
            Some(document) => document,
            None => return,
        };

        let diagnostics = errors
            .iter()
            .filter_map(|(msg, span)| {
                || -> Option<Diagnostic> {
                    let start_position = self.offset_to_position(span.start, &rope)?;
                    let end_position = self.offset_to_position(span.end, &rope)?;

                    Some(Diagnostic::new_simple(
                        Range::new(start_position, end_position),
                        msg.to_string(),
                    ))
                }()
            })
            .collect::<Vec<_>>();

        self.publish_diagnostics(&file_id, diagnostics, Some(version))
            .await;
    }

    #[tracing::instrument(skip_all)]
    pub async fn analize_document(&self, file_id: FileId) {
        let (rope, version) = match self.files.get_document_latest_version(file_id) {
            Some(document) => document,
            None => return,
        };

        let lock = Arc::new(RwLock::new(false));

        let mut lock_w = lock.write().await;

        self.files
            .analyze_lock
            .insert((file_id, version), lock.clone());

        let tokens = self.lsp_analysis.tokenize(&rope.to_string());

        let ParserResponse {
            ast,
            errors,
            semantic_tokens,
        } = self.lsp_analysis.parse(&tokens);

        self.files.errors.insert(
            (file_id, version),
            errors
                .iter()
                .map(|err| (err.to_string(), *err.span()))
                .collect(),
        );
        self.files.ast_map.insert((file_id, version), ast.clone());
        self.files
            .semantic_token_map
            .insert((file_id, version), semantic_tokens);

        self.publish_syntax_errors(file_id, version).await;

        self.files
            .symbol_table
            .insert((file_id, version), SymbolTable::default());

        match ast {
            Grammar::Alpha034(Some(ast)) => {
                analysis::alpha034::global::analyze_global_stmnt(file_id, version, &ast, self)
                    .await;
            }
            Grammar::Alpha035(Some(ast)) => {
                analysis::alpha035::global::analyze_global_stmnt(file_id, version, &ast, self)
                    .await;
            }
            _ => {}
        }

        *lock_w = true;
        drop(lock_w);

        Box::pin(async {
            self.analyze_dependencies(file_id).await;
        })
        .await;
    }

    pub fn offset_to_position(&self, offset: usize, rope: &Rope) -> Option<Position> {
        let line = rope
            .try_char_to_line(offset)
            .ok()
            .unwrap_or(rope.len_lines());
        let first_char_of_line = rope.try_line_to_char(line).ok().unwrap_or(rope.len_chars());
        let column = offset - first_char_of_line;
        Some(Position::new(line as u32, column as u32))
    }

    async fn analyze_dependencies(&self, file_id: FileId) {
        let deps = self.files.get_files_dependant_on(file_id);

        for (dep_file_id, dep_file_version) in deps {
            if dep_file_id == file_id {
                continue;
            }

            self.analize_document(dep_file_id).await;
            self.publish_syntax_errors(dep_file_id, dep_file_version)
                .await;
        }
    }

    async fn get_symbol_at_position(
        &self,
        file_id: FileId,
        position: Position,
    ) -> Option<(SymbolInfo, usize)> {
        let (rope, version) = match self.files.get_document_latest_version(file_id) {
            Some(document) => document,
            None => return None,
        };

        let file = (file_id, version);

        if !self.files.is_file_analyzed(&file).await {
            return None;
        }

        let char = rope
            .try_line_to_char(position.line as usize)
            .ok()
            .unwrap_or(rope.len_chars());
        let offset = char + position.character as usize;

        let symbol_table = match self.files.symbol_table.get(&file) {
            Some(symbol_table) => symbol_table.clone(),
            None => return None,
        };

        let symbol_info = match symbol_table.symbols.get(&offset) {
            Some(symbol) => symbol.clone(),
            None => return None,
        };

        Some((symbol_info, offset))
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("amber".to_string()),
                                        scheme: Some("file".to_string()),
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: self.token_types.to_vec(),
                                    token_modifiers: vec![].into(),
                                },
                                range: Some(true),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![":".to_string(), ".".to_string()]),
                    all_commit_characters: None,
                    completion_item: Some(CompletionOptionsCompletionItem {
                        label_details_support: Some(true),
                    }),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]), // Trigger on '(' and ','
                    retrigger_characters: Some(vec![",".to_string()]), // Retrigger on ','
                    ..Default::default()
                }),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        let options = serde_json::to_value(DidChangeWatchedFilesRegistrationOptions {
            watchers: vec![FileSystemWatcher {
                glob_pattern: GlobPattern::String("**/*.ab".to_string()),
                kind: None, // Default is 7 - Create | Change | Delete
            }],
        })
        .unwrap();

        let _ = self
            .client
            .register_capability(vec![Registration {
                id: "did_change_watched_files".to_string(),
                method: "workspace/didChangeWatchedFiles".to_string(),
                register_options: Some(options),
            }])
            .await;

        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    #[tracing::instrument(skip_all)]
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let version = FileVersion(params.text_document.version);

        if let Some(file_id) = self.files.get(&params.text_document.uri) {
            self.files.change_latest_file_version(file_id, version);
            self.publish_syntax_errors(file_id, version).await;
            return;
        }

        let version = FileVersion(params.text_document.version);

        let file_id = self.files.insert(params.text_document.uri, version);

        self.files.document_map.insert(
            (file_id, version),
            Rope::from_str(&params.text_document.text),
        );

        self.analize_document(file_id).await;

        self.publish_syntax_errors(file_id, version).await;
    }

    #[tracing::instrument(skip_all)]
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        let new_version = FileVersion(params.text_document.version);

        let file_id = match self.files.get(&params.text_document.uri) {
            Some(file_id) => file_id,
            None => {
                return self
                    .client
                    .log_message(MessageType::ERROR, format!("document {uri} is not open"))
                    .await;
            }
        };

        let version = self.files.get_latest_version(file_id);

        if !self.files.document_map.contains_key(&(file_id, version)) {
            return self
                .client
                .log_message(MessageType::ERROR, format!("document {uri} is not open"))
                .await;
        }

        if params
            .content_changes
            .iter()
            .any(|text| text.range_length.is_some())
        {
            return self
                .client
                .log_message(MessageType::ERROR, "range length changes are not supported")
                .await;
        }

        if let Some(change) = params
            .content_changes
            .iter()
            .find(|change| change.range.is_none() && change.range_length.is_none())
        {
            self.files
                .document_map
                .insert((file_id, new_version), Rope::from_str(&change.text));
        } else {
            let mut document = self
                .files
                .document_map
                .get(&(file_id, version))
                .unwrap()
                .clone();

            params
                .content_changes
                .iter()
                .filter(|change| change.range.is_some())
                .for_each(|change| {
                    let range = change.range.as_ref().unwrap();
                    let start = document.line_to_char(range.start.line as usize)
                        + range.start.character as usize;
                    let end = document.line_to_char(range.end.line as usize)
                        + range.end.character as usize;

                    document.remove(start..end);
                    document.insert(start, &change.text);
                });

            self.files
                .document_map
                .insert((file_id, new_version), document);
        }

        self.files.add_new_file_version(file_id, new_version);

        self.analize_document(file_id).await;

        self.publish_syntax_errors(file_id, new_version).await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        // TODO: Invalidate the file and re-analyze dependencies
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "document closed!")
            .await;
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }

    #[tracing::instrument(skip_all)]
    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let file_id = match self.files.get(&params.text_document.uri) {
            Some(file_id) => file_id,
            None => {
                return Ok(None);
            }
        };

        let (rope, file_version) = match self.files.get_document_latest_version(file_id) {
            Some(document) => document,
            None => return Ok(None),
        };

        if !self.files.is_file_analyzed(&(file_id, file_version)).await {
            return Ok(None);
        }

        let semantic_tokens = match self.files.semantic_token_map.get(&(file_id, file_version)) {
            Some(tokens) => tokens,
            None => {
                return Ok(None);
            }
        };

        let mut pre_line = 0;
        let mut pre_start = 0;

        let data = semantic_tokens
            .iter()
            .filter_map(|(token, span)| {
                if span.start > span.end {
                    return None;
                }

                let length = span.end - span.start;
                // Get the line number of the token
                let line = rope.try_byte_to_line(span.start).ok()? as u32;
                // Get the first character of the line
                let first = rope.try_line_to_char(line as usize).ok()? as u32;
                // Get the start position of the token relative to the line
                let start = rope.try_byte_to_char(span.start).ok()? as u32 - first;

                // Calculate the delta line and delta start
                let delta_line = line - pre_line;

                // If the token is on the same line as the previous token
                // calculate the delta start relative to the previous token
                // otherwise calculate the delta start relative to the first character of the line
                let delta_start = if delta_line == 0 {
                    start - pre_start
                } else {
                    start
                };

                let ret = Some(SemanticToken {
                    delta_line,
                    delta_start,
                    length: length as u32,
                    token_type: *token as u32,
                    token_modifiers_bitset: 0,
                });
                pre_line = line;
                pre_start = start;
                ret
            })
            .collect();

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        })))
    }

    #[tracing::instrument(skip_all)]
    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let file_id = match self.files.get(&params.text_document.uri) {
            Some(file_id) => file_id,
            None => {
                return Ok(None);
            }
        };

        let (rope, file_version) = match self.files.get_document_latest_version(file_id) {
            Some(document) => document,
            None => return Ok(None),
        };

        if !self.files.is_file_analyzed(&(file_id, file_version)).await {
            return Ok(None);
        }

        let requested_range = params.range;

        let semantic_tokens = match self.files.semantic_token_map.get(&(file_id, file_version)) {
            Some(tokens) => tokens,
            None => {
                return Ok(None);
            }
        };

        let mut pre_line = 0;
        let mut pre_start = 0;

        let data = semantic_tokens
            .iter()
            .filter_map(|(token, span)| {
                let line = rope.try_byte_to_line(span.start).ok()? as u32;
                let first = rope.try_line_to_char(line as usize).ok()? as u32;
                let start = rope.try_byte_to_char(span.start).ok()? as u32 - first;

                if !(line >= requested_range.start.line
                    && (line < requested_range.end.line
                        || (line == requested_range.end.line
                            && start <= requested_range.end.character)))
                {
                    return None;
                }

                if span.start > span.end {
                    return None;
                }

                let length = span.end - span.start;
                let delta_line = line - pre_line;
                let delta_start = if delta_line == 0 {
                    start - pre_start
                } else {
                    start
                };

                let ret = Some(SemanticToken {
                    delta_line,
                    delta_start,
                    length: length as u32,
                    token_type: *token as u32,
                    token_modifiers_bitset: 0,
                });
                pre_line = line;
                pre_start = start;
                ret
            })
            .collect();

        Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        })))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let definition = {
            let uri = params.text_document_position_params.text_document.uri;
            let file_id = match self.files.get(&uri) {
                Some(file_id) => file_id,
                None => return Ok(None),
            };

            let (rope, version) = match self.files.get_document_latest_version(file_id) {
                Some(document) => document,
                None => return Ok(None),
            };

            if !self.files.is_file_analyzed(&(file_id, version)).await {
                return Ok(None);
            }

            let position = params.text_document_position_params.position;
            let char = rope
                .try_line_to_char(position.line as usize)
                .ok()
                .unwrap_or(rope.len_chars());
            let offset = char + position.character as usize;

            let symbol_table = match self.files.symbol_table.get(&(file_id, version)) {
                Some(symbol_table) => symbol_table.clone(),
                None => return Ok(None),
            };

            let symbol_info = match symbol_table.symbols.get(&offset) {
                Some(symbol) => symbol.clone(),
                None => return Ok(None),
            };

            if symbol_info.symbol_type != SymbolType::ImportPath
                && (symbol_info.undefined || symbol_info.is_definition)
            {
                return Ok(None);
            }

            let response = match symbol_table.definitions.get(&symbol_info.name) {
                Some(definitions) => match definitions.get(&offset) {
                    Some(definition) => {
                        let definition_file_rope =
                            match self.files.get_document_latest_version(definition.file.0) {
                                Some((document, _)) => document.clone(),
                                None => {
                                    return Ok(None);
                                }
                            };

                        let start_position = self
                            .offset_to_position(definition.start, &definition_file_rope)
                            .unwrap();
                        let end_position = self
                            .offset_to_position(definition.end, &definition_file_rope)
                            .unwrap();

                        let file_url = self.files.lookup(&definition.file.0);

                        match symbol_info.symbol_type {
                            SymbolType::ImportPath => {
                                let selection_range = Range {
                                    start: self
                                        .offset_to_position(symbol_info.span.start, &rope)
                                        .unwrap(),
                                    end: self
                                        .offset_to_position(symbol_info.span.end, &rope)
                                        .unwrap(),
                                };

                                Some(GotoDefinitionResponse::Link(vec![LocationLink {
                                    origin_selection_range: Some(selection_range),
                                    target_uri: file_url,
                                    target_range: Range {
                                        start: Position {
                                            line: 0,
                                            character: 0,
                                        },
                                        end: Position {
                                            line: 0,
                                            character: 0,
                                        },
                                    },
                                    target_selection_range: Range {
                                        start: Position {
                                            line: 0,
                                            character: 0,
                                        },
                                        end: Position {
                                            line: 0,
                                            character: 0,
                                        },
                                    },
                                }]))
                            }
                            _ => Some(GotoDefinitionResponse::Scalar(Location::new(
                                file_url,
                                Range {
                                    start: start_position,
                                    end: end_position,
                                },
                            ))),
                        }
                    }
                    None => None,
                },
                None => None,
            };

            response
        };

        Ok(definition)
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::INFO, "configuration changed!")
            .await;
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::INFO, "workspace folders changed!")
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let file_id = match self
            .files
            .get(&params.text_document_position_params.text_document.uri)
        {
            Some(file_id) => file_id,
            None => {
                return Ok(None);
            }
        };

        let position = params.text_document_position_params.position;

        let symbol_info = match self.get_symbol_at_position(file_id, position).await {
            Some((symbol_info, _)) if !symbol_info.undefined => symbol_info,
            _ => {
                return Ok(None);
            }
        };

        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!(
                    "{}```amber\n{}\n```",
                    match symbol_info.symbol_type {
                        SymbolType::Function(FunctionSymbol { ref docs, .. }) if docs.is_some() =>
                            format!("{}\n", docs.clone().unwrap()),
                        _ => "".to_string(),
                    },
                    symbol_info.to_string(&self.files.generic_types)
                ),
            }),
            range: Some(Range {
                start: Position {
                    line: position.line,
                    character: position.character,
                },
                end: Position {
                    line: position.line,
                    character: position.character,
                },
            }),
        }))
    }

    #[tracing::instrument(skip_all)]
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;

        let file_id = match self.files.get(&uri) {
            Some(file_id) => file_id,
            None => {
                return Ok(None);
            }
        };

        let position = params.text_document_position.position;

        let symbol_info = match self.get_symbol_at_position(file_id, position).await {
            Some((symbol_info, _)) => symbol_info,
            None => {
                return Ok(None);
            }
        };

        let version = self.files.get_latest_version(file_id);

        let symbol_table = self
            .files
            .symbol_table
            .get(&(file_id, version))
            .unwrap()
            .clone();

        let completions = match symbol_info.symbol_type {
            SymbolType::ImportPath => {
                let stdlib_paths = find_in_stdlib(self, &symbol_info.name).await;

                if stdlib_paths.contains(&symbol_info.name) {
                    return Ok(None);
                }

                let mut completions: Vec<CompletionItem> = stdlib_paths
                    .iter()
                    .map(|file| CompletionItem {
                        label: file.clone(),
                        kind: Some(CompletionItemKind::MODULE),
                        ..CompletionItem::default()
                    })
                    .collect();

                let file_path = uri.to_file_path().unwrap().canonicalize().unwrap();
                let mut searched_path = file_path.parent().unwrap().to_path_buf();
                searched_path.push(symbol_info.name.clone());

                if let Ok(path) = searched_path.canonicalize() {
                    if path.is_file() {
                        return Ok(None);
                    }
                }

                let dir_to_search = if symbol_info.name.ends_with("/") || searched_path.is_dir() {
                    searched_path.as_path()
                } else {
                    searched_path.parent().unwrap()
                };

                for entry_path in self
                    .files
                    .fs
                    .read_dir(dir_to_search.to_str().unwrap())
                    .await
                {
                    let entry_name = entry_path
                        .file_name()
                        .unwrap()
                        .to_string_lossy()
                        .to_string();

                    let entry_kind = if entry_path.is_symlink() {
                        let target = entry_path.read_link();

                        match target {
                            Ok(target) if target.is_dir() => CompletionItemKind::FOLDER,
                            _ => CompletionItemKind::FILE,
                        }
                    } else {
                        if entry_path.is_dir() {
                            CompletionItemKind::FOLDER
                        } else {
                            CompletionItemKind::FILE
                        }
                    };

                    let absolute_entry_path = entry_path.canonicalize().unwrap();

                    if absolute_entry_path != file_path
                        && (entry_path.is_dir()
                            || entry_path.extension().map(|ext| ext.to_str().unwrap())
                                == Some("ab"))
                    {
                        completions.push(CompletionItem {
                            label: entry_name.clone(),
                            kind: Some(entry_kind),
                            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                                range: Range {
                                    start: Position {
                                        line: position.line,
                                        character: position.character
                                            - symbol_info.name.split("/").last().unwrap_or("").len()
                                                as u32, // Move back by prefix length
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
            SymbolType::Variable | SymbolType::Function(_) => {
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

                            return get_symbol_definition_info(
                                &self.files,
                                name,
                                &location.file,
                                usize::MAX,
                            );
                        })
                        .collect::<Vec<SymbolInfo>>(),
                    _ => symbol_table
                        .definitions
                        .iter()
                        .filter_map(|(name, _)| {
                            get_symbol_definition_info(
                                &self.files,
                                name,
                                &(file_id, version),
                                symbol_info.span.start,
                            )
                        })
                        .collect::<Vec<SymbolInfo>>(),
                };

                for symbol_info in definitions.iter() {
                    match symbol_info.symbol_type {
                        SymbolType::Function(FunctionSymbol { ref arguments, .. }) => {
                            completions.push(CompletionItem {
                                label: symbol_info.name.clone(),
                                insert_text: if import_context.is_some() {
                                    Some(symbol_info.name.clone())
                                } else {
                                    Some(format!(
                                        "{}({})",
                                        symbol_info.name,
                                        arguments
                                            .iter()
                                            .enumerate()
                                            .map(|(idx, (arg, _))| format!(
                                                "${{{}:{}}}",
                                                idx + 1,
                                                arg.name
                                            ))
                                            .collect::<Vec<String>>()
                                            .join(", ")
                                    ))
                                },
                                kind: Some(CompletionItemKind::FUNCTION),
                                detail: Some(symbol_info.to_string(&self.files.generic_types)),
                                insert_text_format: Some(InsertTextFormat::SNIPPET),
                                command: Some(Command {
                                    title: "triggerParameterHints".to_string(),
                                    command: "editor.action.triggerParameterHints".to_string(),
                                    arguments: None,
                                }),
                                ..CompletionItem::default()
                            });
                        }
                        SymbolType::Variable => {
                            completions.push(CompletionItem {
                                label: symbol_info.name.clone(),
                                kind: Some(CompletionItemKind::VARIABLE),
                                label_details: Some(CompletionItemLabelDetails {
                                    description: Some(
                                        symbol_info.data_type.to_string(&self.files.generic_types),
                                    ),
                                    detail: None,
                                }),
                                ..CompletionItem::default()
                            });
                        }
                        _ => continue,
                    };
                }

                completions
            }
        };

        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let file_id = match self
            .files
            .get(&params.text_document_position_params.text_document.uri)
        {
            Some(file_id) => file_id,
            None => {
                return Ok(None);
            }
        };

        let position = params.text_document_position_params.position;

        let (symbol_info, offset) = match self.get_symbol_at_position(file_id, position).await {
            Some((symbol_info, offset)) if !symbol_info.undefined => (symbol_info, offset),
            _ => {
                return Ok(None);
            }
        };

        match symbol_info.symbol_type {
            SymbolType::Function(FunctionSymbol { ref arguments, .. }) => {
                let mut active_parameter = 0 as u32;

                info!(
                    "Signature help for function {} at offset {}",
                    symbol_info.name, offset
                );
                info!("Arguments: {:?}", arguments);

                arguments.iter().enumerate().for_each(|(idx, (_, span))| {
                    let start = span.start;
                    let end = span.end;

                    if offset >= start && offset <= end {
                        active_parameter = idx as u32;
                    }
                });

                Ok(Some(SignatureHelp {
                    signatures: vec![SignatureInformation {
                        label: symbol_info.to_string(&self.files.generic_types),
                        documentation: None,
                        parameters: Some(
                            arguments
                                .iter()
                                .map(|(arg, _)| ParameterInformation {
                                    label: ParameterLabel::Simple(format!(
                                        "{}: {}",
                                        arg.name,
                                        arg.data_type.to_string(&self.files.generic_types)
                                    )),
                                    documentation: None,
                                })
                                .collect::<Vec<ParameterInformation>>(),
                        ),
                        active_parameter: Some(active_parameter),
                    }],
                    active_signature: Some(0),
                    active_parameter: Some(active_parameter),
                }))
            }
            _ => Ok(None),
        }
    }
}
