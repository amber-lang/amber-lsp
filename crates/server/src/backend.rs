use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use ropey::Rope;
use tokio::sync::RwLock;
use tower_lsp_server::jsonrpc::{
    Error,
    Result,
};
use tower_lsp_server::lsp_types::*;
use tower_lsp_server::{
    Client,
    LanguageServer,
    UriExt,
};

use amber_analysis::files::{
    FileVersion,
    Files,
    DEFAULT_VERSION,
};
use amber_analysis::stdlib::save_resources;
use amber_analysis::{
    self as analysis,
    AnalysisHost,
    SymbolInfo,
    SymbolTable,
};
use amber_grammar::{
    self as grammar,
    Grammar,
    LSPAnalysis,
    ParserResponse,
};
use amber_types::fs::{
    LocalFs,
    FS,
};
use amber_types::paths::FileId;

pub use amber_types::AmberVersion;

#[derive(Debug)]
pub struct Backend {
    pub client: Client,
    pub files: Files,
    /// The LSP analysis implementation.
    pub lsp_analysis: Box<dyn LSPAnalysis>,
    pub token_types: Box<[SemanticTokenType]>,
    pub amber_version: AmberVersion,
}

impl AnalysisHost for Backend {
    fn get_amber_version(&self) -> AmberVersion {
        self.amber_version.clone()
    }

    fn get_files(&self) -> &Files {
        &self.files
    }

    fn open_document<'a>(
        &'a self,
        uri: &'a Uri,
    ) -> Pin<Box<dyn Future<Output = Result<(FileId, FileVersion)>> + Send + 'a>> {
        Box::pin(async move {
            if let Some(file_id) = self.files.get(uri) {
                let version = self.files.get_latest_version(file_id);
                return Ok((file_id, version));
            }

            let file_path = match uri.to_file_path() {
                Some(path) => path,
                None => {
                    return Err(Error::invalid_params(
                        "Invalid URI: Unable to convert to file path.",
                    ));
                }
            };

            let text = match self.files.fs.read(&file_path).await {
                Ok(text) => Rope::from_str(&text),
                Err(_) => {
                    return Err(Error::internal_error());
                }
            };

            let file_id = self.files.insert(uri.clone(), DEFAULT_VERSION);

            self.files
                .document_map
                .insert((file_id, DEFAULT_VERSION), text);

            self.analyze_document(file_id, DEFAULT_VERSION).await;

            Ok((file_id, DEFAULT_VERSION))
        })
    }

    async fn show_error(&self, message: &str) {
        self.client.show_message(MessageType::ERROR, message).await;
    }
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
                AmberVersion::Alpha040 => Box::new(grammar::alpha040::AmberCompiler::new()),
                AmberVersion::Alpha050 => Box::new(grammar::alpha050::AmberCompiler::new()),
                AmberVersion::Alpha060 => Box::new(grammar::alpha060::AmberCompiler::new()),
            },
            token_types: match amber_version {
                AmberVersion::Alpha034 => Box::new(grammar::alpha034::semantic_tokens::LEGEND_TYPE),
                AmberVersion::Alpha035 => Box::new(grammar::alpha035::semantic_tokens::LEGEND_TYPE),
                AmberVersion::Alpha040 => Box::new(grammar::alpha040::semantic_tokens::LEGEND_TYPE),
                AmberVersion::Alpha050 => Box::new(grammar::alpha050::semantic_tokens::LEGEND_TYPE),
                AmberVersion::Alpha060 => Box::new(grammar::alpha060::semantic_tokens::LEGEND_TYPE),
            },
            amber_version,
        }
    }

    #[tracing::instrument(skip_all)]
    async fn publish_diagnostics(
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

    #[tracing::instrument(skip_all)]
    pub async fn publish_issues(&self, file_id: FileId, file_version: FileVersion) {
        let (rope, version) = match self.files.get_document_latest_version(file_id) {
            Some(document) => document,
            None => return,
        };

        if file_version != version {
            return;
        }

        let diagnostics =
            crate::diagnostics::collect_diagnostics(self, file_id, file_version, &rope);

        self.publish_diagnostics(&file_id, diagnostics, Some(version))
            .await;
    }

    #[tracing::instrument(skip_all)]
    pub async fn analyze_document(&self, file_id: FileId, version: FileVersion) {
        let rope = match self.files.document_map.get(&(file_id, version)) {
            Some(document) => document.clone(),
            None => return,
        };

        if self.files.analyze_lock.contains_key(&(file_id, version)) {
            return;
        }

        let lock = Arc::new(RwLock::new(false));

        let c_lock = lock.clone();
        let mut lock_w = c_lock.write().await;

        self.files.analyze_lock.insert((file_id, version), lock);

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
            Grammar::Alpha040(Some(ast)) => {
                analysis::alpha040::global::analyze_global_stmnt(file_id, version, &ast, self)
                    .await;
            }
            Grammar::Alpha050(Some(ast)) => {
                analysis::alpha050::global::analyze_global_stmnt(file_id, version, &ast, self)
                    .await;
                analysis::alpha050::unused::check_unused_symbols(file_id, version, &self.files);
            }
            Grammar::Alpha060(Some(ast)) => {
                analysis::alpha060::global::analyze_global_stmnt(file_id, version, &ast, self)
                    .await;
                analysis::alpha060::unused::check_unused_symbols(file_id, version, &self.files);
            }
            _ => {}
        }

        *lock_w = true;
        drop(lock_w);

        Box::pin(async {
            self.analyze_dependencies(file_id, version).await;
        })
        .await;
    }

    pub fn offset_to_position(&self, offset: usize, rope: &Rope) -> Position {
        let line = rope
            .try_char_to_line(offset)
            .ok()
            .unwrap_or(rope.len_lines());
        let first_char_of_line = rope.try_line_to_char(line).ok().unwrap_or(rope.len_chars());
        let column = offset - first_char_of_line;

        Position::new(line as u32, column as u32)
    }

    async fn analyze_dependencies(&self, file_id: FileId, file_version: FileVersion) {
        let deps = self.files.get_files_dependant_on(file_id);

        for (dep_file_id, dep_file_version) in deps {
            if dep_file_id == file_id {
                continue;
            }

            let new_version = self.files.get_latest_version(file_id);
            if file_version != new_version {
                return;
            }

            self.analyze_document(dep_file_id, dep_file_version).await;
            self.publish_issues(dep_file_id, dep_file_version).await;
        }
    }

    pub(crate) async fn position_to_offset(
        &self,
        file: (FileId, FileVersion),
        position: Position,
    ) -> Option<usize> {
        let rope = match self.files.document_map.get(&file) {
            Some(document) => document.clone(),
            None => return None,
        };

        if !self.files.is_file_analyzed(&file).await {
            return None;
        }

        let char = rope
            .try_line_to_char(position.line as usize)
            .ok()
            .unwrap_or(rope.len_chars());
        Some(char + position.character as usize)
    }

    pub(crate) async fn get_symbol_at_position(
        &self,
        file_id: FileId,
        position: Position,
    ) -> Option<(SymbolInfo, usize)> {
        let version = self.files.get_latest_version(file_id);
        let file = (file_id, version);

        let offset = match self.position_to_offset(file, position).await {
            Some(offset) => offset,
            None => return None,
        };

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

impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        save_resources(self).await;

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
                                    token_modifiers: vec![],
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

        let file_id = self.files.insert(params.text_document.uri, version);

        self.files.document_map.insert(
            (file_id, version),
            Rope::from_str(&params.text_document.text),
        );

        self.analyze_document(file_id, version).await;

        self.publish_issues(file_id, version).await;
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

        self.files.add_new_file_version(file_id, new_version);

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
            let mut document = match self.files.get_document_latest_version(file_id) {
                Some((document, _)) => document.clone(),
                None => {
                    return self
                        .client
                        .log_message(MessageType::ERROR, format!("document {uri} is not open"))
                        .await;
                }
            };

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

        self.analyze_document(file_id, new_version).await;

        self.publish_issues(file_id, new_version).await;
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
        crate::semantic_tokens::handle_semantic_tokens_full(self, params).await
    }

    #[tracing::instrument(skip_all)]
    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        crate::semantic_tokens::handle_semantic_tokens_range(self, params).await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        crate::goto_definition::handle_goto_definition(self, params).await
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
        crate::hover::handle_hover(self, params).await
    }

    #[tracing::instrument(skip_all)]
    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        crate::completion::handle_completion(self, params).await
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        crate::hover::handle_signature_help(self, params).await
    }
}
