use chumsky::span::SimpleSpan;
use clap::builder::PossibleValue;
use clap::ValueEnum;
use dashmap::DashMap;
use ropey::Rope;
use serde_json::Value;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::fs::{LocalFs, FS};
use crate::grammar::Spanned;
use crate::grammar::{
    alpha034::{semantic_tokens::LEGEND_TYPE, AmberCompiler},
    Grammar, LSPAnalysis, ParserResponse, SpannedSemanticToken,
};
use crate::paths::{FileId, PathInterner};
use crate::symbol_table::alpha034::global::analyze_global_stmnt;
use crate::symbol_table::types::GenericsMap;
use crate::symbol_table::{FunctionSymbol, SymbolTable, SymbolType};

#[derive(Clone, Debug, PartialEq)]
pub enum AmberVersion {
    Auto,
    Alpha034,
    Alpha035,
    Alpha040,
}

impl ValueEnum for AmberVersion {
    fn value_variants<'a>() -> &'a [AmberVersion] {
        &[AmberVersion::Auto, AmberVersion::Alpha034]
    }

    fn to_possible_value(&self) -> Option<PossibleValue> {
        match self {
            AmberVersion::Auto => Some(PossibleValue::new("auto")),
            AmberVersion::Alpha034 => Some(PossibleValue::new("0.3.4-alpha")),
            AmberVersion::Alpha035 => Some(PossibleValue::new("0.3.5-alpha")),
            AmberVersion::Alpha040 => Some(PossibleValue::new("0.4.0-alpha")),
        }
    }
}

#[derive(Debug)]
pub struct Backend {
    pub client: Client,
    pub paths: PathInterner,
    pub fs: Box<dyn FS>,
    /// A map from document URI to the parsed AST.
    pub ast_map: DashMap<FileId, Grammar>,
    /// A map from document URI to the parse errors.
    pub errors: DashMap<FileId, Vec<Spanned<String>>>,
    /// A map from document URI to the document content and version.
    pub document_map: DashMap<FileId, (Rope, i32)>,
    /// A map from document URI to the semantic tokens.
    pub semantic_token_map: DashMap<FileId, Vec<SpannedSemanticToken>>,
    /// A map from document URI to the symbol table.
    pub symbol_table: DashMap<FileId, SymbolTable>,
    pub generic_types: GenericsMap,
    /// The LSP analysis implementation.
    pub lsp_analysis: Box<dyn LSPAnalysis>,
    pub token_types: Box<[SemanticTokenType]>,
    pub amber_version: AmberVersion,
}

impl Backend {
    pub fn new(client: Client, amber_version: AmberVersion, fs: Option<Box<dyn FS>>) -> Self {
        let (lsp_analysis, token_types) = match amber_version {
            _ => (Box::new(AmberCompiler::new()), Box::new(LEGEND_TYPE)),
        };

        let fs = if let Some(fs) = fs {
            fs
        } else {
            Box::new(LocalFs::new())
        };

        Self {
            client,
            paths: PathInterner::default(),
            fs,
            ast_map: DashMap::new(),
            errors: DashMap::new(),
            document_map: DashMap::new(),
            semantic_token_map: DashMap::new(),
            symbol_table: DashMap::new(),
            generic_types: GenericsMap::new(),
            lsp_analysis,
            token_types,
            amber_version,
        }
    }

    pub fn get_document(&self, file_id: &FileId) -> Option<(Rope, i32)> {
        match self.document_map.get(&file_id) {
            Some(document) => Some(document.clone()),
            None => None,
        }
    }

    pub fn open_document(&self, uri: &Url) -> Result<FileId> {
        if let Some(file_id) = self.paths.get(uri) {
            return Ok(file_id);
        }

        let text = match self.fs.read(&uri.to_file_path().unwrap().to_string_lossy()) {
            Ok(text) => Rope::from_str(&text),
            Err(_) => {
                return Err(Error::internal_error());
            }
        };

        let file_id = self.paths.insert(uri.clone());

        self.document_map.insert(file_id, (text, 0));

        self.analize_document(&file_id);

        Ok(file_id)
    }

    pub async fn publish_diagnostics(
        &self,
        file_id: &FileId,
        diagnostics: Vec<Diagnostic>,
        version: Option<i32>,
    ) {
        let uri = self.paths.lookup(file_id);
        self.client
            .publish_diagnostics(uri, diagnostics, version)
            .await;
    }

    pub async fn publish_syntax_errors(&self, file_id: &FileId) {
        let errors = match self.errors.get(file_id) {
            Some(errors) => errors.clone(),
            None => return,
        };

        let (rope, version) = match self.get_document(file_id) {
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

        self.publish_diagnostics(file_id, diagnostics, Some(version))
            .await;
    }

    pub fn analize_document(&self, file_id: &FileId) {
        let (rope, _) = match self.get_document(file_id) {
            Some(document) => document,
            None => return,
        };

        let tokens = self.lsp_analysis.tokenize(&rope.to_string());

        let ParserResponse {
            ast,
            errors,
            semantic_tokens,
        } = self.lsp_analysis.parse(&tokens);

        self.errors.insert(
            *file_id,
            errors
                .iter()
                .map(|err| (err.to_string(), *err.span()))
                .collect(),
        );
        self.ast_map.insert(*file_id, ast.clone());
        self.semantic_token_map.insert(*file_id, semantic_tokens);

        self.symbol_table.insert(*file_id, SymbolTable::default());

        match ast {
            Grammar::Alpha034(Some(ast)) => {
                analyze_global_stmnt(file_id, &ast, self);
            }
            _ => {}
        }
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

    pub fn report_error(&self, file_id: &FileId, msg: &str, span: SimpleSpan) {
        let mut errors = match self.errors.get(file_id) {
            Some(errors) => errors.clone(),
            None => vec![],
        };
        errors.push((msg.to_string(), span));
        self.errors.insert(*file_id, errors);
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

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let file_id = self.paths.insert(params.text_document.uri);

        self.document_map.insert(
            file_id,
            (
                Rope::from_str(&params.text_document.text),
                params.text_document.version,
            ),
        );

        self.analize_document(&file_id);

        self.publish_syntax_errors(&file_id).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();

        let file_id = match self.paths.get(&params.text_document.uri) {
            Some(file_id) => file_id,
            None => {
                return self
                    .client
                    .log_message(MessageType::ERROR, format!("document {uri} is not open"))
                    .await;
            }
        };

        if !self.document_map.contains_key(&file_id) {
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
            self.document_map.insert(
                file_id,
                (Rope::from_str(&change.text), params.text_document.version),
            );
        } else {
            let mut document = self.document_map.get_mut(&file_id).unwrap();

            params
                .content_changes
                .iter()
                .filter(|change| change.range.is_some())
                .for_each(|change| {
                    let range = change.range.as_ref().unwrap();
                    let start = document.0.line_to_char(range.start.line as usize)
                        + range.start.character as usize;
                    let end = document.0.line_to_char(range.end.line as usize)
                        + range.end.character as usize;

                    document.0.remove(start..end);
                    document.0.insert(start, &change.text);
                });

            document.1 = params.text_document.version;
        }

        self.analize_document(&file_id);

        self.publish_syntax_errors(&file_id).await;
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

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let file_id = match self.paths.get(&params.text_document.uri) {
            Some(file_id) => file_id,
            None => {
                return Ok(None);
            }
        };

        let semantic_tokens = match self.semantic_token_map.get(&file_id) {
            Some(tokens) => tokens.clone(),
            None => {
                return Ok(None);
            }
        };

        let (rope, _) = match self.get_document(&file_id) {
            Some(document) => document,
            None => return Ok(None),
        };

        let mut pre_line = 0;
        let mut pre_start = 0;

        let data = semantic_tokens
            .iter()
            .filter_map(|(token, span)| {
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

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let file_id = match self.paths.get(&params.text_document.uri) {
            Some(file_id) => file_id,
            None => {
                return Ok(None);
            }
        };
        let requested_range = params.range;

        let semantic_tokens = match self.semantic_token_map.get(&file_id) {
            Some(tokens) => tokens.clone(),
            None => {
                return Ok(None);
            }
        };

        let (rope, _) = match self.get_document(&file_id) {
            Some(document) => document,
            None => return Ok(None),
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
            let file_id = match self.paths.get(&uri) {
                Some(file_id) => file_id,
                None => return Ok(None),
            };

            let (rope, _) = match self.document_map.get(&file_id) {
                Some(document) => document.clone(),
                None => return Ok(None),
            };

            let position = params.text_document_position_params.position;
            let char = rope
                .try_line_to_char(position.line as usize)
                .ok()
                .unwrap_or(rope.len_chars());
            let offset = char + position.character as usize;

            let symbol_table = match self.symbol_table.get(&file_id) {
                Some(symbol_table) => symbol_table,
                None => return Ok(None),
            };

            let symbol_info = match symbol_table.symbols.get(&offset) {
                Some(symbol) => symbol.clone(),
                None => return Ok(None),
            };

            if symbol_info.undefined || symbol_info.is_definition {
                return Ok(None);
            }

            let response = match symbol_table.definitions.get(&symbol_info.name) {
                Some(definitions) => match definitions.get(&offset) {
                    Some(definition) => {
                        let file_rope = match self.document_map.get(&definition.file) {
                            Some(document) => document.0.clone(),
                            None => {
                                return Ok(None);
                            }
                        };

                        let start_position = self
                            .offset_to_position(definition.start, &file_rope)
                            .unwrap();
                        let end_position =
                            self.offset_to_position(definition.end, &file_rope).unwrap();

                        let file_url = self.paths.lookup(&definition.file);

                        Some(GotoDefinitionResponse::Scalar(Location::new(
                            file_url,
                            Range {
                                start: start_position,
                                end: end_position,
                            },
                        )))
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

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        self.client
            .log_message(MessageType::INFO, "command executed!")
            .await;

        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let file_id = match self.paths.get(&uri) {
            Some(file_id) => file_id,
            None => return Ok(None),
        };

        let (rope, _) = match self.document_map.get(&file_id) {
            Some(document) => document.clone(),
            None => return Ok(None),
        };

        let position = params.text_document_position_params.position;
        let char = rope
            .try_line_to_char(position.line as usize)
            .ok()
            .unwrap_or(rope.len_chars());
        let offset = char + position.character as usize;

        let symbol_table = match self.symbol_table.get(&file_id) {
            Some(symbol_table) => symbol_table,
            None => return Ok(None),
        };

        let symbol_info = match symbol_table.symbols.get(&offset) {
            Some(symbol) => symbol.clone(),
            None => return Ok(None),
        };

        if symbol_info.undefined {
            return Ok(None);
        }

        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!(
                    "```ab\n{}{}: {}\n```",
                    symbol_info.name,
                    match symbol_info.symbol_type {
                        SymbolType::Function(FunctionSymbol {
                            arguments,
                            is_public: _,
                        }) => format!(
                            "({})",
                            arguments
                                .iter()
                                .map(|(name, ty)| format!(
                                    "{}: {}",
                                    name,
                                    ty.to_string(&self.generic_types)
                                ))
                                .collect::<Vec<String>>()
                                .join(", ")
                        ),
                        _ => "".to_string(),
                    },
                    symbol_info.data_type.to_string(&self.generic_types),
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
}
