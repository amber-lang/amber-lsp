use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::lsp_types::*;

use amber_analysis::SymbolType;

use crate::backend::Backend;

/// Handle `textDocument/definition`.
pub async fn handle_goto_definition(
    backend: &Backend,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>> {
    let uri = params.text_document_position_params.text_document.uri;
    let file_id = match backend.files.get(&uri) {
        Some(file_id) => file_id,
        None => return Ok(None),
    };

    let (rope, version) = match backend.files.get_document_latest_version(file_id) {
        Some(document) => document,
        None => return Ok(None),
    };

    if !backend.files.is_file_analyzed(&(file_id, version)).await {
        return Ok(None);
    }

    let position = params.text_document_position_params.position;
    let char = rope
        .try_line_to_char(position.line as usize)
        .ok()
        .unwrap_or(rope.len_chars());
    let offset = char + position.character as usize;

    let symbol_table = match backend.files.symbol_table.get(&(file_id, version)) {
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

    let definition = match symbol_table.definitions.get(&symbol_info.name) {
        Some(definitions) => match definitions.get(&offset) {
            Some(definition) => {
                let definition_file_rope =
                    match backend.files.get_document_latest_version(definition.file.0) {
                        Some((document, _)) => document.clone(),
                        None => return Ok(None),
                    };

                let start_position =
                    backend.offset_to_position(definition.start, &definition_file_rope);
                let end_position =
                    backend.offset_to_position(definition.end, &definition_file_rope);

                let file_uri = backend.files.lookup(&definition.file.0);

                match symbol_info.symbol_type {
                    SymbolType::ImportPath => {
                        let selection_range = Range {
                            start: backend.offset_to_position(symbol_info.span.start, &rope),
                            end: backend.offset_to_position(symbol_info.span.end, &rope),
                        };

                        Some(GotoDefinitionResponse::Link(vec![LocationLink {
                            origin_selection_range: Some(selection_range),
                            target_uri: file_uri,
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
                        file_uri,
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

    Ok(definition)
}
