use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::lsp_types::*;

use amber_analysis::{
    FunctionSymbol,
    SymbolType,
};

use crate::backend::Backend;

/// Handle `textDocument/hover`.
pub async fn handle_hover(backend: &Backend, params: HoverParams) -> Result<Option<Hover>> {
    let file_id = match backend
        .files
        .get(&params.text_document_position_params.text_document.uri)
    {
        Some(file_id) => file_id,
        None => return Ok(None),
    };

    let version = backend.files.get_latest_version(file_id);

    if !backend.files.is_file_analyzed(&(file_id, version)).await {
        return Ok(None);
    }

    let position = params.text_document_position_params.position;

    let symbol_info = match backend.get_symbol_at_position(file_id, position).await {
        Some((symbol_info, _)) if !symbol_info.undefined => symbol_info,
        _ => return Ok(None),
    };

    Ok(Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!(
                "```amber\n{}\n```{}",
                symbol_info.to_string(&backend.files.generic_types),
                match symbol_info.symbol_type {
                    SymbolType::Function(FunctionSymbol { ref docs, .. }) if docs.is_some() =>
                        format!("\n{}", docs.clone().unwrap()),
                    _ => "".to_string(),
                },
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

/// Handle `textDocument/signatureHelp`.
pub async fn handle_signature_help(
    backend: &Backend,
    params: SignatureHelpParams,
) -> Result<Option<SignatureHelp>> {
    let file_id = match backend
        .files
        .get(&params.text_document_position_params.text_document.uri)
    {
        Some(file_id) => file_id,
        None => return Ok(None),
    };

    let version = backend.files.get_latest_version(file_id);

    if !backend.files.is_file_analyzed(&(file_id, version)).await {
        return Ok(None);
    }

    let symbol_table = match backend.files.symbol_table.get(&(file_id, version)) {
        Some(symbol_table) => symbol_table.clone(),
        None => return Ok(None),
    };

    let position = params.text_document_position_params.position;

    let offset = match backend
        .position_to_offset((file_id, version), position)
        .await
    {
        Some(offset) => offset,
        None => return Ok(None),
    };

    let symbol_info = match symbol_table.fun_call_arg_scope.get(&offset) {
        Some(symbol_info) => symbol_info.clone(),
        None => return Ok(None),
    };

    match symbol_info.symbol_type {
        SymbolType::Function(FunctionSymbol { ref arguments, .. }) => {
            let mut active_parameter = None;

            arguments.iter().enumerate().for_each(|(idx, (_, span))| {
                if offset >= span.start && offset <= span.end {
                    active_parameter = Some(idx as u32);
                }
            });

            Ok(Some(SignatureHelp {
                signatures: vec![SignatureInformation {
                    label: symbol_info.to_string(&backend.files.generic_types),
                    documentation: None,
                    parameters: Some(
                        arguments
                            .iter()
                            .map(|(arg, _)| ParameterInformation {
                                label: ParameterLabel::Simple(format!(
                                    "{}: {}",
                                    arg.name,
                                    arg.data_type.to_string(&backend.files.generic_types)
                                )),
                                documentation: None,
                            })
                            .collect::<Vec<ParameterInformation>>(),
                    ),
                    active_parameter,
                }],
                active_signature: Some(0),
                active_parameter,
            }))
        }
        _ => Ok(None),
    }
}
