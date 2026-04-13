use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::lsp_types::*;
use tower_lsp_server::UriExt;

use amber_grammar::Grammar;

use crate::backend::Backend;

pub async fn handle_code_lens(
    backend: &Backend,
    params: CodeLensParams,
) -> Result<Option<Vec<CodeLens>>> {
    let uri = &params.text_document.uri;

    let file_id = match backend.files.get(uri) {
        Some(file_id) => file_id,
        None => return Ok(None),
    };

    let version = backend.files.get_latest_version(file_id);
    let file = (file_id, version);

    let rope = match backend.files.document_map.get(&file) {
        Some(document) => document.clone(),
        None => return Ok(None),
    };

    let ast = match backend.files.ast_map.get(&file) {
        Some(ast) => ast.clone(),
        None => return Ok(None),
    };

    let test_blocks = match &ast {
        Grammar::Alpha060(Some(stmnts)) => {
            stmnts
                .iter()
                .filter_map(|(global, _span)| {
                    if let amber_grammar::alpha060::GlobalStatement::TestBlock(
                        _test_keyword,
                        Some((name, _name_span)),
                        _body,
                    ) = global
                    {
                        if name.is_empty() {
                            return None;
                        }
                        // Use _span (the full test block span) for the code lens position
                        let start_offset = _span.start;
                        let start_pos = backend.offset_to_position(start_offset, &rope);
                        let end_pos = backend.offset_to_position(_span.end, &rope);
                        let range = Range::new(start_pos, end_pos);

                        let file_path = uri.to_file_path()?;
                        let file_path_str = file_path.to_string_lossy().to_string();

                        Some(CodeLens {
                            range,
                            command: Some(Command {
                                title: format!("▶ Run Test: {name}"),
                                command: "amber.runTest".to_string(),
                                arguments: Some(vec![
                                    serde_json::Value::String(file_path_str),
                                    serde_json::Value::String(name.clone()),
                                ]),
                            }),
                            data: None,
                        })
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        }
        _ => vec![],
    };

    if test_blocks.is_empty() {
        Ok(None)
    } else {
        Ok(Some(test_blocks))
    }
}
