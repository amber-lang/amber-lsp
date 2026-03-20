use ropey::Rope;
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::lsp_types::*;

use crate::backend::Backend;
use amber_analysis::files::FileVersion;
use amber_grammar::SpannedSemanticToken;

/// Map raw semantic tokens into LSP [`SemanticToken`] deltas.
///
/// The optional `range_filter` restricts output to tokens whose starting line
/// falls within the given LSP range.
fn map_semantic_tokens(
    rope: &Rope,
    raw_tokens: &[SpannedSemanticToken],
    range_filter: Option<&Range>,
) -> Vec<SemanticToken> {
    let mut pre_line: u32 = 0;
    let mut pre_start: u32 = 0;

    raw_tokens
        .iter()
        .filter_map(|(token, span)| {
            let line = rope.try_byte_to_line(span.start).ok()? as u32;
            let first = rope.try_line_to_char(line as usize).ok()? as u32;
            let start = rope.try_byte_to_char(span.start).ok()? as u32 - first;

            // If a range filter is provided, skip tokens outside the range.
            if let Some(range) = range_filter {
                if !((line > range.start.line
                    || (line == range.start.line && start >= range.start.character))
                    && (line < range.end.line
                        || (line == range.end.line && start <= range.end.character)))
                {
                    return None;
                }
            }

            if span.start > span.end {
                return None;
            }

            let length = (span.end - span.start) as u32;
            let delta_line = line - pre_line;
            let delta_start = if delta_line == 0 {
                start - pre_start
            } else {
                start
            };

            let ret = Some(SemanticToken {
                delta_line,
                delta_start,
                length,
                token_type: *token as u32,
                token_modifiers_bitset: 0,
            });
            pre_line = line;
            pre_start = start;
            ret
        })
        .collect()
}

/// Resolve the file and its semantic tokens, returning `None` if unavailable.
async fn resolve_tokens(
    backend: &Backend,
    uri: &Uri,
) -> Option<(Rope, FileVersion, Vec<SpannedSemanticToken>)> {
    let file_id = backend.files.get(uri)?;

    let (rope, file_version) = backend.files.get_document_latest_version(file_id)?;

    if !backend
        .files
        .is_file_analyzed(&(file_id, file_version))
        .await
    {
        return None;
    }

    let tokens = backend
        .files
        .semantic_token_map
        .get(&(file_id, file_version))?
        .clone();

    Some((rope, file_version, tokens))
}

/// Handle `textDocument/semanticTokens/full`.
pub async fn handle_semantic_tokens_full(
    backend: &Backend,
    params: SemanticTokensParams,
) -> Result<Option<SemanticTokensResult>> {
    let (rope, _version, raw_tokens) =
        match resolve_tokens(backend, &params.text_document.uri).await {
            Some(t) => t,
            None => return Ok(None),
        };

    let data = map_semantic_tokens(&rope, &raw_tokens, None);

    Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data,
    })))
}

/// Handle `textDocument/semanticTokens/range`.
pub async fn handle_semantic_tokens_range(
    backend: &Backend,
    params: SemanticTokensRangeParams,
) -> Result<Option<SemanticTokensRangeResult>> {
    let (rope, _version, raw_tokens) =
        match resolve_tokens(backend, &params.text_document.uri).await {
            Some(t) => t,
            None => return Ok(None),
        };

    let data = map_semantic_tokens(&rope, &raw_tokens, Some(&params.range));

    Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
        result_id: None,
        data,
    })))
}
