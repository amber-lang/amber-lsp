use ropey::Rope;
use tower_lsp_server::lsp_types::*;

use amber_analysis::files::FileVersion;
use amber_types::paths::FileId;

use crate::backend::Backend;

/// Convert a `(message, span)` pair into a [`Diagnostic`] with the given severity.
fn span_to_diagnostic(
    backend: &Backend,
    rope: &Rope,
    msg: &str,
    span: &chumsky::span::SimpleSpan,
    severity: DiagnosticSeverity,
    tags: Option<Vec<DiagnosticTag>>,
) -> Diagnostic {
    let start_position = backend.offset_to_position(span.start, rope);
    let end_position = backend.offset_to_position(span.end, rope);

    Diagnostic {
        range: Range::new(start_position, end_position),
        severity: Some(severity),
        tags,
        message: msg.to_string(),
        ..Default::default()
    }
}

/// Collect all diagnostics (errors, warnings, unused) for a given file version.
pub fn collect_diagnostics(
    backend: &Backend,
    file_id: FileId,
    file_version: FileVersion,
    rope: &Rope,
) -> Vec<Diagnostic> {
    let mut diagnostics: Vec<Diagnostic> = vec![];

    if let Some(errors) = backend.files.errors.get(&(file_id, file_version)) {
        diagnostics.extend(errors.iter().map(|(msg, span)| {
            span_to_diagnostic(backend, rope, msg, span, DiagnosticSeverity::ERROR, None)
        }));
    }

    if let Some(warnings) = backend.files.warnings.get(&(file_id, file_version)) {
        diagnostics.extend(warnings.iter().map(|(msg, span)| {
            span_to_diagnostic(backend, rope, msg, span, DiagnosticSeverity::WARNING, None)
        }));
    }

    if let Some(unused) = backend
        .files
        .unused_diagnostics
        .get(&(file_id, file_version))
    {
        diagnostics.extend(unused.iter().map(|(msg, span)| {
            span_to_diagnostic(
                backend,
                rope,
                msg,
                span,
                DiagnosticSeverity::WARNING,
                Some(vec![DiagnosticTag::UNNECESSARY]),
            )
        }));
    }

    diagnostics
}
