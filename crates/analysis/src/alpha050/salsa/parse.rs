//! Parse query: tokenize + parse with Salsa memoization.
//!
//! Contains the pure `tokenize_and_parse` function and the Salsa-tracked
//! `parse_file` query that wraps it for memoization.

use amber_grammar::SpannedSemanticToken;
use amber_types::AmberVersion;
use chumsky::span::SimpleSpan;

use super::types::{
    ParseOutput,
    ParsedFile,
    SourceFile,
};
use super::Db;

// ---------------------------------------------------------------------------
// Pure tokenize + parse function (no Salsa, no I/O)
// ---------------------------------------------------------------------------

/// Pure tokenize + parse, factored out of `Backend::analyze_document`.
///
/// This function has no side effects: given the same `(text, amber_version)`,
/// it always produces the same output.
pub fn tokenize_and_parse(text: &str, amber_version: AmberVersion) -> ParseOutput {
    let compiler: Box<dyn amber_grammar::LSPAnalysis> = match amber_version {
        AmberVersion::Alpha034 => Box::new(amber_grammar::alpha034::AmberCompiler::new()),
        AmberVersion::Alpha035 => Box::new(amber_grammar::alpha035::AmberCompiler::new()),
        AmberVersion::Alpha040 => Box::new(amber_grammar::alpha040::AmberCompiler::new()),
        AmberVersion::Alpha050 => Box::new(amber_grammar::alpha050::AmberCompiler::new()),
    };
    let tokens = compiler.tokenize(text);
    let response = compiler.parse(&tokens);
    ParseOutput {
        ast: response.ast,
        errors: response
            .errors
            .iter()
            .map(|err| (err.to_string(), *err.span()))
            .collect(),
        semantic_tokens: response.semantic_tokens,
    }
}

// ---------------------------------------------------------------------------
// Salsa query
// ---------------------------------------------------------------------------

/// Pure tokenize + parse. Depends only on `SourceFile::text` and `SourceFile::amber_version`.
#[salsa::tracked]
pub fn parse_file<'db>(db: &'db dyn Db, source: SourceFile) -> ParsedFile<'db> {
    let text = source.text(db);
    let amber_version = source.amber_version(db);
    let output = tokenize_and_parse(text, amber_version);
    ParsedFile::new(
        db,
        source,
        output.ast,
        output.errors,
        output.semantic_tokens,
    )
}

/// Extract only the parse errors as diagnostics.
///
/// This is a convenience query for consumers that only need diagnostics
/// without the full AST. Memoized independently — if the AST changes but
/// errors stay the same, downstream queries aren't invalidated.
#[salsa::tracked(returns(ref))]
pub fn file_parse_errors(db: &dyn Db, source: SourceFile) -> Vec<(String, SimpleSpan)> {
    parse_file(db, source).parse_errors(db).clone()
}

/// Extract only the semantic tokens from parsing.
///
/// Convenience query for consumers that need syntax highlighting tokens
/// without triggering full analysis.
#[salsa::tracked(returns(ref))]
pub fn file_semantic_tokens(db: &dyn Db, source: SourceFile) -> Vec<SpannedSemanticToken> {
    parse_file(db, source).semantic_tokens(db).clone()
}
