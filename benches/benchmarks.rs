use std::path::Path;
use std::sync::Arc;

use amber_analysis::files::DEFAULT_VERSION;
use amber_analysis::{
    AnalysisHost,
    SymbolTable,
};
use amber_grammar::{
    Grammar,
    LSPAnalysis,
};
use amber_lsp::backend::{
    AmberVersion,
    Backend,
};
use amber_types::fs::MemoryFS;
use criterion::{
    criterion_group,
    criterion_main,
    BenchmarkId,
    Criterion,
};
use include_dir::{
    include_dir,
    Dir,
};
use ropey::Rope;
use tower_lsp_server::lsp_types::{
    CompletionContext,
    CompletionParams,
    CompletionTriggerKind,
    Position,
    TextDocumentIdentifier,
    TextDocumentPositionParams,
    Uri,
};
use tower_lsp_server::UriExt;

static RESOURCES: Dir = include_dir!("$CARGO_MANIFEST_DIR/../../resources/");

/// Collect all stdlib source files for a given version into one concatenated string.
fn load_stdlib_source(version_dir: &str) -> String {
    let dir = RESOURCES
        .get_dir(version_dir)
        .unwrap_or_else(|| panic!("Missing resource directory: {version_dir}"));

    let mut source = String::new();
    collect_files(dir, &mut source);
    source
}

fn collect_files(dir: &Dir, source: &mut String) {
    for entry in dir.entries() {
        match entry {
            include_dir::DirEntry::Dir(d) => collect_files(d, source),
            include_dir::DirEntry::File(f) => {
                if let Some(contents) = f.contents_utf8() {
                    source.push_str(contents);
                    source.push('\n');
                }
            }
        }
    }
}

/// Create a tokio runtime for async benchmarks.
fn rt() -> tokio::runtime::Runtime {
    tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap()
}

// ─── Version configurations ──────────────────────────────────────────

struct VersionConfig {
    name: &'static str,
    resource_dir: &'static str,
    amber_version: AmberVersion,
    compiler: Box<dyn LSPAnalysis>,
}

fn versions() -> Vec<VersionConfig> {
    vec![
        VersionConfig {
            name: "alpha034",
            resource_dir: "alpha034",
            amber_version: AmberVersion::Alpha034,
            compiler: Box::new(amber_grammar::alpha034::AmberCompiler::new()),
        },
        VersionConfig {
            name: "alpha035",
            resource_dir: "alpha035",
            amber_version: AmberVersion::Alpha035,
            compiler: Box::new(amber_grammar::alpha035::AmberCompiler::new()),
        },
        VersionConfig {
            name: "alpha040",
            resource_dir: "alpha040",
            amber_version: AmberVersion::Alpha040,
            compiler: Box::new(amber_grammar::alpha040::AmberCompiler::new()),
        },
        VersionConfig {
            name: "alpha050",
            resource_dir: "alpha050",
            amber_version: AmberVersion::Alpha050,
            compiler: Box::new(amber_grammar::alpha050::AmberCompiler::new()),
        },
    ]
}

// ─── Tokenize benchmarks ────────────────────────────────────────────

fn bench_tokenize(c: &mut Criterion) {
    let mut group = c.benchmark_group("tokenize");

    for v in versions() {
        let source = load_stdlib_source(v.resource_dir);
        let line_count = source.lines().count();

        group.bench_with_input(
            BenchmarkId::new(v.name, format!("{line_count} lines")),
            &source,
            |b, src| {
                b.iter(|| {
                    let _tokens = v.compiler.tokenize(src);
                });
            },
        );
    }

    group.finish();
}

// ─── Parse benchmarks ───────────────────────────────────────────────

fn bench_parse(c: &mut Criterion) {
    let mut group = c.benchmark_group("parse");

    group.sample_size(30);

    for v in versions() {
        let source = load_stdlib_source(v.resource_dir);
        let line_count = source.lines().count();
        let tokens = v.compiler.tokenize(&source);

        group.bench_with_input(
            BenchmarkId::new(v.name, format!("{line_count} lines")),
            &tokens,
            |b, toks| {
                b.iter(|| {
                    let _parsed = v.compiler.parse(toks);
                });
            },
        );
    }

    group.finish();
}

// ─── Analysis benchmarks ────────────────────────────────────────────

fn bench_analysis(c: &mut Criterion) {
    let mut group = c.benchmark_group("analysis");

    group.sample_size(10);

    for v in versions() {
        let source = load_stdlib_source(v.resource_dir);
        let line_count = source.lines().count();
        let amber_version = v.amber_version.clone();

        // Pre-tokenize and pre-parse so the benchmark only measures analysis.
        let tokens = v.compiler.tokenize(&source);
        let parsed = v.compiler.parse(&tokens);
        let pre_ast = parsed.ast;

        let runtime = rt();

        group.bench_with_input(
            BenchmarkId::new(v.name, format!("{line_count} lines")),
            &source,
            |b, src| {
                b.iter(|| {
                    runtime.block_on(async {
                        let (service, _) = tower_lsp_server::LspService::new(|client| {
                            Backend::new(
                                client,
                                amber_version.clone(),
                                Some(Arc::new(MemoryFS::new())),
                            )
                        });
                        let backend = service.inner();

                        let file = Path::new("/bench_main.ab");
                        let uri = Uri::from_file_path(file).unwrap();

                        // Populate internal state without tokenizing/parsing.
                        let file_id = backend.files.insert(uri.clone(), DEFAULT_VERSION);
                        backend
                            .files
                            .document_map
                            .insert((file_id, DEFAULT_VERSION), Rope::from_str(src));
                        backend
                            .files
                            .symbol_table
                            .insert((file_id, DEFAULT_VERSION), SymbolTable::default());

                        // Run only the analysis dispatch.
                        match &pre_ast {
                            Grammar::Alpha034(Some(ast)) => {
                                amber_analysis::alpha034::global::analyze_global_stmnt(
                                    file_id,
                                    DEFAULT_VERSION,
                                    ast,
                                    backend,
                                )
                                .await;
                            }
                            Grammar::Alpha035(Some(ast)) => {
                                amber_analysis::alpha035::global::analyze_global_stmnt(
                                    file_id,
                                    DEFAULT_VERSION,
                                    ast,
                                    backend,
                                )
                                .await;
                            }
                            Grammar::Alpha040(Some(ast)) => {
                                amber_analysis::alpha040::global::analyze_global_stmnt(
                                    file_id,
                                    DEFAULT_VERSION,
                                    ast,
                                    backend,
                                )
                                .await;
                            }
                            Grammar::Alpha050(Some(ast)) => {
                                amber_analysis::alpha050::global::analyze_global_stmnt(
                                    file_id,
                                    DEFAULT_VERSION,
                                    ast,
                                    backend,
                                )
                                .await;
                                amber_analysis::alpha050::unused::check_unused_symbols(
                                    file_id,
                                    DEFAULT_VERSION,
                                    &backend.files,
                                );
                            }
                            _ => {}
                        }
                    });
                });
            },
        );
    }

    group.finish();
}

// ─── End-to-end benchmarks (tokenize + parse + analysis) ────────────

fn bench_end_to_end(c: &mut Criterion) {
    let mut group = c.benchmark_group("end_to_end");

    group.sample_size(10);

    for v in versions() {
        let source = load_stdlib_source(v.resource_dir);
        let line_count = source.lines().count();
        let amber_version = v.amber_version.clone();
        let runtime = rt();

        group.bench_with_input(
            BenchmarkId::new(v.name, format!("{line_count} lines")),
            &source,
            |b, src| {
                b.iter(|| {
                    runtime.block_on(async {
                        let (service, _) = tower_lsp_server::LspService::new(|client| {
                            Backend::new(
                                client,
                                amber_version.clone(),
                                Some(Arc::new(MemoryFS::new())),
                            )
                        });
                        let backend = service.inner();

                        let file = Path::new("/bench_main.ab");
                        let uri = Uri::from_file_path(file).unwrap();

                        backend
                            .files
                            .fs
                            .write(&uri.to_file_path().unwrap(), src)
                            .await
                            .unwrap();

                        let _file_id = backend.open_document(&uri).await.unwrap();
                    });
                });
            },
        );
    }

    group.finish();
}

// ─── Autocomplete benchmarks ────────────────────────────────────────

/// Sample source used by the autocomplete benchmarks. Defines some symbols
/// then references `result` where the cursor is placed.
const AUTOCOMPLETE_SOURCE: &str = r#"
fun greet(name) {
    echo "Hello {name}"
}

fun goodbye(name) {
    echo "Bye {name}"
}

let result = greet("world")
echo result
"#;

/// Cursor position inside [`AUTOCOMPLETE_SOURCE`] — on `result` in the echo
/// line (line 10, col 5).
const AUTOCOMPLETE_CURSOR: (u32, u32) = (10, 5);

/// Benchmark the completion endpoint by opening a document with a partial
/// function call and requesting completions at that position.
fn bench_autocomplete(c: &mut Criterion) {
    let mut group = c.benchmark_group("autocomplete");

    group.sample_size(30);

    let autocomplete_versions: Vec<(&str, AmberVersion)> = vec![
        ("alpha034", AmberVersion::Alpha034),
        ("alpha035", AmberVersion::Alpha035),
        ("alpha040", AmberVersion::Alpha040),
        ("alpha050", AmberVersion::Alpha050),
    ];

    for (name, amber_version) in &autocomplete_versions {
        let amber_version = amber_version.clone();
        let source = AUTOCOMPLETE_SOURCE.to_string();
        let (cursor_line, cursor_char) = AUTOCOMPLETE_CURSOR;

        let runtime = rt();

        group.bench_function(BenchmarkId::new(*name, "completion"), |b| {
            b.iter(|| {
                runtime.block_on(async {
                    let (service, _) = tower_lsp_server::LspService::new(|client| {
                        Backend::new(
                            client,
                            amber_version.clone(),
                            Some(Arc::new(MemoryFS::new())),
                        )
                    });
                    let backend = service.inner();

                    let file = Path::new("/bench_complete.ab");
                    let uri = Uri::from_file_path(file).unwrap();

                    backend
                        .files
                        .fs
                        .write(&uri.to_file_path().unwrap(), &source)
                        .await
                        .unwrap();

                    let _file_id = backend.open_document(&uri).await.unwrap();

                    let params = CompletionParams {
                        text_document_position: TextDocumentPositionParams {
                            text_document: TextDocumentIdentifier { uri: uri.clone() },
                            position: Position::new(cursor_line, cursor_char),
                        },
                        work_done_progress_params: Default::default(),
                        partial_result_params: Default::default(),
                        context: Some(CompletionContext {
                            trigger_kind: CompletionTriggerKind::INVOKED,
                            trigger_character: None,
                        }),
                    };

                    use tower_lsp_server::LanguageServer;
                    let _result = backend.completion(params).await;
                });
            });
        });
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_tokenize,
    bench_parse,
    bench_analysis,
    bench_end_to_end,
    bench_autocomplete,
);
criterion_main!(benches);
