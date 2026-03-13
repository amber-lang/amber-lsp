use std::path::Path;
use std::sync::Arc;

use amber_analysis::AnalysisHost;
use amber_grammar::LSPAnalysis;
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

    for v in versions() {
        let source = load_stdlib_source(v.resource_dir);
        let line_count = source.lines().count();
        let amber_version = v.amber_version.clone();

        group.bench_with_input(
            BenchmarkId::new(v.name, format!("{line_count} lines")),
            &source,
            |b, src| {
                b.iter(|| {
                    let runtime = rt();
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

// ─── End-to-end benchmarks (tokenize + parse + analysis) ────────────

fn bench_end_to_end(c: &mut Criterion) {
    let mut group = c.benchmark_group("end_to_end");

    for v in versions() {
        let source = load_stdlib_source(v.resource_dir);
        let line_count = source.lines().count();
        let amber_version = v.amber_version.clone();

        group.bench_with_input(
            BenchmarkId::new(v.name, format!("{line_count} lines")),
            &source,
            |b, src| {
                b.iter(|| {
                    let runtime = rt();
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

/// Benchmark the completion endpoint by opening a document with a partial
/// function call and requesting completions at that position.
fn bench_autocomplete(c: &mut Criterion) {
    let mut group = c.benchmark_group("autocomplete");

    // Autocomplete test sources for each version.
    // Each source defines some symbols then has a partial identifier the
    // cursor sits on. We place the cursor on the `result` variable after
    // assignment — a position where the symbol table has definitions to
    // offer completions from.
    let autocomplete_sources: Vec<(&str, AmberVersion, &str, u32, u32)> = vec![
        (
            "alpha034",
            AmberVersion::Alpha034,
            r#"
fun greet(name) {
    echo "Hello {name}"
}

fun goodbye(name) {
    echo "Bye {name}"
}

let result = greet("world")
echo result
"#,
            // cursor on `result` in the echo line (line 10, col 5)
            10,
            5,
        ),
        (
            "alpha035",
            AmberVersion::Alpha035,
            r#"
fun greet(name) {
    echo "Hello {name}"
}

fun goodbye(name) {
    echo "Bye {name}"
}

let result = greet("world")
echo result
"#,
            10,
            5,
        ),
        (
            "alpha040",
            AmberVersion::Alpha040,
            r#"
fun greet(name) {
    echo "Hello {name}"
}

fun goodbye(name) {
    echo "Bye {name}"
}

let result = greet("world")
echo result
"#,
            10,
            5,
        ),
        (
            "alpha050",
            AmberVersion::Alpha050,
            r#"
fun greet(name) {
    echo "Hello {name}"
}

fun goodbye(name) {
    echo "Bye {name}"
}

let result = greet("world")
echo result
"#,
            10,
            5,
        ),
    ];

    for (name, amber_version, source, cursor_line, cursor_char) in &autocomplete_sources {
        let amber_version = amber_version.clone();
        let source = source.to_string();
        let cursor_line = *cursor_line;
        let cursor_char = *cursor_char;

        group.bench_function(BenchmarkId::new(*name, "completion"), |b| {
            b.iter(|| {
                let runtime = rt();
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
