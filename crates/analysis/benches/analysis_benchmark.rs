//! Benchmarks for the analysis pipeline.
//!
//! Compares parsing, pure analysis, and Salsa-based incremental analysis
//! on realistic stdlib-size files.
//!
//! Run with: `cargo bench -p amber_analysis`

use std::collections::HashMap;

use criterion::{
    black_box,
    criterion_group,
    criterion_main,
    BenchmarkId,
    Criterion,
};

use amber_analysis::alpha050::pure_analysis::{
    analyze_global_pure,
    file_key_for_path,
    ResolvedImport,
};
use amber_analysis::alpha050::salsa_db::{
    analyze_file,
    read_stdlib_content,
    tokenize_and_parse,
    AnalysisDatabase,
    FileIndex,
    SourceFile,
};
use amber_analysis::SymbolTable;
use amber_grammar::alpha050::GlobalStatement;
use amber_grammar::{
    Grammar,
    Spanned,
};
use amber_types::{
    AmberVersion,
    LocalGenericsAllocator,
};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Load a stdlib file's content.
fn stdlib_content(relative_path: &str) -> String {
    read_stdlib_content(AmberVersion::Alpha050, relative_path)
        .unwrap_or_else(|| panic!("stdlib file not found: {relative_path}"))
}

/// Build a large synthetic file by concatenating all stdlib sources.
fn all_stdlib_combined() -> String {
    let files = [
        "std/text.ab",
        "std/fs.ab",
        "std/env.ab",
        "std/array.ab",
        "std/date.ab",
        "std/math.ab",
        "std/http.ab",
    ];
    let mut combined = String::new();
    for f in &files {
        combined.push_str(&stdlib_content(f));
        combined.push('\n');
    }
    combined
}

/// Parse a file and extract Alpha050 AST statements.
fn parse_ast(text: &str) -> Vec<Spanned<GlobalStatement>> {
    let output = tokenize_and_parse(text, AmberVersion::Alpha050);
    match output.ast {
        Grammar::Alpha050(Some(stmts)) => stmts,
        _ => vec![],
    }
}

/// Extra function appended to a file for incremental re-analysis benchmarks.
const EXTRA_FUNCTION: &str = r#"
pub fun benchmark_extra_function(a, b, c) {
    let result = a + b
    if result > c {
        return result - c
    } else {
        return c - result
    }
}
"#;

// ---------------------------------------------------------------------------
// Setup helpers for Salsa benchmarks
// ---------------------------------------------------------------------------

/// Create a Salsa database with stdlib pre-registered and a user file.
fn make_salsa_db(code: &str) -> (AnalysisDatabase, SourceFile, FileIndex) {
    let mut db = AnalysisDatabase::new();
    let fi = db.new_file_index();
    db.register_stdlib(AmberVersion::Alpha050, fi);

    let sf = SourceFile::new(
        &db,
        "/bench/main.ab".to_string(),
        code.to_string(),
        1,
        AmberVersion::Alpha050,
    );
    db.update_file_index(fi, "/bench/main.ab".to_string(), sf);
    (db, sf, fi)
}

/// Build a pre-resolved import map with an empty builtin (for pure analysis).
fn make_builtin_import() -> HashMap<String, ResolvedImport> {
    let mut map = HashMap::new();
    map.insert(
        "builtin".to_string(),
        ResolvedImport {
            symbol_table: SymbolTable::default(),
            file_key: file_key_for_path("builtin.ab"),
        },
    );
    map
}

// ---------------------------------------------------------------------------
// Benchmarks
// ---------------------------------------------------------------------------

fn bench_parse(c: &mut Criterion) {
    let text_ab = stdlib_content("std/text.ab");
    let all_stdlib = all_stdlib_combined();

    let mut group = c.benchmark_group("parse");

    group.bench_with_input(
        BenchmarkId::new("tokenize_and_parse", "std/text.ab (312 lines)"),
        &text_ab,
        |b, code| {
            b.iter(|| tokenize_and_parse(black_box(code), AmberVersion::Alpha050));
        },
    );

    group.bench_with_input(
        BenchmarkId::new("tokenize_and_parse", "all_stdlib (~960 lines)"),
        &all_stdlib,
        |b, code| {
            b.iter(|| tokenize_and_parse(black_box(code), AmberVersion::Alpha050));
        },
    );

    group.finish();
}

fn bench_pure_analysis(c: &mut Criterion) {
    let text_ab = stdlib_content("std/text.ab");
    let all_stdlib = all_stdlib_combined();

    let text_ast = parse_ast(&text_ab);
    let all_ast = parse_ast(&all_stdlib);

    let import_resolution = make_builtin_import();
    let file_key = file_key_for_path("/bench/main.ab");

    let mut group = c.benchmark_group("pure_analysis");

    group.bench_function("std/text.ab (312 lines)", |b| {
        b.iter(|| {
            analyze_global_pure(
                black_box(file_key),
                black_box(&text_ast),
                &AmberVersion::Alpha050,
                &import_resolution,
                true, // treat as builtin to skip implicit import
                LocalGenericsAllocator::new(42),
            )
        });
    });

    group.bench_function("all_stdlib (~960 lines)", |b| {
        b.iter(|| {
            analyze_global_pure(
                black_box(file_key),
                black_box(&all_ast),
                &AmberVersion::Alpha050,
                &import_resolution,
                true,
                LocalGenericsAllocator::new(42),
            )
        });
    });

    group.finish();
}

fn bench_salsa_cold(c: &mut Criterion) {
    let text_ab = stdlib_content("std/text.ab");
    let all_stdlib = all_stdlib_combined();

    let mut group = c.benchmark_group("salsa_cold_analysis");

    group.bench_function("std/text.ab (312 lines)", |b| {
        b.iter_with_setup(
            || make_salsa_db(&text_ab),
            |(db, sf, fi)| {
                let result = analyze_file(&db, sf, fi);
                black_box(result);
            },
        );
    });

    group.bench_function("all_stdlib (~960 lines)", |b| {
        b.iter_with_setup(
            || make_salsa_db(&all_stdlib),
            |(db, sf, fi)| {
                let result = analyze_file(&db, sf, fi);
                black_box(result);
            },
        );
    });

    group.finish();
}

fn bench_salsa_incremental(c: &mut Criterion) {
    let text_ab = stdlib_content("std/text.ab");
    let all_stdlib = all_stdlib_combined();

    let mut group = c.benchmark_group("salsa_incremental_reanalysis");

    // Benchmark: modify the file (append a function) and re-analyze.
    // Setup performs the cold analysis; the measured portion is the re-analysis.
    group.bench_function("std/text.ab + extra fn", |b| {
        b.iter_with_setup(
            || {
                let (mut db, sf, fi) = make_salsa_db(&text_ab);
                // Cold analysis
                let _ = analyze_file(&db, sf, fi);
                // Modify the file
                let modified = format!("{}\n{}", &text_ab, EXTRA_FUNCTION);
                {
                    use salsa::Setter;
                    sf.set_text(&mut db).to(modified);
                    sf.set_version(&mut db).to(2);
                }
                (db, sf, fi)
            },
            |(db, sf, fi)| {
                let result = analyze_file(&db, sf, fi);
                black_box(result);
            },
        );
    });

    group.bench_function("all_stdlib + extra fn", |b| {
        b.iter_with_setup(
            || {
                let (mut db, sf, fi) = make_salsa_db(&all_stdlib);
                let _ = analyze_file(&db, sf, fi);
                let modified = format!("{}\n{}", &all_stdlib, EXTRA_FUNCTION);
                {
                    use salsa::Setter;
                    sf.set_text(&mut db).to(modified);
                    sf.set_version(&mut db).to(2);
                }
                (db, sf, fi)
            },
            |(db, sf, fi)| {
                let result = analyze_file(&db, sf, fi);
                black_box(result);
            },
        );
    });

    group.finish();
}

fn bench_end_to_end(c: &mut Criterion) {
    let all_stdlib = all_stdlib_combined();

    let mut group = c.benchmark_group("end_to_end_parse_and_analyze");

    // Pure pipeline: parse + analyze_global_pure (no Salsa overhead)
    group.bench_function("pure: all_stdlib (~960 lines)", |b| {
        let import_resolution = make_builtin_import();
        let file_key = file_key_for_path("/bench/main.ab");
        b.iter(|| {
            let ast = parse_ast(black_box(&all_stdlib));
            analyze_global_pure(
                file_key,
                &ast,
                &AmberVersion::Alpha050,
                &import_resolution,
                true,
                LocalGenericsAllocator::new(42),
            )
        });
    });

    // Salsa pipeline: create db + analyze_file (cold, includes parse_file query)
    group.bench_function("salsa_cold: all_stdlib (~960 lines)", |b| {
        b.iter_with_setup(
            || make_salsa_db(&all_stdlib),
            |(db, sf, fi)| {
                let result = analyze_file(&db, sf, fi);
                black_box(result);
            },
        );
    });

    // Salsa incremental: re-analyze after appending a function
    group.bench_function("salsa_incr: all_stdlib + extra fn", |b| {
        b.iter_with_setup(
            || {
                let (mut db, sf, fi) = make_salsa_db(&all_stdlib);
                let _ = analyze_file(&db, sf, fi);
                let modified = format!("{}\n{}", &all_stdlib, EXTRA_FUNCTION);
                {
                    use salsa::Setter;
                    sf.set_text(&mut db).to(modified);
                    sf.set_version(&mut db).to(2);
                }
                (db, sf, fi)
            },
            |(db, sf, fi)| {
                let result = analyze_file(&db, sf, fi);
                black_box(result);
            },
        );
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_parse,
    bench_pure_analysis,
    bench_salsa_cold,
    bench_salsa_incremental,
    bench_end_to_end,
);
criterion_main!(benches);
