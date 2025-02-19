use std::{
    os::unix::process::ExitStatusExt,
    process::{Command, Stdio},
};

use amber_lsp::backend::{AmberVersion, Backend};
use clap::Parser;
use tower_lsp::{LspService, Server};
use tracing::subscriber;
use tracing_subscriber::fmt::format::FmtSpan;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Version of the Amber language to use.
    #[arg(value_enum, long, short, default_value = "auto")]
    amber_version: AmberVersion,
}

#[tokio::main(flavor = "multi_thread", worker_threads = 10)]
async fn main() {
    // construct a subscriber that prints formatted traces to stdout
    let subscriber = tracing_subscriber::fmt()
        // Use a more compact, abbreviated log format
        .compact()
        // Display source code file paths
        .with_file(true)
        // Display source code line numbers
        .with_line_number(true)
        // Don't display the thread ID an event was recorded on
        .with_thread_ids(false)
        // Don't display the event's target (module path)
        .with_target(false)
        // Log when entering and exiting spans
        .with_span_events(FmtSpan::ENTER | FmtSpan::CLOSE)
        // Log to stderr
        // .with_writer(std::io::stderr)
        // log to a file
        .with_writer(std::fs::OpenOptions::new().create(true).append(true).open("amber-lsp.log").unwrap())
        // Disabled ANSI color codes for better compatibility with some terminals
        .with_ansi(false)
        // Build the subscriber
        .finish();

    // use that subscriber to process traces emitted after this point
    subscriber::set_global_default(subscriber).expect("Could not set global default subscriber");

    let args = Args::parse();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let amber_version = if args.amber_version == AmberVersion::Auto {
        detect_amber_version()
    } else {
        args.amber_version
    };

    let (service, socket) = LspService::new(|client| Backend::new(client, amber_version, None));
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[tracing::instrument(skip_all)]
fn detect_amber_version() -> AmberVersion {
    let output = String::from_utf8_lossy(
        Command::new("amber")
            .arg("-V")
            .stdout(Stdio::piped())
            .output()
            .unwrap_or(std::process::Output {
                stdout: Vec::new(),
                stderr: Vec::new(),
                status: std::process::ExitStatus::from_raw(0),
            })
            .stdout
            .as_slice(),
    )
    .to_string();

    match output.split_whitespace().last() {
        Some("0.3.4-alpha") => AmberVersion::Alpha034,
        Some("0.3.5-alpha") => AmberVersion::Alpha035,
        Some("0.4.0-alpha") => AmberVersion::Alpha040,
        _ => AmberVersion::Auto,
    }
}
