use std::{
    env::temp_dir,
    process::{
        Command,
        Stdio,
    },
};

use amber_lsp::backend::{
    AmberVersion,
    Backend,
};
use clap::{
    builder::PossibleValue,
    Parser,
    ValueEnum,
};
use tower_lsp_server::{
    LspService,
    Server,
};
use tracing::subscriber;
use tracing_subscriber::fmt::format::FmtSpan;

#[derive(Clone, Debug, PartialEq)]
enum CliAmberVersion {
    Auto,
    Alpha034,
    Alpha035,
    Alpha040,
    Alpha050,
}

impl From<CliAmberVersion> for AmberVersion {
    fn from(val: CliAmberVersion) -> Self {
        match val {
            CliAmberVersion::Auto => AmberVersion::Alpha034,
            CliAmberVersion::Alpha034 => AmberVersion::Alpha034,
            CliAmberVersion::Alpha035 => AmberVersion::Alpha035,
            CliAmberVersion::Alpha040 => AmberVersion::Alpha040,
            CliAmberVersion::Alpha050 => AmberVersion::Alpha050,
        }
    }
}

impl ValueEnum for CliAmberVersion {
    fn value_variants<'a>() -> &'a [CliAmberVersion] {
        &[
            CliAmberVersion::Auto,
            CliAmberVersion::Alpha034,
            CliAmberVersion::Alpha035,
            CliAmberVersion::Alpha040,
            CliAmberVersion::Alpha050,
        ]
    }

    fn to_possible_value(&self) -> Option<PossibleValue> {
        match self {
            CliAmberVersion::Auto => Some(PossibleValue::new("auto")),
            CliAmberVersion::Alpha034 => Some(PossibleValue::new("0.3.4-alpha")),
            CliAmberVersion::Alpha035 => Some(PossibleValue::new("0.3.5-alpha")),
            CliAmberVersion::Alpha040 => Some(PossibleValue::new("0.4.0-alpha")),
            CliAmberVersion::Alpha050 => Some(PossibleValue::new("0.5.0-alpha")),
        }
    }
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Version of the Amber language to use.
    #[arg(value_enum, long, short, default_value = "auto")]
    amber_version: CliAmberVersion,
}

#[tokio::main]
async fn main() {
    let cache_dir = temp_dir().join("amber-lsp");
    let file_appender = tracing_appender::rolling::hourly(cache_dir, "amber-lsp.log");
    let (non_blocking_writer, _guard) = tracing_appender::non_blocking(file_appender);

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
        // log to a file
        .with_writer(non_blocking_writer)
        // Disabled ANSI color codes for better compatibility with some terminals
        .with_ansi(false)
        // Build the subscriber
        .finish();

    // use that subscriber to process traces emitted after this point
    subscriber::set_global_default(subscriber).expect("Could not set global default subscriber");

    let args = Args::parse();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let amber_version = if args.amber_version == CliAmberVersion::Auto {
        detect_amber_version()
    } else {
        args.amber_version.into()
    };

    let (service, socket) = LspService::new(|client| Backend::new(client, amber_version, None));
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[tracing::instrument(skip_all)]
fn detect_amber_version() -> AmberVersion {
    let output = Command::new("amber")
        .arg("-V")
        .stdout(Stdio::piped())
        .output();

    let version = match output {
        Ok(output) => String::from_utf8_lossy(&output.stdout).to_string(),
        Err(e) => {
            tracing::error!("Failed to execute amber command: {}", e);
            return AmberVersion::Alpha050; // Default to the latest version if detection fails
        }
    };

    match version.split_whitespace().last() {
        Some("0.3.4-alpha") => AmberVersion::Alpha034,
        Some("0.3.5-alpha") => AmberVersion::Alpha035,
        Some("0.4.0-alpha") => AmberVersion::Alpha040,
        Some("0.5.0-alpha") => AmberVersion::Alpha050,
        _ => AmberVersion::Alpha050,
    }
}
