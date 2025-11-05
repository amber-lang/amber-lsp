use clap::{builder::PossibleValue, ValueEnum};
use std::process::{Command, Stdio};

/// The version of amber programming language that the amber source is compiliable in.
#[derive(Clone, Debug, PartialEq)]
pub enum AmberVersion {
    Alpha034,
    Alpha035,
    Alpha040,
}

/// Used for clap parsing.
/// Allows for the user to manually override the detected amber version.
#[derive(Clone, Debug, PartialEq)]
pub enum CliAmberVersion {
    Auto,
    Alpha034,
    Alpha035,
    Alpha040,
}

impl From<CliAmberVersion> for AmberVersion {
    fn from(val: CliAmberVersion) -> Self {
        match val {
            CliAmberVersion::Auto => AmberVersion::Alpha034,
            CliAmberVersion::Alpha034 => AmberVersion::Alpha034,
            CliAmberVersion::Alpha035 => AmberVersion::Alpha035,
            CliAmberVersion::Alpha040 => AmberVersion::Alpha040,
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
        ]
    }

    fn to_possible_value(&self) -> Option<PossibleValue> {
        match self {
            CliAmberVersion::Auto => Some(PossibleValue::new("auto")),
            CliAmberVersion::Alpha034 => Some(PossibleValue::new("0.3.4-alpha")),
            CliAmberVersion::Alpha035 => Some(PossibleValue::new("0.3.5-alpha")),
            CliAmberVersion::Alpha040 => Some(PossibleValue::new("0.4.0-alpha")),
        }
    }
}

/// Detects the amber version by invoking the amber command.
#[tracing::instrument(skip_all)]
pub fn detect_amber_version() -> AmberVersion {
    let output = Command::new("amber")
        .arg("-V")
        .stdout(Stdio::piped())
        .output();

    let version = match output {
        Ok(output) => String::from_utf8_lossy(&output.stdout).to_string(),
        Err(e) => {
            tracing::error!("Failed to execute amber command: {}", e);
            return AmberVersion::Alpha040; // Default to the latest version if detection fails
        }
    };

    match version.split_whitespace().last() {
        Some("0.3.4-alpha") => AmberVersion::Alpha034,
        Some("0.3.5-alpha") => AmberVersion::Alpha035,
        Some("0.4.0-alpha") => AmberVersion::Alpha040,
        _ => AmberVersion::Alpha040,
    }
}
