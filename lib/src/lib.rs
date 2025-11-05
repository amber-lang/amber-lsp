pub mod amber_version;
pub mod analysis;
pub mod backend;
pub mod files;
pub mod fs;
pub mod grammar;
pub mod paths;
pub mod stdlib;
pub mod utils;

pub use amber_version::{detect_amber_version, AmberVersion, CliAmberVersion};
