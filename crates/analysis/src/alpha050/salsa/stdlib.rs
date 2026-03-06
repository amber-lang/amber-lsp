//! Stdlib file utilities for the Salsa pipeline.
//!
//! Provides functions to enumerate and read embedded stdlib files,
//! and to pre-register them in the Salsa database.

use std::collections::HashSet;

use amber_types::AmberVersion;

use super::types::{
    FileIndex,
    SourceFile,
};
use super::AnalysisDatabase;

// ---------------------------------------------------------------------------
// Stdlib file listing (pure — reads from embedded include_dir at compile time)
// ---------------------------------------------------------------------------

/// Collect all stdlib file paths for a given Amber version from the embedded resources.
///
/// Returns paths like `"std/array.ab"`, `"std/math.ab"`, `"builtin.ab"`, etc.
pub fn collect_stdlib_paths(amber_version: AmberVersion) -> HashSet<String> {
    use crate::stdlib::STDLIB;

    let version_dir = match amber_version.clone() {
        AmberVersion::Alpha034 => "alpha034",
        AmberVersion::Alpha035 => "alpha035",
        AmberVersion::Alpha040 => "alpha040",
        AmberVersion::Alpha050 => "alpha050",
    };

    let mut paths = HashSet::new();

    if let Some(dir) = STDLIB.get_dir(version_dir) {
        collect_paths_recursive(dir, "", &mut paths);
    }

    paths
}

fn collect_paths_recursive(dir: &include_dir::Dir<'_>, prefix: &str, paths: &mut HashSet<String>) {
    for entry in dir.entries() {
        match entry {
            include_dir::DirEntry::Dir(sub_dir) => {
                let name = sub_dir.path().file_name().unwrap().to_str().unwrap();
                let new_prefix = if prefix.is_empty() {
                    name.to_string()
                } else {
                    format!("{}/{}", prefix, name)
                };
                collect_paths_recursive(sub_dir, &new_prefix, paths);
            }
            include_dir::DirEntry::File(file) => {
                let name = file.path().file_name().unwrap().to_str().unwrap();
                let full_path = if prefix.is_empty() {
                    name.to_string()
                } else {
                    format!("{}/{}", prefix, name)
                };
                paths.insert(full_path);
            }
        }
    }
}

/// Read an embedded stdlib file's content by its path relative to the version directory.
///
/// E.g., `read_stdlib_content(Alpha050, "std/array.ab")` returns the contents of
/// `resources/alpha050/std/array.ab`.
pub fn read_stdlib_content(amber_version: AmberVersion, relative_path: &str) -> Option<String> {
    use crate::stdlib::STDLIB;

    let version_dir = match amber_version {
        AmberVersion::Alpha034 => "alpha034",
        AmberVersion::Alpha035 => "alpha035",
        AmberVersion::Alpha040 => "alpha040",
        AmberVersion::Alpha050 => "alpha050",
    };

    let full_path = format!("{}/{}", version_dir, relative_path);
    STDLIB
        .get_file(&full_path)
        .and_then(|f| f.contents_utf8())
        .map(|s| s.to_string())
}

// ---------------------------------------------------------------------------
// Pre-registration helper
// ---------------------------------------------------------------------------

impl AnalysisDatabase {
    /// Pre-register all stdlib files for the given Amber version.
    ///
    /// Creates SourceFile inputs for all stdlib files and registers them
    /// in the provided FileIndex.
    pub fn register_stdlib(
        &mut self,
        amber_version: AmberVersion,
        file_index: FileIndex,
    ) -> Vec<SourceFile> {
        let paths = collect_stdlib_paths(amber_version.clone());
        let mut files = Vec::new();

        for path in paths {
            if let Some(content) = read_stdlib_content(amber_version.clone(), &path) {
                let source = SourceFile::new(
                    self,
                    path.clone(),
                    content,
                    0, // stdlib version is always 0
                    amber_version.clone(),
                );
                self.update_file_index(file_index, path, source);
                files.push(source);
            }
        }

        files
    }
}
