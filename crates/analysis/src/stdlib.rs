use std::env::current_exe;
use std::future::Future;
use std::path::{
    Path,
    PathBuf,
};
use std::pin::Pin;

use clap::builder::OsStr;
use include_dir::{
    include_dir,
    Dir,
    DirEntry,
};
use tower_lsp_server::lsp_types::Uri;
use tower_lsp_server::UriExt;
use tracing::warn;

use crate::AnalysisHost;
use amber_types::AmberVersion;

pub const STDLIB: Dir = include_dir!("$CARGO_MANIFEST_DIR/../../resources/");

/// Environment variable that overrides the base directory where stdlib and
/// builtin resource files are saved.  When set, resources are written to
/// `$AMBER_LSP_RESOURCES_DIR/{alpha_version}/` instead of the default
/// `<exe_dir>/amber-lsp-resources/{alpha_version}/`.
///
/// This is useful on operating systems that sandbox the application and
/// prevent writing next to the executable.
const RESOURCES_DIR_ENV: &str = "AMBER_LSP_RESOURCES_DIR";

fn get_stdlib_dir(amber_version: AmberVersion) -> Result<PathBuf, std::io::Error> {
    let amber_subdir = match amber_version {
        AmberVersion::Alpha034 => "alpha034",
        AmberVersion::Alpha035 => "alpha035",
        AmberVersion::Alpha040 => "alpha040",
        AmberVersion::Alpha050 => "alpha050",
    };

    let base_dir = match std::env::var(RESOURCES_DIR_ENV) {
        Ok(dir) if !dir.is_empty() => PathBuf::from(dir),
        _ => current_exe()?
            .parent()
            .unwrap()
            .to_path_buf()
            .join("amber-lsp-resources"),
    };

    Ok(base_dir.join(amber_subdir))
}

#[tracing::instrument]
pub fn is_builtin_file(uri: &Uri, amber_version: AmberVersion) -> bool {
    let builtin_file_path = match get_stdlib_dir(amber_version) {
        Ok(dir) => dir,
        Err(_) => {
            return false;
        }
    }
    .join("builtin.ab")
    .canonicalize();

    let file_path = match uri.to_file_path() {
        Some(path) => path.canonicalize(),
        None => {
            return false;
        }
    };

    if builtin_file_path.is_err() || file_path.is_err() {
        return true; // For local testing
    }

    file_path.unwrap() == builtin_file_path.unwrap()
}

#[tracing::instrument(skip_all)]
pub async fn save_resources(backend: &impl AnalysisHost) -> Option<PathBuf> {
    let stdlib_dir = match get_stdlib_dir(backend.get_amber_version().clone()) {
        Ok(stdlib_dir) => stdlib_dir,
        Err(_) => {
            backend
                .show_error("Failed to resolve LSP executable path.")
                .await;
            return None;
        }
    };

    let binary_stdlib_dir = match backend.get_amber_version() {
        AmberVersion::Alpha034 => "alpha034/".to_string(),
        AmberVersion::Alpha035 => "alpha035/".to_string(),
        AmberVersion::Alpha040 => "alpha040/".to_string(),
        AmberVersion::Alpha050 => "alpha050/".to_string(),
    };

    if backend
        .get_files()
        .fs
        .create_dir_all(&stdlib_dir)
        .await
        .is_err()
    {
        backend
            .show_error(&format!(
                "Could not save std lib files to {}",
                stdlib_dir.to_string_lossy()
            ))
            .await;
        return None;
    }

    if let Some(dir) = STDLIB.get_dir(binary_stdlib_dir) {
        for entry in dir.entries() {
            save_entry(backend, &stdlib_dir, entry).await;
        }
    }

    Some(stdlib_dir)
}

fn save_entry<'a>(
    backend: &'a impl AnalysisHost,
    current_path: &'a Path,
    entry: &'a DirEntry<'a>,
) -> Pin<Box<dyn Future<Output = ()> + Send + 'a>> {
    Box::pin(async move {
        match entry {
            DirEntry::Dir(dir) => {
                let path = current_path.join(dir.path().file_name().unwrap());

                let _ = backend.get_files().fs.create_dir_all(&path).await;
                for entry in dir.entries() {
                    save_entry(backend, &path, entry).await;
                }
            }
            DirEntry::File(file) => {
                let path = current_path.join(file.path().file_name().unwrap());

                if path.exists() {
                    return;
                }

                let contents = match file.contents_utf8() {
                    Some(c) => c.to_string(),
                    None => {
                        backend
                            .show_error(&format!(
                                "Embedded std lib file at {:?} is not valid UTF-8",
                                path
                            ))
                            .await;
                        return;
                    }
                };

                if backend
                    .get_files()
                    .fs
                    .write(&path, &contents)
                    .await
                    .is_err()
                {
                    backend
                        .show_error(&format!(
                            "Could not save std lib file to {}",
                            path.to_string_lossy()
                        ))
                        .await;
                }
            }
        }
    })
}

#[tracing::instrument(skip_all)]
pub async fn resolve(backend: &impl AnalysisHost, path: String) -> Option<Uri> {
    let file_path = path + ".ab";

    let memory_path = match backend.get_amber_version() {
        AmberVersion::Alpha034 => PathBuf::from("alpha034"),
        AmberVersion::Alpha035 => PathBuf::from("alpha035"),
        AmberVersion::Alpha040 => PathBuf::from("alpha040"),
        AmberVersion::Alpha050 => PathBuf::from("alpha050"),
    }
    .join(file_path.clone());

    if !STDLIB.contains(memory_path.clone()) {
        warn!(
            "File not found in stdlib: {}",
            memory_path.clone().to_str().unwrap()
        );
        return None;
    }

    let base_path = match save_resources(backend).await {
        Some(base_path) => base_path,
        None => return None,
    };

    let file_path = base_path.join(file_path);

    warn!("File found in resources: {}", file_path.to_str().unwrap());

    Uri::from_file_path(file_path)
}

pub async fn find_in_stdlib(backend: &impl AnalysisHost, path: &str) -> Vec<String> {
    let parts = path.split('/').collect::<Vec<&str>>();

    match backend.get_amber_version() {
        AmberVersion::Alpha034 => {
            vec!["std".to_string()]
        }
        _ => {
            if parts.len() <= 1 {
                return vec!["std".to_string()];
            }

            if parts.len() > 1 && parts[0] != "std" {
                return vec![];
            }

            let stdlib_dir = match save_resources(backend).await {
                Some(stdlib_dir) => stdlib_dir,
                None => return vec![],
            };

            let path_in_std = stdlib_dir.clone().join(parts.join("/"));

            backend
                .get_files()
                .fs
                .read_dir(&path_in_std)
                .await
                .iter()
                .filter(|path| path.is_dir() || (path.extension() == Some(&OsStr::from("ab"))))
                .map(|path| {
                    let base_path = path
                        .strip_prefix(stdlib_dir.clone().join("std"))
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .to_string();

                    if path.is_file() {
                        return base_path.strip_suffix(".ab").unwrap().to_string();
                    }

                    base_path
                })
                .collect()
        }
    }
}
