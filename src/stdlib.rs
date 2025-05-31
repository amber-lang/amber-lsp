use std::{env::temp_dir, future::Future, path::PathBuf, pin::Pin};

use clap::builder::OsStr;
use include_dir::{include_dir, Dir, DirEntry};
use tower_lsp::lsp_types::Url;
use tracing::info;

use crate::backend::{AmberVersion, Backend};

pub const STDLIB: Dir = include_dir!("$CARGO_MANIFEST_DIR/resources/");

pub fn is_builtin_file(url: &Url) -> bool {
    let cache_dir = temp_dir().join("amber-lsp");

    let file_path = url.to_file_path().unwrap();

    file_path.starts_with(cache_dir) && file_path.ends_with("builtin.ab")
}

#[tracing::instrument(skip(backend))]
async fn save_resources(backend: &Backend) -> PathBuf {
    let cache_dir = temp_dir().join("amber-lsp");

    let path = match backend.amber_version {
        AmberVersion::Alpha034 => "alpha034/".to_string(),
        AmberVersion::Alpha035 => "alpha035/".to_string(),
        AmberVersion::Alpha040 => "alpha040/".to_string(),
    };

    let _ = backend
        .files
        .fs
        .create_dir_all(&cache_dir.join(path.clone()))
        .await;

    if let Some(dir) = STDLIB.get_dir(path.clone()) {
        for entry in dir.entries() {
            save_entry(backend, &cache_dir, entry).await;
        }
    }

    cache_dir.join(path.clone())
}

fn save_entry<'a>(
    backend: &'a Backend,
    current_path: &'a PathBuf,
    entry: &'a DirEntry<'a>,
) -> Pin<Box<dyn Future<Output = ()> + Send + 'a>> {
    Box::pin(async move {
        match entry {
            DirEntry::Dir(dir) => {
                let path = current_path.join(dir.path());

                let _ = backend.files.fs.create_dir_all(&path).await;
                for entry in dir.entries() {
                    save_entry(backend, current_path, entry).await;
                }
            }
            DirEntry::File(file) => {
                let path = current_path.join(file.path());

                if path.exists() {
                    return;
                }

                let contents = file.contents_utf8().unwrap().to_string();

                backend.files.fs.write(&path, &contents).await.unwrap();
            }
        }
    })
}

#[tracing::instrument(skip(backend))]
pub async fn resolve(backend: &Backend, path: String) -> Option<Url> {
    let file_path = path + ".ab";

    let memory_path = match backend.amber_version {
        AmberVersion::Alpha034 => PathBuf::from("alpha034"),
        AmberVersion::Alpha035 => PathBuf::from("alpha035"),
        AmberVersion::Alpha040 => PathBuf::from("alpha040"),
    }
    .join(file_path.clone());

    if !STDLIB.contains(memory_path.clone()) {
        info!(
            "File not found in stdlib: {}",
            memory_path.clone().to_str().unwrap()
        );
        return None;
    }

    let base_path = save_resources(backend).await;

    let file_path = base_path.join(file_path);

    info!("File found in resources: {}", file_path.to_str().unwrap());

    Url::from_file_path(file_path).ok()
}

pub async fn find_in_stdlib(backend: &Backend, path: &str) -> Vec<String> {
    let parts = path.split('/').collect::<Vec<&str>>();

    match backend.amber_version {
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

            let stdlib_dir = save_resources(backend).await;

            let path_in_std = stdlib_dir.clone().join(parts.join("/"));

            backend
                .files
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
