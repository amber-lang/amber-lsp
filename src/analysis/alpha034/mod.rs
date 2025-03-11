use tower_lsp::lsp_types::Url;

use crate::{backend::Backend, stdlib::resolve};

pub mod exp;
pub mod global;
pub mod stmnts;

#[tracing::instrument(skip_all)]
pub async fn map_import_path(uri: &Url, path: &str, backend: &Backend) -> Url {
    if path.starts_with("std/") || path == "std" {
        match backend.amber_version {
            _ => {
                if let Some(url) = resolve(backend, "main".to_string()).await {
                    return url;
                }
            } // AmberVersion::Alpha034 if path == "std" => {
              //     if let Some(url) = resolve(backend, "std/main".to_string()).await {
              //         return url;
              //     }
              // }
              // _ => {
              //     if let Some(url) = resolve(backend, path.to_string()).await {
              //         return url;
              //     }
              // }
        }
    }

    let path = uri.to_file_path().unwrap().parent().unwrap().join(path);

    Url::from_file_path(path).unwrap()
}
