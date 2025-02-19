use tower_lsp::lsp_types::Url;

use crate::{backend::Backend, stdlib::resolve};

pub mod exp;
pub mod global;
pub mod stmnts;

#[tracing::instrument(skip_all)]
pub async fn map_import_path(uri: &Url, path: &str, backend: &Backend) -> Url {
    if path == "std" {
        match resolve(backend, "main".to_string()).await {
            Some(path) => {
                return path;
            }
            None => {}
        }
    }

    let path = uri.to_file_path().unwrap().parent().unwrap().join(path);

    Url::from_file_path(path).unwrap()
}
