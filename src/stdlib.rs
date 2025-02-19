use std::env::temp_dir;

use include_dir::{include_dir, Dir};
use tower_lsp::lsp_types::Url;

use crate::backend::{AmberVersion, Backend};

pub const STDLIB: Dir = include_dir!("$CARGO_MANIFEST_DIR/resources/");

#[tracing::instrument(skip(backend))]
pub async fn resolve(backend: &Backend, path: String) -> Option<Url> {
    let dir = temp_dir();

    let file_name: String = path + ".ab";

    let path = match backend.amber_version {
        AmberVersion::Alpha034 => "alpha034/std/".to_string() + &file_name,
        AmberVersion::Alpha035 => "alpha035/std/".to_string() + &file_name,
        _ => "alpha040/std/".to_string() + &file_name, // TODO: Change default path when resolved issues with parsing
    };

    if let Some(module) = STDLIB.get_file(path.clone()) {
        let tmp_file_path = dir.join(file_name);
        let tmp_file_path = tmp_file_path.to_str().unwrap();

        if backend.files.fs.exists(tmp_file_path).await {
            return Url::from_file_path(tmp_file_path).ok();
        }

        if backend
            .files
            .fs
            .write(tmp_file_path, &module.contents_utf8().unwrap().to_string())
            .await
            .is_err()
        {
            return None;
        }

        return Url::from_file_path(tmp_file_path).ok();
    } else {
        None
    }
}
