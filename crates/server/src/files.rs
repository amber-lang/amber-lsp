use std::sync::Arc;

use chumsky::span::SimpleSpan;
use ropey::Rope;
use tokio::sync::RwLock;
use tower_lsp_server::lsp_types::Uri;

use crate::analysis::types::GenericsMap;
use crate::analysis::SymbolTable;
use amber_types::fs::FS;
use crate::grammar::{
    Grammar,
    Spanned,
    SpannedSemanticToken,
};
use amber_types::paths::{
    FileId,
    PathInterner,
};
use amber_types::utils::FastDashMap;
pub use amber_types::file_version::FileVersion;



#[derive(Debug)]
pub struct Files {
    paths: PathInterner,
    file_versions: FastDashMap<FileId, FileVersion>,
    file_dependencies: FastDashMap<(FileId, FileVersion), Vec<FileId>>,
    pub analyze_lock: FastDashMap<(FileId, FileVersion), Arc<RwLock<bool>>>,
    pub fs: Arc<dyn FS>,
    pub ast_map: FastDashMap<(FileId, FileVersion), Grammar>,
    pub errors: FastDashMap<(FileId, FileVersion), Vec<Spanned<String>>>,
    pub warnings: FastDashMap<(FileId, FileVersion), Vec<Spanned<String>>>,
    pub document_map: FastDashMap<(FileId, FileVersion), Rope>,
    pub semantic_token_map: FastDashMap<(FileId, FileVersion), Vec<SpannedSemanticToken>>,
    pub symbol_table: FastDashMap<(FileId, FileVersion), SymbolTable>,
    pub generic_types: GenericsMap,
}

pub const DEFAULT_VERSION: FileVersion = FileVersion(1);

impl Files {
    pub fn new(fs: Arc<dyn FS>) -> Self {
        Files {
            paths: PathInterner::default(),
            fs,
            file_versions: FastDashMap::default(),
            file_dependencies: FastDashMap::default(),
            ast_map: FastDashMap::default(),
            errors: FastDashMap::default(),
            warnings: FastDashMap::default(),
            document_map: FastDashMap::default(),
            semantic_token_map: FastDashMap::default(),
            symbol_table: FastDashMap::default(),
            generic_types: GenericsMap::new(),
            analyze_lock: FastDashMap::default(),
        }
    }

    pub fn insert(&self, uri: Uri, version: FileVersion) -> FileId {
        let file_id = self.paths.insert(uri);
        self.add_new_file_version(file_id, version);

        file_id
    }

    pub fn lookup(&self, file_id: &FileId) -> Uri {
        self.paths.lookup(file_id)
    }

    pub fn get(&self, uri: &Uri) -> Option<FileId> {
        self.paths.get(uri)
    }

    #[tracing::instrument(skip_all)]
    pub fn add_new_file_version(&self, file_id: FileId, version: FileVersion) {
        if let Some(old_version) = self.file_versions.insert(file_id, version) {
            // The threshold value of 50 is used to limit the number of file versions retained.
            // Versions older than 50 are considered outdated and are removed to optimize memory usage.
            if old_version.0 < 50 {
                return;
            }

            self.remove_file_version(file_id, old_version.prev_n_version(50));
        }
    }

    #[tracing::instrument(skip_all)]
    fn remove_file_version(&self, file_id: FileId, version: FileVersion) {
        self.ast_map.remove(&(file_id, version));
        self.errors.remove(&(file_id, version));
        self.warnings.remove(&(file_id, version));
        self.document_map.remove(&(file_id, version));
        self.semantic_token_map.remove(&(file_id, version));
        self.symbol_table.remove(&(file_id, version));
        self.generic_types.clean(file_id, version);
        self.file_dependencies.remove(&(file_id, version));
    }

    pub fn get_latest_version(&self, file_id: FileId) -> FileVersion {
        *self.file_versions.get(&file_id).unwrap()
    }

    pub fn get_document_latest_version(&self, file_id: FileId) -> Option<(Rope, FileVersion)> {
        let file_version = self.get_latest_version(file_id);

        self.document_map
            .get(&(file_id, file_version))
            .map(|document| (document.clone(), file_version))
    }

    pub fn report_error(&self, file: &(FileId, FileVersion), msg: &str, span: SimpleSpan) {
        let mut errors = self.errors.entry(*file).or_default();
        errors.push((msg.to_string(), span));
    }

    pub fn report_warning(&self, file: &(FileId, FileVersion), msg: &str, span: SimpleSpan) {
        let mut warnings = self.warnings.entry(*file).or_default();
        warnings.push((msg.to_string(), span));
    }

    #[tracing::instrument(skip_all)]
    pub async fn is_file_analyzed(&self, file: &(FileId, FileVersion)) -> bool {
        match self.analyze_lock.get(file).map(|lock| lock.clone()) {
            Some(lock) => {
                let result = lock.read().await;

                *result
            }
            None => false,
        }
    }

    pub fn get_files_dependant_on(&self, file_id: FileId) -> Vec<(FileId, FileVersion)> {
        let mut dependant_files = self
            .file_dependencies
            .iter()
            .filter_map(|file_ref| {
                let file = file_ref.key();
                let file_deps = file_ref.value();

                if file_deps.contains(&file_id) {
                    Some(*file)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        dependant_files.dedup_by_key(|(file_id, _)| *file_id);

        dependant_files
    }

    pub fn add_file_dependency(&self, file: &(FileId, FileVersion), dependency: FileId) {
        let mut dependencies = self.file_dependencies.entry(*file).or_default();

        dependencies.push(dependency);
    }

    pub fn is_depending_on(&self, file: &(FileId, FileVersion), dependency: FileId) -> bool {
        match self.file_dependencies.get(file) {
            Some(deps) => deps.iter().any(|dep| {
                *dep == dependency
                    || self.is_depending_on(&(*dep, self.get_latest_version(*dep)), dependency)
            }),
            None => false,
        }
    }
}
