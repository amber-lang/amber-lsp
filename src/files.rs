use std::sync::Arc;

use chumsky::span::SimpleSpan;
use ropey::Rope;
use tokio::sync::RwLock;
use tower_lsp::lsp_types::Url;

use crate::{
    analysis::{types::GenericsMap, SymbolTable},
    fs::FS,
    grammar::{Grammar, Spanned, SpannedSemanticToken},
    paths::{FileId, PathInterner},
    utils::FastDashMap,
};

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileVersion(pub i32);

impl From<FileVersion> for i32 {
    fn from(val: FileVersion) -> Self {
        val.0
    }
}

impl FileVersion {
    pub fn prev_n_version(&self, n: i32) -> FileVersion {
        if self.0 - n < 1 {
            return FileVersion(1);
        }

        FileVersion(self.0 - n)
    }
}

#[derive(Debug)]
pub struct Files {
    paths: PathInterner,
    file_versions: FastDashMap<FileId, FileVersion>,
    file_dependencies: FastDashMap<(FileId, FileVersion), Vec<FileId>>,
    pub analyze_lock: FastDashMap<(FileId, FileVersion), Arc<RwLock<bool>>>,
    pub fs: Arc<dyn FS>,
    pub ast_map: FastDashMap<(FileId, FileVersion), Grammar>,
    pub errors: FastDashMap<(FileId, FileVersion), Vec<Spanned<String>>>,
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
            document_map: FastDashMap::default(),
            semantic_token_map: FastDashMap::default(),
            symbol_table: FastDashMap::default(),
            generic_types: GenericsMap::new(),
            analyze_lock: FastDashMap::default(),
        }
    }

    pub fn insert(&self, url: Url, version: FileVersion) -> FileId {
        let file_id = self.paths.insert(url);
        self.add_new_file_version(file_id, version);

        file_id
    }

    pub fn lookup(&self, file_id: &FileId) -> Url {
        self.paths.lookup(file_id)
    }

    pub fn get(&self, url: &Url) -> Option<FileId> {
        self.paths.get(url)
    }

    #[tracing::instrument(skip_all)]
    pub fn add_new_file_version(&self, file_id: FileId, version: FileVersion) {
        if let Some(old_version) = self.file_versions.insert(file_id, version) {
            self.remove_file_version(file_id, old_version);
        }
    }

    #[tracing::instrument(skip_all)]
    fn remove_file_version(&self, file_id: FileId, version: FileVersion) {
        self.ast_map.remove(&(file_id, version));
        self.errors.remove(&(file_id, version));
        self.document_map.remove(&(file_id, version));
        self.semantic_token_map.remove(&(file_id, version));
        self.symbol_table.remove(&(file_id, version));
        self.generic_types.clean(file_id, version);
        self.file_dependencies.remove(&(file_id, version));

        if 10 > version.0 {
            return;
        }
        // Remove the lock from 10 versions ago
        self.analyze_lock
            .remove(&(file_id, version.prev_n_version(10)));
    }

    pub fn change_latest_file_version(&self, file_id: FileId, new_version: FileVersion) {
        let current_versions = self.get_latest_version(file_id);

        if current_versions == new_version {
            return;
        }

        self.ast_map.insert(
            (file_id, new_version),
            self.ast_map.remove(&(file_id, current_versions)).unwrap().1,
        );
        self.errors.insert(
            (file_id, new_version),
            self.errors.remove(&(file_id, current_versions)).unwrap().1,
        );
        self.document_map.insert(
            (file_id, new_version),
            self.document_map
                .remove(&(file_id, current_versions))
                .unwrap()
                .1,
        );
        self.semantic_token_map.insert(
            (file_id, new_version),
            self.semantic_token_map
                .remove(&(file_id, current_versions))
                .unwrap()
                .1,
        );
        self.symbol_table.insert(
            (file_id, new_version),
            self.symbol_table
                .remove(&(file_id, current_versions))
                .unwrap()
                .1,
        );
        self.generic_types.insert(
            file_id,
            new_version,
            self.generic_types
                .get_generics(file_id, current_versions)
                .clone(),
        );
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
        let mut errors = match self.errors.get(file) {
            Some(errors) => errors.clone(),
            None => vec![],
        };
        errors.push((msg.to_string(), span));
        self.errors.insert(*file, errors);
    }

    #[tracing::instrument(skip_all)]
    pub async fn is_file_analyzed(&self, file: &(FileId, FileVersion)) -> bool {
        match self.analyze_lock.get(file) {
            Some(lock) => {
                let result = lock.read().await;

                *result
            }
            None => false,
        }
    }

    pub fn get_files_dependant_on(&self, file_id: FileId) -> Vec<(FileId, FileVersion)> {
        self.file_dependencies
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
            .collect::<Vec<_>>()
    }

    pub fn add_file_dependency(&self, file: &(FileId, FileVersion), dependency: FileId) {
        let mut dependencies = self.file_dependencies.entry(*file).or_insert(vec![]);

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
