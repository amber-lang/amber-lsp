use std::sync::Arc;

use chumsky::span::SimpleSpan;
use ropey::Rope;
use tower_lsp::lsp_types::Url;

use crate::{
    analysis::{types::GenericsMap, SymbolTable},
    fs::FS,
    grammar::{Grammar, Spanned, SpannedSemanticToken},
    paths::{FileId, PathInterner},
    utils::{FastDashMap, FastDashSet},
};

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileVersion(pub i32);

impl Into<i32> for FileVersion {
    fn into(self) -> i32 {
        self.0
    }
}

#[derive(Debug)]
pub struct Files {
    paths: PathInterner,
    file_versions: FastDashMap<FileId, FileVersion>,
    analyzed_files: FastDashSet<(FileId, FileVersion)>,
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
            ast_map: FastDashMap::default(),
            errors: FastDashMap::default(),
            document_map: FastDashMap::default(),
            semantic_token_map: FastDashMap::default(),
            symbol_table: FastDashMap::default(),
            generic_types: GenericsMap::new(),
            analyzed_files: FastDashSet::default(),
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

    pub fn add_new_file_version(&self, file_id: FileId, version: FileVersion) {
        match self.file_versions.insert(file_id, version) {
            Some(old_version) => {
                self.remove_file_version(file_id, old_version);
            }
            None => {}
        }
    }

    fn remove_file_version(&self, file_id: FileId, version: FileVersion) {
        self.ast_map.remove(&(file_id, version));
        self.errors.remove(&(file_id, version));
        self.document_map.remove(&(file_id, version));
        self.semantic_token_map.remove(&(file_id, version));
        self.symbol_table.remove(&(file_id, version));
        self.generic_types.clean(file_id, version);
        self.analyzed_files.remove(&(file_id, version));
    }

    pub fn add_new_file_default(&self, file_id: FileId) {
        self.add_new_file_version(file_id, DEFAULT_VERSION);
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
        self.analyzed_files.insert((file_id, new_version));
    }

    pub fn get_latest_version(&self, file_id: FileId) -> FileVersion {
        self.file_versions.get(&file_id).unwrap().clone()
    }

    pub fn get_document_latest_version(&self, file_id: FileId) -> Option<(Rope, FileVersion)> {
        let file_version = self.get_latest_version(file_id);

        match self.document_map.get(&(file_id, file_version)) {
            Some(document) => Some((document.clone(), file_version)),
            None => None,
        }
    }

    pub fn report_error(&self, file: &(FileId, FileVersion), msg: &str, span: SimpleSpan) {
        let mut errors = match self.errors.get(file) {
            Some(errors) => errors.clone(),
            None => vec![],
        };
        errors.push((msg.to_string(), span));
        self.errors.insert(*file, errors);
    }

    pub fn mark_as_analyzed(&self, file: &(FileId, FileVersion)) {
        self.analyzed_files.insert(*file);
    }

    pub fn is_analyzed(&self, file: &(FileId, FileVersion)) -> bool {
        self.analyzed_files.contains(file)
    }
}
