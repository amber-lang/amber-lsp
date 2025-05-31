use std::{
    collections::HashMap,
    fmt::Debug,
    future::Future,
    io::Result,
    path::{Path, PathBuf},
    pin::Pin,
    sync::{Arc, Mutex},
};

use tokio::fs::{create_dir_all, metadata, read_dir, read_to_string, write};

pub trait FS: Sync + Send + Debug {
    fn read<'a>(
        &'a self,
        path: &'a Path,
    ) -> Pin<Box<dyn Future<Output = Result<String>> + Send + 'a>>;
    fn write<'a>(
        &'a self,
        path: &'a Path,
        content: &'a str,
    ) -> Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>;
    fn exists<'a>(&'a self, path: &'a Path) -> Pin<Box<dyn Future<Output = bool> + Send + 'a>>;
    fn read_dir<'a>(
        &'a self,
        path: &'a Path,
    ) -> Pin<Box<dyn Future<Output = Vec<PathBuf>> + Send + 'a>>;
    fn create_dir_all<'a>(
        &'a self,
        path: &'a Path,
    ) -> Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>;
}

#[derive(Debug)]
pub struct MemoryFS {
    files: Arc<Mutex<HashMap<String, String>>>,
}

impl Default for MemoryFS {
    fn default() -> Self {
        Self::new()
    }
}

impl MemoryFS {
    pub fn new() -> Self {
        MemoryFS {
            files: Arc::new(Mutex::new(HashMap::new())),
        }
    }
}

impl FS for MemoryFS {
    fn read<'a>(
        &'a self,
        path: &'a Path,
    ) -> Pin<Box<(dyn Future<Output = Result<String>> + Send + 'a)>> {
        Box::pin(async move {
            let files = self.files.lock().unwrap();
            Ok(files.get(path.to_str().unwrap()).unwrap().clone())
        })
    }

    fn write<'a>(
        &'a self,
        path: &'a Path,
        content: &'a str,
    ) -> Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>> {
        Box::pin(async move {
            let mut files = self.files.lock().unwrap();
            files.insert(path.to_string_lossy().to_string(), content.to_string());

            Ok(())
        })
    }

    fn exists<'a>(&'a self, path: &'a Path) -> Pin<Box<dyn Future<Output = bool> + Send + 'a>> {
        Box::pin(async move {
            let files = self.files.lock().unwrap();
            files.contains_key(path.to_str().unwrap())
        })
    }

    fn read_dir<'a>(
        &'a self,
        path: &'a Path,
    ) -> Pin<Box<dyn Future<Output = Vec<PathBuf>> + Send + 'a>> {
        Box::pin(async move {
            let files = self.files.lock().unwrap();

            let mut entries = Vec::new();
            for (file, _) in files.iter() {
                if file.starts_with(path.to_str().unwrap()) {
                    entries.push(PathBuf::from(file));
                }
            }

            entries
        })
    }

    fn create_dir_all<'a>(
        &'a self,
        _: &'a Path,
    ) -> Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>> {
        Box::pin(async move { Ok(()) })
    }
}

#[derive(Debug)]
pub struct LocalFs {}

impl Default for LocalFs {
    fn default() -> Self {
        Self::new()
    }
}

impl LocalFs {
    pub fn new() -> Self {
        LocalFs {}
    }
}

impl FS for LocalFs {
    fn read<'a>(
        &'a self,
        path: &'a Path,
    ) -> Pin<Box<dyn Future<Output = Result<String>> + Send + 'a>> {
        Box::pin(async move { read_to_string(path).await })
    }

    fn write<'a>(
        &'a self,
        path: &'a Path,
        content: &'a str,
    ) -> Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>> {
        Box::pin(async move { write(path, content).await })
    }

    fn exists<'a>(&'a self, path: &'a Path) -> Pin<Box<dyn Future<Output = bool> + Send + 'a>> {
        Box::pin(async move { metadata(path).await.is_ok() })
    }

    fn read_dir<'a>(
        &'a self,
        path: &'a Path,
    ) -> Pin<Box<dyn Future<Output = Vec<PathBuf>> + Send + 'a>> {
        Box::pin(async move {
            let mut dir = match read_dir(path).await {
                Ok(dir) => dir,
                Err(_) => return Vec::new(),
            };
            let mut entries = Vec::new();

            while let Some(child) = dir.next_entry().await.unwrap_or(None) {
                entries.push(child.path());
            }

            entries
        })
    }

    fn create_dir_all<'a>(
        &'a self,
        path: &'a Path,
    ) -> Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>> {
        Box::pin(async move { create_dir_all(path).await })
    }
}
