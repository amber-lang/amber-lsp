use std::{
    collections::HashMap,
    fmt::Debug,
    future::Future,
    io::Result,
    pin::Pin,
    sync::{Arc, Mutex},
};

use tokio::fs::{metadata, read_to_string, write};

pub trait FS: Sync + Send + Debug {
    fn read<'a>(
        &'a self,
        path: &'a str,
    ) -> Pin<Box<dyn Future<Output = Result<String>> + Send + 'a>>;
    fn write<'a>(
        &'a self,
        path: &'a str,
        content: &'a str,
    ) -> Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>>;
    fn exists<'a>(&'a self, path: &'a str) -> Pin<Box<dyn Future<Output = bool> + Send + 'a>>;
}

#[derive(Debug)]
pub struct MemoryFS {
    files: Arc<Mutex<HashMap<String, String>>>,
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
        path: &'a str,
    ) -> Pin<Box<(dyn Future<Output = Result<String>> + Send + 'a)>> {
        Box::pin(async move {
            let files = self.files.lock().unwrap();
            Ok(files.get(path).unwrap().clone())
        })
    }

    fn write<'a>(
        &'a self,
        path: &'a str,
        content: &'a str,
    ) -> Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>> {
        Box::pin(async move {
            let mut files = self.files.lock().unwrap();
            files.insert(path.to_string(), content.to_string());

            Ok(())
        })
    }

    fn exists<'a>(&'a self, path: &'a str) -> Pin<Box<dyn Future<Output = bool> + Send + 'a>> {
        Box::pin(async move {
            let files = self.files.lock().unwrap();
            files.contains_key(path)
        })
    }
}

#[derive(Debug)]
pub struct LocalFs {}

impl LocalFs {
    pub fn new() -> Self {
        LocalFs {}
    }
}

impl FS for LocalFs {
    fn read<'a>(
        &'a self,
        path: &'a str,
    ) -> Pin<Box<dyn Future<Output = Result<String>> + Send + 'a>> {
        Box::pin(async move { read_to_string(path).await })
    }

    fn write<'a>(
        &'a self,
        path: &'a str,
        content: &'a str,
    ) -> Pin<Box<dyn Future<Output = Result<()>> + Send + 'a>> {
        Box::pin(async move { write(path, content).await })
    }

    fn exists<'a>(&'a self, path: &'a str) -> Pin<Box<dyn Future<Output = bool> + Send + 'a>> {
        Box::pin(async move { metadata(path).await.is_ok() })
    }
}
