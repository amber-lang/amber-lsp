use std::{collections::HashMap, fmt::Debug, fs, io::Result, sync::{Arc, Mutex}};

pub trait FS: Sync + Send + Debug {
    fn read(&self, path: &str) -> Result<String>;
    fn write(&self, path: &str, content: &str) -> Result<()>;
    fn exists(&self, path: &str) -> bool;
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
    fn read(&self, path: &str) -> Result<String> {
        let files = self.files.lock().unwrap();
        Ok(files.get(path).unwrap().clone())
    }

    fn write(&self, path: &str, content: &str) -> Result<()> {
        let mut files = self.files.lock().unwrap();
        files.insert(path.to_string(), content.to_string());

        Ok(())
    }

    fn exists(&self, path: &str) -> bool {
        let files = self.files.lock().unwrap();
        files.contains_key(path)
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
    fn read(&self, path: &str) -> Result<String> {
        fs::read_to_string(path)
    }

    fn write(&self, path: &str, content: &str) -> Result<()> {
        fs::write(path, content)
    }

    fn exists(&self, path: &str) -> bool {
        fs::metadata(path).is_ok()
    }
}
