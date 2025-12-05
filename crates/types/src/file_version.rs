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
