use crate::error::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum FilePath {
    Path(String), Input(String), None
}
impl FilePath {
    pub fn path(&self) -> String {
        match self {
            Self::Path(path) => path.clone(),
            Self::Input(_) => "<input>".into(),
            Self::None => "<none>".into(),
        }
    }
    pub fn content(&self) -> Option<String> {
        match self {
            FilePath::Path(path) => std::fs::read_to_string(path).ok(),
            FilePath::Input(input) => Some(input.clone()),
            FilePath::None => None
        }
    }
    pub fn open(path: &str) -> Result<(Self, String), Error> {
        if path == "<none>" {
            return Ok((Self::None, String::new()))
        }
        let path = std::path::Path::new(path);
        if !path.exists() {
            return Err(Error::new(format!("file not found: {}", path.display()), Self::None, None))
        }
        if !path.is_file() {
            return Err(Error::new(format!("not a file: {}", path.display()), Self::None, None))
        }
        let path = path.canonicalize().unwrap();
        let path = path.to_str().unwrap();
        let content = std::fs::read_to_string(path).unwrap();
        Ok((Self::Path(path.into()), content))
    }
}