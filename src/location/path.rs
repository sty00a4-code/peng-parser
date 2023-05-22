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
}