use std::fmt::Display;
use crate::location::{position::Position, path::FilePath};

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    msg: String,
    path: FilePath,
    pos: Option<Position>
}
impl Error {
    pub fn new<S: ToString>(msg: S, path: FilePath, pos: Option<Position>) -> Self {
        Self { msg: msg.to_string(), path, pos }
    }
    pub fn msg<S: ToString>(msg: S) -> Self {
        Self { msg: msg.to_string(), path: FilePath::None, pos: None }
    }
}
impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.path.path())?;
        if let Some(pos) = &self.pos {
            write!(f, ":{}:{}", pos.ln.start + 1, pos.col.start + 1)?;
        }
        write!(f, ": ")?;
        write!(f, "{}", self.msg)
    }
}

#[macro_export]
macro_rules! error {
    ($msg:literal, $path:expr, $pos:expr) => {
        Err(self::Error::new(format!($msg), Some($path), Some($pos)))
    };
}
#[macro_export]
macro_rules! cant_open_file_error {
    ($path:expr) => {
        Err(self::Error::msg(format!("couldn't open path {:?}", $path)))
    };
}