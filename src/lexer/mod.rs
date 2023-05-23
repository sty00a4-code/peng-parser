use crate::{location::path::FilePath, error::Error};

pub mod lexer;
pub mod token;

use lexer::*;

pub fn lex(path: &FilePath, text: String) -> Result<Vec<Line>, Error> {
    Lexer::new(path.clone(), text).lex()
}