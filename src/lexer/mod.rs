use crate::{location::{path::FilePath, position::Located}, error::Error};

pub mod lexer;
pub mod token;

use lexer::Lexer;
use token::Token;

pub fn lex(path: &FilePath, text: String) -> Result<Vec<Located<Token>>, Error> {
    Lexer::new(path.clone(), text).lex()
}