use crate::{location::{path::FilePath, position::Located}, error::Error, lexer::token::Token};

pub mod parser;
pub mod ast;

use parser::Parser;
use ast::*;

pub fn parse(path: &FilePath, tokens: Vec<Vec<Located<Token>>>) -> Result<Chunk, Error> {
    Parser::new(path.clone(), tokens).parse()
}