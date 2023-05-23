use crate::{location::path::FilePath, error::Error, lexer::lexer::Line};

pub mod parser;
pub mod ast;

use parser::Parser;
use ast::*;

pub fn parse(path: &FilePath, tokens: Vec<Line>) -> Result<Chunk, Error> {
    Parser::new(path.clone(), tokens).parse()
}