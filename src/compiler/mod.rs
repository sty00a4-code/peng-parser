pub mod bytecode;
pub mod compiler;

use bytecode::*;
use compiler::*;

use crate::{location::path::FilePath, parser::ast::Chunk, error::Error};

pub fn compile(path: &FilePath, ast: Chunk) -> Result<Code, Error> {
    let mut compiler = Compiler::new(path.clone());
    compiler.compile(ast)
}