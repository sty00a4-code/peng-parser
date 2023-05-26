pub mod interpreter;
pub mod value;

use interpreter::*;
use value::*;

use crate::{location::path::FilePath, compiler::bytecode::*, error::Error};

pub fn run(path: &FilePath, code: Code) -> Result<Option<Value>, Error> {
    let mut interpreter = Interpreter::new(path.clone(), code);
    interpreter.run()
}