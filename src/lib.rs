#![allow(dead_code, unused_variables)]

use arguments::Arguments;
use error::Error;
use location::path::FilePath;
use parser::ast::Chunk;
pub mod arguments;
pub mod location;
pub mod error;
pub mod lexer;
pub mod parser;
#[cfg(test)]
mod tests;

pub fn parse(path: FilePath, text: String, arguments: &Arguments) -> Result<Chunk, Error> {
    let tokens = lexer::lex(&path, text)?;
    if arguments.get_flag("tokens") {
        println!("{}", join_debug!(tokens, "\n"));
    }
    let ast = parser::parse(&path, tokens)?;
    if arguments.get_flag("ast") {
        println!("{ast:?}");
    }
    Ok(ast)
}
pub fn parse_file<S: ToString>(path: S, arguments: &Arguments) -> Result<Chunk, Error> {
    let (path, text) = FilePath::open(path.to_string().as_str())?;
    parse(path, text, arguments)
}

#[macro_export]
macro_rules! join {
    ($v:expr, $sep:literal) => {
        $v.iter().map(|x| x.to_string()).collect::<Vec<String>>().join($sep)
    };
}
#[macro_export]
macro_rules! join_debug {
    ($v:expr, $sep:literal) => {
        $v.iter().map(|x| format!("{:?}", x)).collect::<Vec<String>>().join($sep)
    };
}
#[macro_export]
macro_rules! join_enum {
    ($v:expr, $sep1:literal, $sep2:literal) => {
        $v.iter().map(|(k, v)| format!("{}{}{}", k, $sep1, v)).collect::<Vec<String>>().join($sep2)
    };
}
#[macro_export]
macro_rules! join_enum_debug {
    ($v:expr, $sep1:literal, $sep2:literal) => {
        $v.iter().map(|(k, v)| format!("{}{}{:?}", k, $sep1, v)).collect::<Vec<String>>().join($sep2)
    };
}
#[macro_export]
macro_rules! join_into_enum {
    ($v:expr, $sep1:literal, $sep2:literal) => {
        $v.iter().enumerate().map(|(k, v)| format!("{}{}{}", k, $sep1, v)).collect::<Vec<String>>().join($sep2)
    };
}
#[macro_export]
macro_rules! join_into_enum_debug {
    ($v:expr, $sep1:literal, $sep2:literal) => {
        $v.iter().enumerate().map(|(k, v)| format!("{}{}{:?}", k, $sep1, v)).collect::<Vec<String>>().join($sep2)
    };
}