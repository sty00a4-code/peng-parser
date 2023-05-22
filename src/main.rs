#![allow(dead_code)]
mod arguments;
mod location;
mod error;
mod lexer;

use std::{env, fs, process::exit, io::{self, Write}};
use arguments::Arguments;
use error::Error;
use location::path::FilePath;

pub fn run(path: FilePath, text: String, arguments: &Arguments) -> Result<(), Error> {
    let tokens = lexer::lex(&path, text)?;
    if arguments.get_flag("tokens") { println!("{}", tokens.iter().map(|x| join!(x, " ")).collect::<Vec<String>>().join("\n")) }
    todo!("run()")
}

fn entry() -> Result<(), Error> {
    let mut arguments = Arguments::new().args(env::args()).map_err(|msg| Error::msg(msg))?;
    if let Some(path) = arguments.next_input() {
        let Ok(text) = fs::read_to_string(&path) else {
            return cant_open_file_error!(path);
        };
        run(FilePath::Path(path), text, &arguments)?;
    } else {
        loop {
            let mut input = String::new();
            print!("> ");
            io::stdout().flush().map_err(|err| Error::msg(err.to_string()))?;
            io::stdin().read_line(&mut input).map_err(|err| Error::msg(err.to_string()))?;
            run(FilePath::Input(input.clone()), input, &arguments)?;
        }
    }
    Ok(())
}

fn main() {
    if let Some(err) = entry().err() {
        eprintln!("ERROR: {err}");
        exit(1);
    }
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