#![allow(dead_code, unused_variables)]
mod arguments;
mod location;
mod error;
mod lexer;
mod parser;

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