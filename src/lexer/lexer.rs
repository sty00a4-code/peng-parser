use std::fmt::Display;

use crate::{location::{position::*, path::FilePath}, error::Error, join};
use super::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct Line {
    pub tokens: Vec<Located<Token>>,
    pub indent: usize
}
impl Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", " ".repeat(self.indent), join!(self.tokens, " "))
    }
}
type LexerResult = Result<Vec<Line>, Error>;

pub struct Lexer {
    pub path: FilePath,
    text: Vec<String>,
    ln: usize, col: usize
}
impl Lexer {
    pub fn new(path: FilePath, text: String) -> Self {
        Self { path, text: text.split("\n").map(|x| x.to_string()).collect(), ln: 0, col: 0 }
    }
    pub fn get(&self) -> Option<char> {
        self.text.get(self.ln)?.get(self.col..self.col+1)?.chars().next()
    }
    pub fn pos(&self) -> Position {
        Position::new(self.ln..self.ln+1, self.col..self.col+1)
    }
    pub fn advance(&mut self) {
        self.col += 1;
    }
    pub fn advance_line(&mut self) {
        self.ln += 1;
        self.col = 0;
    }

    pub fn next(&mut self) -> Result<Option<Located<Token>>, Error> {
        while let Some(c) = self.get() {
            if !c.is_whitespace() { break; }
            self.advance();
        }
        let mut pos = self.pos();
        match self.get() {
            Some(c) => match c {
                '(' => {
                    self.advance();
                    Ok(Some(Located::new(Token::ExprIn, pos)))
                }
                ')' => {
                    self.advance();
                    Ok(Some(Located::new(Token::ExprOut, pos)))
                }
                '[' => {
                    self.advance();
                    Ok(Some(Located::new(Token::IndexIn, pos)))
                }
                ']' => {
                    self.advance();
                    Ok(Some(Located::new(Token::IndexOut, pos)))
                }
                '{' => {
                    self.advance();
                    Ok(Some(Located::new(Token::ObjIn, pos)))
                }
                '}' => {
                    self.advance();
                    Ok(Some(Located::new(Token::ObjOut, pos)))
                }
                '=' => {
                    self.advance();
                    if self.get() == Some('=') {
                        pos.extend(&self.pos());
                        self.advance();
                        return Ok(Some(Located::new(Token::EQ, pos)))
                    }
                    Ok(Some(Located::new(Token::Equal, pos)))
                }
                ',' => {
                    self.advance();
                    Ok(Some(Located::new(Token::Seperate, pos)))
                }
                ':' => {
                    self.advance();
                    Ok(Some(Located::new(Token::Represent, pos)))
                }
                '.' => {
                    self.advance();
                    Ok(Some(Located::new(Token::Field, pos)))
                }
                '+' => {
                    self.advance();
                    if self.get() == Some('=') {
                        pos.extend(&self.pos());
                        self.advance();
                        return Ok(Some(Located::new(Token::AddEqual, pos)))
                    }
                    Ok(Some(Located::new(Token::Add, pos)))
                }
                '-' => {
                    self.advance();
                    if self.get() == Some('=') {
                        pos.extend(&self.pos());
                        self.advance();
                        return Ok(Some(Located::new(Token::SubEqual, pos)))
                    }
                    if self.get() == Some('>') {
                        pos.extend(&self.pos());
                        self.advance();
                        return Ok(Some(Located::new(Token::Out, pos)))
                    }
                    Ok(Some(Located::new(Token::Sub, pos)))
                }
                '*' => {
                    self.advance();
                    if self.get() == Some('=') {
                        pos.extend(&self.pos());
                        self.advance();
                        return Ok(Some(Located::new(Token::MulEqual, pos)))
                    }
                    Ok(Some(Located::new(Token::Mul, pos)))
                }
                '/' => {
                    self.advance();
                    if self.get() == Some('=') {
                        pos.extend(&self.pos());
                        self.advance();
                        return Ok(Some(Located::new(Token::DivEqual, pos)))
                    }
                    Ok(Some(Located::new(Token::Div, pos)))
                }
                '%' => {
                    self.advance();
                    if self.get() == Some('=') {
                        pos.extend(&self.pos());
                        self.advance();
                        return Ok(Some(Located::new(Token::ModEqual, pos)))
                    }
                    Ok(Some(Located::new(Token::Mod, pos)))
                }
                '^' => {
                    self.advance();
                    if self.get() == Some('=') {
                        pos.extend(&self.pos());
                        self.advance();
                        return Ok(Some(Located::new(Token::PowEqual, pos)))
                    }
                    Ok(Some(Located::new(Token::Pow, pos)))
                }
                '!' => {
                    self.advance();
                    if self.get() == Some('=') {
                        pos.extend(&self.pos());
                        self.advance();
                        return Ok(Some(Located::new(Token::NE, pos)))
                    }
                    Ok(Some(Located::new(Token::Not, pos)))
                }
                '<' => {
                    self.advance();
                    if self.get() == Some('=') {
                        pos.extend(&self.pos());
                        self.advance();
                        return Ok(Some(Located::new(Token::EQ, pos)))
                    }
                    Ok(Some(Located::new(Token::LT, pos)))
                }
                '>' => {
                    self.advance();
                    if self.get() == Some('=') {
                        pos.extend(&self.pos());
                        self.advance();
                        return Ok(Some(Located::new(Token::GE, pos)))
                    }
                    Ok(Some(Located::new(Token::GT, pos)))
                }
                '\'' => {
                    self.advance();
                    if let Some(c) = self.get() {
                        self.advance();
                        if self.get() != Some('\'') {
                            return Err(Error::new(format!("unclosed character"), self.path.clone(), Some(self.pos())))
                        }
                        pos.extend(&self.pos());
                        self.advance();
                        Ok(Some(Located::new(Token::Char(c), pos)))
                    } else {
                        Err(Error::new(format!("expected a character, not end of input"), self.path.clone(), Some(self.pos())))
                    }
                }
                '"' => {
                    self.advance();
                    let mut string = String::new();
                    while let Some(c) = self.get() {
                        if c == '"' { break; }
                        string.push(c);
                        self.advance();
                    }
                    if self.get() != Some('"') {
                        return Err(Error::new(format!("unclosed string"), self.path.clone(), Some(self.pos())))
                    }
                    pos.extend(&self.pos());
                    self.advance();
                    Ok(Some(Located::new(Token::String(string), pos)))
                }
                c if c.is_digit(10) => {
                    let mut number = String::from(c);
                    self.advance();
                    while let Some(c) = self.get() {
                        if !c.is_digit(10) { break; }
                        number.push(c);
                        pos.extend(&self.pos());
                        self.advance();
                    }
                    if self.get() == Some('.') {
                        number.push('.');
                        pos.extend(&self.pos());
                        self.advance();
                        while let Some(c) = self.get() {
                            if !c.is_digit(10) { break; }
                            number.push(c);
                            pos.extend(&self.pos());
                            self.advance();
                        }
                        match number.parse() {
                            Ok(number) => Ok(Some(Located::new(Token::Float(number), pos))),
                            Err(err) => Err(Error::new(format!("error while parsing number {number:?}: {err}"), self.path.clone(), Some(pos)))
                        }
                    } else {
                        match number.parse() {
                            Ok(number) => Ok(Some(Located::new(Token::Int(number), pos))),
                            Err(err) => Err(Error::new(format!("error while parsing number {number:?}: {err}"), self.path.clone(), Some(pos)))
                        }
                    }
                }
                c if c.is_alphabetic() || c == '_' => {
                    let mut id = String::from(c);
                    self.advance();
                    while let Some(c) = self.get() {
                        if !c.is_alphabetic() && c != '_' { break; }
                        id.push(c);
                        pos.extend(&self.pos());
                        self.advance();
                    }
                    Ok(Some(Located::new(Token::from_id(id), pos)))
                }
                c => Err(Error::new(format!("bad character {:?}", c), self.path.clone(), Some(pos)))
            }
            None => Ok(None)
        }
    }
    pub fn lex(&mut self) -> LexerResult {
        let mut lines = vec![];
        while self.ln < self.text.len() {
            let mut tokens = vec![];
            let mut indent = 0;
            while let Some(c) = self.get() {
                if !c.is_whitespace() { break; }
                indent += 1;
                self.advance();
            }
            while let Some(token) = self.next()? {
                tokens.push(token);
            }
            lines.push(Line { tokens, indent });
            self.advance_line();
        }
        Ok(lines)
    }
}

