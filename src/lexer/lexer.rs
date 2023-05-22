use crate::{location::{position::*, path::FilePath}, error::Error};
use super::token::Token;

type LexerResult = Result<Vec<Vec<Located<Token>>>, Error>;

pub struct Lexer {
    pub path: FilePath,
    text: Vec<String>,
    idx: usize, ln: usize, col: usize
}
impl Lexer {
    pub fn new(path: FilePath, text: String) -> Self {
        Self { path, text: text.split("\n").map(|x| x.to_string()).collect(), idx: 0, ln: 0, col: 0 }
    }
    pub fn get(&self) -> Option<char> {
        self.text.get(self.ln)?.get(self.idx..self.idx+1)?.chars().next()
    }
    pub fn pos(&self) -> Position {
        Position::new(self.ln..self.ln+1, self.col..self.col+1)
    }
    pub fn advance(&mut self) {
        if self.get() == Some('\n') {
            self.ln += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
        self.idx += 1;
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
                    Ok(Some(Located::new(Token::Equal, pos)))
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
                c if c.is_alphabetic() => {
                    let mut id = String::from(c);
                    self.advance();
                    while let Some(c) = self.get() {
                        if !c.is_alphabetic() { break; }
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
            while let Some(token) = self.next()? {
                tokens.push(token);
            }
            lines.push(tokens);
            self.ln += 1;
        }
        Ok(lines)
    }
}

