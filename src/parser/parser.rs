use crate::{location::{path::FilePath, position::{Located, Position}}, lexer::{token::Token, lexer::Line}, error::Error};
use super::ast::*;

pub trait Parsable where Self: Sized {
    fn parse(parser: &mut Parser, indent: usize) -> Result<Located<Self>, Error>;
}

#[derive(Clone)]
pub struct Parser {
    pub path: FilePath,
    lines: Vec<Line>,
    ln: usize
}
impl Parser {
    pub fn new(path: FilePath, lines: Vec<Line>) -> Self {
        Self { path, lines, ln: 0 }
    }

    pub fn pos(&self) -> Position {
        Position::new(self.ln..self.ln+1, 0..1)
    }
    pub fn next_line(&mut self) {
        if self.lines.len() > 0 { self.lines.remove(0); self.ln += 1 }
    }
    pub fn indent(&self) -> usize {
        self.lines.get(0).and_then(|line| Some(line.indent)).unwrap_or_default()
    }
    pub fn token(&mut self) -> Option<Located<Token>> {
        if self.lines.get(0)?.tokens.len() > 0 { Some(self.lines.get_mut(0)?.tokens.remove(0)) } else { None }
    }
    pub fn token_ref(&self) -> Option<&Located<Token>> {
        self.lines.get(0)?.tokens.get(0)
    }
    pub fn token_checked(&mut self) -> Result<Located<Token>, Error> {
        let Some(token) = self.token() else {
            return Err(Error::new("unexpected end of line", self.path.clone(), Some(Position::new(self.ln..self.ln+1, 0..0))))
        };
        Ok(token)
    }
    pub fn token_ref_checked(&self) -> Result<&Located<Token>, Error> {
        let Some(token) = self.token_ref() else {
            return Err(Error::new("unexpected end of line", self.path.clone(), Some(Position::new(self.ln..self.ln+1, 0..0))))
        };
        Ok(token)
    }
    pub fn token_expect(&mut self, expect: Token) -> Result<Located<Token>, Error> {
        let loc_token = self.token_checked()?;
        if expect != loc_token.item {
            return Err(Error::new(format!("expected {}, got {}", expect.name(), loc_token.item.name()), self.path.clone(), Some(loc_token.pos)))
        }
        Ok(loc_token)
    }
    pub fn token_try_expect(&mut self, expect: Token) -> Option<Located<Token>> {
        let loc_token = self.token()?;
        if expect != loc_token.item {
            return None
        }
        Some(loc_token)
    }
    pub fn token_expects(&mut self, expects: &[Token]) -> Result<Located<Token>, Error> {
        let loc_token = self.token_checked()?;
        if !expects.contains(&loc_token.item) {
            let expects = expects.iter().map(|token| token.name()).collect::<Vec<String>>().join("/");
            return Err(Error::new(format!("expected {}, got {}", expects, loc_token.item.name()), self.path.clone(), Some(loc_token.pos)))
        }
        Ok(loc_token)
    }
    pub fn token_try_expects(&mut self, expects: &[Token]) -> Option<Located<Token>> {
        let loc_token = self.token()?;
        if !expects.contains(&loc_token.item) {
            return None
        }
        Some(loc_token)
    }
    pub fn expect_end(&mut self) -> Result<(), Error> {
        if let Some(Located { item: token, pos }) = self.token() {
            return Err(Error::new(format!("expected end of line, not {}", token.name()), self.path.clone(), Some(pos)))
        }
        self.next_line();
        Ok(())
    }
    pub fn skip_end(&mut self) {
        if self.token_ref().is_none() {
            self.next_line();
        }
    }

    pub fn parse(&mut self) -> Result<Chunk, Error> {
        let mut nodes = vec![];
        if self.token_ref().is_none() {
            while self.lines.len() > 0 {
                self.next_line();
            }
        }
        while self.token_ref().is_some() {
            if self.indent() != 0 {
                return Err(Error::new(format!("unexpected indention, expected none"), self.path.clone(), Some(Position::new(self.ln..self.ln+1, 0..1))))
            }
            nodes.push(Statment::parse(self, self.indent())?);
            if self.token_ref().is_none() {
                while self.lines.len() > 0 {
                    self.next_line();
                }
            }
        }
        Ok(Chunk(nodes))
    }
}