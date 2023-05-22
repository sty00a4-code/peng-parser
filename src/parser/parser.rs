use crate::{location::{path::FilePath, position::{Located, Position}}, lexer::token::Token, error::Error};
use super::ast::*;

pub trait Parsable where Self: Sized {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error>;
    fn try_parse(parser: &mut Parser) -> Option<Located<Self>>;
}

#[derive(Clone)]
pub struct Parser {
    pub path: FilePath,
    tokens: Vec<Vec<Located<Token>>>,
    ln: usize
}
impl Parser {
    pub fn new(path: FilePath, tokens: Vec<Vec<Located<Token>>>) -> Self {
        Self { path, tokens, ln: 0 }
    }

    pub fn next_line(&mut self) {
        if self.tokens.len() > 0 { self.tokens.remove(0); self.ln += 1 }
    }
    pub fn token(&mut self) -> Option<Located<Token>> {
        if self.tokens.get(0)?.len() > 0 { Some(self.tokens.get_mut(0)?.remove(0)) } else { None }
    }
    pub fn token_ref(&self) -> Option<&Located<Token>> {
        self.tokens.get(0)?.get(0)
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

    pub fn parse(&mut self) -> Result<Chunk, Error> {
        let mut nodes = vec![];
        while let Some(statment) = Statment::try_parse(self) {
            nodes.push(statment);
        }
        Ok(Chunk { nodes })
    }
}