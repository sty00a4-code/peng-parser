use crate::{location::position::Located, error::Error, lexer::token::Token};

use super::parser::*;

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ID(String);
impl Parsable for ID {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error> {
        todo!()
    }
    fn try_parse(parser: &mut Parser) -> Option<Located<Self>> {
        todo!()
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter(Located<ID>, Option<Located<TypeExpression>>);
impl Parsable for Parameter {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error> {
        todo!()
    }
    fn try_parse(parser: &mut Parser) -> Option<Located<Self>> {
        todo!()
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct ObjectEntry(Located<ID>, Located<Expression>);
impl Parsable for ObjectEntry {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error> {
        todo!()
    }
    fn try_parse(parser: &mut Parser) -> Option<Located<Self>> {
        todo!()
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Path {
    ID(ID), Field(Box<Located<Self>>, Located<ID>),
    Index(Box<Located<Self>>, Box<Located<Expression>>)
}
impl Parsable for Path {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error> {
        todo!()
    }
    fn try_parse(parser: &mut Parser) -> Option<Located<Self>> {
        todo!()
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Path(Path), Int(i64), Float(f64), Bool(bool), Char(char), String(String),
    Expression(Box<Located<Expression>>),
    Vector(Vec<Located<Expression>>), Object(Vec<ObjectEntry>)
}
impl Parsable for Atom {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error> {
        todo!()
    }
    fn try_parse(parser: &mut Parser) -> Option<Located<Self>> {
        todo!()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    Add, Sub, Mul, Div, Mod, Pow,
    EQ, NE, LT, GT, LE, GE,
    And, Or
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryLeftOperator {
    Neg, Not
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryRightOperator {
    // ?
}
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Atom(Located<Atom>),
    Binary { op: BinaryOperator, left: Box<Located<Self>>, right: Box<Located<Self>> },
    UnaryLeft { op: UnaryLeftOperator, right: Box<Located<Self>> },
    UnaryRight { op: UnaryRightOperator, left: Box<Located<Self>> },
}
impl Parsable for Expression {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error> {
        todo!()
    }
    fn try_parse(parser: &mut Parser) -> Option<Located<Self>> {
        todo!()
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpression {
    Type(ID), Sub(Box<Located<Self>>, Vec<Located<Self>>),
}
impl Parsable for TypeExpression {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error> {
        todo!()
    }
    fn try_parse(parser: &mut Parser) -> Option<Located<Self>> {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct KeyArgument(Located<ID>, Located<Expression>);
impl Parsable for KeyArgument {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error> {
        todo!()
    }
    fn try_parse(parser: &mut Parser) -> Option<Located<Self>> {
        todo!()
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Arguments {
    positional: Vec<Located<Expression>>,
    kwargs: Option<Vec<Located<KeyArgument>>>,
}
impl Parsable for Arguments {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error> {
        todo!()
    }
    fn try_parse(parser: &mut Parser) -> Option<Located<Self>> {
        todo!()
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Parameters {
    positional: Vec<Located<Parameter>>,
    args: Option<Located<Parameter>>,
    kwargs: Option<Located<Parameter>>,
}
impl Parsable for Parameters {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error> {
        todo!()
    }
    fn try_parse(parser: &mut Parser) -> Option<Located<Self>> {
        todo!()
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Statment {
    Assign(Located<Path>, Located<Expression>),
    Call(Located<Path>, Located<Arguments>),
    If(Vec<Located<Expression>>, Vec<Located<Block>>, Option<Located<Block>>),
    While(Located<Expression>, Located<Block>), Repeat(Located<Expression>, Located<Block>),
    For(Located<Parameters>, Located<Expression>, Located<Block>),
}
impl Parsable for Statment {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error> {
        let Located { item: token, pos } = parser.token_ref_checked()?;
        match token {
            Token::ID(_) => {
                let mut pos = pos.clone();
                let path = Path::parse(parser)?;
                let Located { item: token, pos: mut args_pos } = parser.token_expects(&[Token::Equal, Token::ExprIn])?;
                match token {
                    Token::Equal => {
                        let expr = Expression::parse(parser)?;
                        pos.extend(&expr.pos);
                        Ok(Located::new(Statment::Assign(path, expr), pos))
                    }
                    Token::ExprIn => {
                        let mut args = vec![];
                        while let Some(Located { item: token, pos: _ }) = parser.token_ref() {
                            if token == &Token::ExprOut { break }
                            args.push(Expression::parse(parser)?)
                        }
                        let Located { item: _, pos: args_end_pos } = parser.token_expect(Token::ExprOut)?;
                        args_pos.extend(&args_end_pos);
                        let args = Located::new(Arguments { positional: args, kwargs: None }, args_pos);
                        Ok(Located::new(Statment::Call(path, args), pos))
                    }
                    _ => panic!()
                }
            }
            token => Err(Error::new(format!("unexpected {}", token.name()), parser.path.clone(), Some(pos.clone())))
        }
    }
    fn try_parse(parser: &mut Parser) -> Option<Located<Self>> {
        let Located { item: token, pos } = parser.token_ref()?;
        match token {
            Token::ID(_) => {
                let mut pos = pos.clone();
                let path = Path::try_parse(parser)?;
                let Located { item: token, pos: mut args_pos } = parser.token_try_expects(&[Token::Equal, Token::ExprIn])?;
                match token {
                    Token::Equal => {
                        let expr = Expression::try_parse(parser)?;
                        pos.extend(&expr.pos);
                        Some(Located::new(Statment::Assign(path, expr), pos))
                    }
                    Token::ExprIn => {
                        let mut args = vec![];
                        while let Some(Located { item: token, pos: _ }) = parser.token_ref() {
                            if token == &Token::ExprOut { break }
                            args.push(Expression::try_parse(parser)?)
                        }
                        let Located { item: _, pos: args_end_pos } = parser.token_try_expect(Token::ExprOut)?;
                        args_pos.extend(&args_end_pos);
                        let args = Located::new(Arguments { positional: args, kwargs: None }, args_pos);
                        Some(Located::new(Statment::Call(path, args), pos))
                    }
                    _ => panic!()
                }
            }
            _ => None 
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Block(Vec<Located<Statment>>);
#[derive(Debug, Clone, PartialEq)]
pub struct Chunk {
    pub nodes: Vec<Located<Statment>>,
}