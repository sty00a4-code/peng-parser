use std::fmt::format;

use crate::{location::position::Located, error::Error, lexer::token::Token};

use super::parser::*;

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ID(String);
impl Parsable for ID {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error> {
        let Located { item: token, pos } = parser.token_checked()?;
        if let Token::ID(id) = token {
            Ok(Located::new(ID(id), pos))
        } else {
            Err(Error::new(format!("expected {}, got {}", Token::ID("".into()).name(), token.name()), parser.path.clone(), Some(pos)))
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter(Located<ID>, Located<TypeExpression>);
impl Parsable for Parameter {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error> {
        let id = ID::parse(parser)?;
        let mut pos = id.pos.clone();
        parser.token_expect(Token::Represent)?;
        let typ = TypeExpression::parse(parser)?;
        pos.extend(&typ.pos);
        Ok(Located::new(Self(id, typ), pos))
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct ObjectEntry(Located<ID>, Located<Expression>);
impl Parsable for ObjectEntry {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error> {
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
        let Located { item: id, mut pos } = ID::parse(parser)?;
        let mut head = Located::new(Self::ID(id), pos);
        Ok(head)
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Path(Located<Path>), Int(i64), Float(f64), Bool(bool), Char(char), String(String),
    Expression(Box<Located<Expression>>),
    Vector(Vec<Located<Expression>>), Object(Vec<ObjectEntry>)
}
impl Parsable for Atom {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error> {
        let Located { item: token, pos: _ } = parser.token_ref_checked()?;
        if let Token::ID(_) = token {
            let path = Path::parse(parser)?;
            let pos = path.pos.clone();
            return Ok(Located::new(Self::Path(path), pos))
        }
        let Located { item: token, mut pos } = parser.token_checked()?;
        match token {
            Token::Int(v) => Ok(Located::new(Self::Int(v), pos)),
            Token::Float(v) => Ok(Located::new(Self::Float(v), pos)),
            Token::Bool(v) => Ok(Located::new(Self::Bool(v), pos)),
            Token::Char(v) => Ok(Located::new(Self::Char(v), pos)),
            Token::String(v) => Ok(Located::new(Self::String(v), pos)),
            Token::ExprIn => {
                let expr = Expression::parse(parser)?;
                let Located { item: _, pos: end_pos } = parser.token_expect(Token::ExprOut)?;
                pos.extend(&end_pos);
                Ok(Located::new(Self::Expression(Box::new(expr)), pos))
            }
            Token::IndexIn => todo!(),
            Token::ObjIn => todo!(),
            token => Err(Error::new(format!("unexpected {}", token.name()), parser.path.clone(), Some(pos)))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    Add, Sub, Mul, Div, Mod, Pow,
    EQ, NE, LT, GT, LE, GE,
    And, Or
}
static  BINARY_LAYERS: &[&[BinaryOperator]] = &[
    &[BinaryOperator::And, BinaryOperator::Or],
    &[BinaryOperator::EQ, BinaryOperator::NE, BinaryOperator::LT, BinaryOperator::GT, BinaryOperator::LE, BinaryOperator::GE],
    &[BinaryOperator::Add, BinaryOperator::Sub],
    &[BinaryOperator::Mul, BinaryOperator::Div, BinaryOperator::Mod],
    &[BinaryOperator::Pow],
];
impl BinaryOperator {
    pub fn token(token: Token) -> Option<Self> {
        match token {
            _ => None
        }
    }
    pub fn layer(layer: usize) -> Option<&'static [Self]> {
        BINARY_LAYERS.get(layer).copied()
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryLeftOperator {
    Neg, Not
}
static UNARY_LEFT_LAYERS: &[&[UnaryLeftOperator]] = &[
    &[UnaryLeftOperator::Not],
    &[UnaryLeftOperator::Neg],
];
impl UnaryLeftOperator {
    pub fn token(token: Token) -> Option<Self> {
        match token {
            _ => None
        }
    }
    pub fn layer(layer: usize) -> Option<&'static [Self]> {
        UNARY_LEFT_LAYERS.get(layer).copied()
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryRightOperator {
    // ?
}
static UNARY_RIGHT_LAYERS: &[&[UnaryRightOperator]] = &[
];
impl UnaryRightOperator {
    pub fn token(token: Token) -> Option<Self> {
        match token {
            _ => None
        }
    }
    pub fn layer(layer: usize) -> Option<&'static [Self]> {
        UNARY_RIGHT_LAYERS.get(layer).copied()
    }
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
        let atom = Atom::parse(parser)?;
        let pos = atom.pos.clone();
        Ok(Located::new(Self::Atom(atom), pos))
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpression {
    Type(ID), Sub(Box<Located<Self>>, Vec<Located<Self>>),
}
impl Parsable for TypeExpression {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error> {
        let Located { item: id, mut pos } = ID::parse(parser)?;
        let mut typ = Located::new(Self::Type(id), pos.clone());
        if let Some(Located { item: Token::IndexIn, pos: _ }) = parser.token_ref() {
            while let Some(Located { item: Token::IndexIn, pos: _ }) = parser.token_ref() {
                parser.token_checked()?;
                let mut subs = vec![];
                while let Some(Located { item: token, pos: _ }) = parser.token_ref() {
                    subs.push(Self::parse(parser)?);
                    let Located { item: token, pos: end_pos } = parser.token_expects(&[Token::Seperate, Token::IndexOut])?;
                    if token == Token::IndexOut {
                        pos.extend(&end_pos);
                        break;
                    }
                }
                typ = Located::new(Self::Sub(Box::new(typ), subs), pos.clone())
            }
        }
        Ok(typ)
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Arguments(Vec<Located<Expression>>);
impl Parsable for Arguments {
    fn parse(parser: &mut Parser) -> Result<Located<Self>, Error> {
        let mut args = vec![];
        let expr = Expression::parse(parser)?;
        let mut pos = expr.pos.clone();
        args.push(expr);
        while let Some(Located { item: Token::Seperate, pos: _ }) = parser.token_ref() {
            parser.token_checked()?;
            let expr = Expression::parse(parser)?;
            pos.extend(&expr.pos);
            args.push(expr);
        }
        Ok(Located::new(Self(args), pos))
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
}
#[derive(Debug, Clone, PartialEq)]
pub enum Statment {
    Variable(Located<Path>, Option<Located<TypeExpression>>, Located<Expression>),
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
                let Located { item: token, pos: mut args_pos } = parser.token_expects(&[Token::Equal, Token::Represent, Token::ExprIn])?;
                match token {
                    Token::Represent => {
                        if let Located { item: Path::ID(_), pos: _ } = &path {
                            if let Some(Located { item: Token::Equal, pos: _ }) = parser.token_ref() {
                                parser.token_checked()?;
                                let expr = Expression::parse(parser)?;
                                pos.extend(&expr.pos);
                                return Ok(Located::new(Self::Variable(path, None, expr), pos))
                            }
                            let typ = TypeExpression::parse(parser)?;
                            parser.token_expect(Token::Equal)?;
                            let expr = Expression::parse(parser)?;
                            pos.extend(&expr.pos);
                            Ok(Located::new(Self::Variable(path, Some(typ), expr), pos))
                        } else {
                            return Err(Error::new(format!("expected {}", Token::ID("".into()).name()), parser.path.clone(), Some(pos)))
                        }
                    }
                    Token::Equal => {
                        let expr = Expression::parse(parser)?;
                        pos.extend(&expr.pos);
                        Ok(Located::new(Self::Assign(path, expr), pos))
                    }
                    Token::ExprIn => {
                        let mut args = vec![];
                        while let Some(Located { item: token, pos: _ }) = parser.token_ref() {
                            if token == &Token::ExprOut { break }
                            args.push(Expression::parse(parser)?)
                        }
                        let Located { item: _, pos: args_end_pos } = parser.token_expect(Token::ExprOut)?;
                        args_pos.extend(&args_end_pos);
                        let args = Located::new(Arguments(args), args_pos);
                        Ok(Located::new(Self::Call(path, args), pos))
                    }
                    _ => panic!()
                }
            }
            token => Err(Error::new(format!("unexpected {}", token.name()), parser.path.clone(), Some(pos.clone())))
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Block(Vec<Located<Statment>>);
#[derive(Debug, Clone, PartialEq)]
pub struct Chunk(pub Vec<Located<Statment>>);