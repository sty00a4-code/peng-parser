use crate::{location::position::{Located, Position}, error::Error, lexer::token::Token};

use super::parser::*;

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ID(String);
impl Parsable for ID {
    fn parse(parser: &mut Parser, indent: usize) -> Result<Located<Self>, Error> {
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
    fn parse(parser: &mut Parser, indent: usize) -> Result<Located<Self>, Error> {
        let id = ID::parse(parser, indent)?;
        let mut pos = id.pos.clone();
        parser.token_expect(Token::Represent)?;
        let typ = TypeExpression::parse(parser, indent)?;
        pos.extend(&typ.pos);
        Ok(Located::new(Self(id, typ), pos))
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct ObjectEntry(Located<ID>, Located<Expression>);
impl Parsable for ObjectEntry {
    fn parse(parser: &mut Parser, indent: usize) -> Result<Located<Self>, Error> {
        todo!()
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Path {
    ID(ID), Field(Box<Located<Self>>, Located<ID>),
    Index(Box<Located<Self>>, Box<Located<Expression>>)
}
impl Parsable for Path {
    fn parse(parser: &mut Parser, indent: usize) -> Result<Located<Self>, Error> {
        let Located { item: id, pos } = ID::parse(parser, indent)?;
        let head = Located::new(Self::ID(id), pos);
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
    fn parse(parser: &mut Parser, indent: usize) -> Result<Located<Self>, Error> {
        let Located { item: token, pos: _ } = parser.token_ref_checked()?;
        if let Token::ID(_) = token {
            let path = Path::parse(parser, indent)?;
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
                let expr = Expression::parse(parser, indent)?;
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
    pub fn token(token: &Token) -> Option<Self> {
        match token {
            Token::Add => Some(Self::Add),
            Token::Sub => Some(Self::Sub),
            Token::Mul => Some(Self::Mul),
            Token::Div => Some(Self::Div),
            Token::Mod => Some(Self::Mod),
            Token::Pow => Some(Self::Pow),
            Token::EQ => Some(Self::EQ),
            Token::NE => Some(Self::NE),
            Token::LT => Some(Self::LT),
            Token::GT => Some(Self::GT),
            Token::LE => Some(Self::LE),
            Token::GE => Some(Self::GE),
            Token::And => Some(Self::And),
            Token::Or => Some(Self::Or),
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
    pub fn token(token: &Token) -> Option<Self> {
        match token {
            Token::Sub => Some(Self::Neg),
            Token::Not => Some(Self::Not),
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
    pub fn token(token: &Token) -> Option<Self> {
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
impl Expression {
    pub fn binary(parser: &mut Parser, indent: usize, layer: usize) -> Result<Located<Self>, Error> {
        let Some(ops) = BinaryOperator::layer(layer) else {
            return Self::unary_left(parser, indent, 0)
        };
        let mut left = Self::binary(parser, indent, layer + 1)?;
        while let Some(Located { item: token, pos: _ }) = parser.token_ref() {
            let Some(op) = BinaryOperator::token(token) else { break; };
            if !ops.contains(&op) { break; }
            parser.token_checked()?;
            let mut pos = left.pos.clone();
            let right = Self::binary(parser, indent, layer + 1)?;
            pos.extend(&right.pos);
            left = Located::new(Self::Binary { op, left: Box::new(left), right: Box::new(right) }, pos);
        }
        Ok(left)
    }
    pub fn unary_left(parser: &mut Parser, indent: usize, layer: usize) -> Result<Located<Self>, Error> {
        let Some(ops) = UnaryLeftOperator::layer(layer) else {
            return Self::unary_right(parser, indent, 0)
        };
        if let Some(Located { item: token, pos: _ }) = parser.token_ref() {
            if let Some(op) = UnaryLeftOperator::token(token) {
                if ops.contains(&op) {
                    let Located { item: _, mut pos } = parser.token_checked()?;
                    let right = Self::unary_left(parser, indent, layer)?;
                    pos.extend(&right.pos);
                    return Ok(Located::new(Self::UnaryLeft { op, right: Box::new(right) }, pos))
                }
            }
        }
        Self::unary_left(parser, indent, layer + 1)
    }
    pub fn unary_right(parser: &mut Parser, indent: usize, layer: usize) -> Result<Located<Self>, Error> {
        let Some(ops) = UnaryRightOperator::layer(layer) else {
            let atom = Atom::parse(parser, indent)?;
            let pos = atom.pos.clone();
            return Ok(Located::new(Self::Atom(atom), pos))
        };
        let mut left = Self::unary_left(parser, indent, layer + 1)?;
        if let Some(Located { item: token, pos: _ }) = parser.token_ref() {
            if let Some(op) = UnaryRightOperator::token(token) {
                if ops.contains(&op) {
                    let Located { item: _, pos: end_pos } = parser.token_checked()?;
                    let mut pos = left.pos.clone();
                    pos.extend(&end_pos);
                    left = Located::new(Self::UnaryRight { op, left: Box::new(left) }, pos)
                }
            }
        }
        Ok(left)
    }
}
impl Parsable for Expression {
    fn parse(parser: &mut Parser, indent: usize) -> Result<Located<Self>, Error> {
        Self::binary(parser, indent, 0)
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpression {
    Type(ID), Sub(Box<Located<Self>>, Vec<Located<Self>>),
}
impl Parsable for TypeExpression {
    fn parse(parser: &mut Parser, indent: usize) -> Result<Located<Self>, Error> {
        let Located { item: id, mut pos } = ID::parse(parser, indent)?;
        let mut typ = Located::new(Self::Type(id), pos.clone());
        if let Some(Located { item: Token::IndexIn, pos: _ }) = parser.token_ref() {
            while let Some(Located { item: Token::IndexIn, pos: _ }) = parser.token_ref() {
                parser.token_checked()?;
                let mut subs = vec![];
                while let Some(Located { item: token, pos: _ }) = parser.token_ref() {
                    subs.push(Self::parse(parser, indent)?);
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
    fn parse(parser: &mut Parser, indent: usize) -> Result<Located<Self>, Error> {
        let mut args = vec![];
        let expr = Expression::parse(parser, indent)?;
        let mut pos = expr.pos.clone();
        args.push(expr);
        while let Some(Located { item: Token::Seperate, pos: _ }) = parser.token_ref() {
            parser.token_checked()?;
            let expr = Expression::parse(parser, indent)?;
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
    fn parse(parser: &mut Parser, indent: usize) -> Result<Located<Self>, Error> {
        todo!()
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AssignOperator {
    Equal, Add, Sub, Mul, Div, Mod, Pow
}
impl AssignOperator {
    pub fn token(token: &Token) -> Option<Self> {
        match token {
            Token::Equal => Some(Self::Equal),
            Token::AddEqual => Some(Self::Add),
            Token::SubEqual => Some(Self::Sub),
            Token::MulEqual => Some(Self::Mul),
            Token::DivEqual => Some(Self::Div),
            Token::ModEqual => Some(Self::Mod),
            Token::PowEqual => Some(Self::Pow),
            _ => None
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Statment {
    Variable(Located<Path>, Option<Located<TypeExpression>>, Located<Expression>),
    Assign(Located<Path>, Located<AssignOperator>, Located<Expression>),
    Call(Located<Path>, Located<Arguments>),
    If(Vec<Located<Expression>>, Vec<Located<Block>>, Option<Located<Block>>),
    While(Located<Expression>, Located<Block>), Repeat(Located<Expression>, Located<Block>),
    For(Located<Parameters>, Located<Expression>, Located<Block>),
}
impl Parsable for Statment {
    fn parse(parser: &mut Parser, indent: usize) -> Result<Located<Self>, Error> {
        let Located { item: token, pos } = parser.token_ref_checked()?;
        match token {
            Token::ID(_) => {
                let mut pos = pos.clone();
                let path = Path::parse(parser, indent)?;
                let Located { item: token, pos: mut args_pos } = parser.token_expects(&[Token::Equal, Token::AddEqual, Token::SubEqual, Token::MulEqual, Token::DivEqual, Token::ModEqual, Token::PowEqual, Token::Represent, Token::ExprIn])?;
                match token {
                    Token::Represent => {
                        if let Located { item: Path::ID(_), pos: _ } = &path {
                            if let Some(Located { item: Token::Equal, pos: _ }) = parser.token_ref() {
                                parser.token_checked()?;
                                let expr = Expression::parse(parser, indent)?;
                                pos.extend(&expr.pos);
                                return Ok(Located::new(Self::Variable(path, None, expr), pos))
                            }
                            let typ = TypeExpression::parse(parser, indent)?;
                            parser.token_expect(Token::Equal)?;
                            let expr = Expression::parse(parser, indent)?;
                            pos.extend(&expr.pos);
                            parser.expect_end()?;
                            parser.next_line();
                            Ok(Located::new(Self::Variable(path, Some(typ), expr), pos))
                        } else {
                            return Err(Error::new(format!("expected {}", Token::ID("".into()).name()), parser.path.clone(), Some(pos)))
                        }
                    }
                    Token::Equal | Token::AddEqual | Token::SubEqual | Token::MulEqual | Token::DivEqual | Token::ModEqual | Token::PowEqual => {
                        let op = AssignOperator::token(&token).unwrap();
                        let op = Located::new(op, pos.clone());
                        let expr = Expression::parse(parser, indent)?;
                        pos.extend(&expr.pos);
                        parser.expect_end()?;
                        parser.next_line();
                        Ok(Located::new(Self::Assign(path, op, expr), pos))
                    }
                    Token::ExprIn => {
                        let mut args = vec![];
                        while let Some(Located { item: token, pos: _ }) = parser.token_ref() {
                            if token == &Token::ExprOut { break }
                            args.push(Expression::parse(parser, indent)?)
                        }
                        let Located { item: _, pos: args_end_pos } = parser.token_expect(Token::ExprOut)?;
                        args_pos.extend(&args_end_pos);
                        let args = Located::new(Arguments(args), args_pos);
                        parser.expect_end()?;
                        parser.next_line();
                        Ok(Located::new(Self::Call(path, args), pos))
                    }
                    _ => panic!()
                }
            }
            Token::If => {
                let Located { item: _, mut pos } = parser.token_checked()?;
                let mut conds = vec![];
                let mut cases = vec![];
                loop {
                    let cond = Expression::parse(parser, indent)?;
                    conds.push(cond);
                    let case = Block::parse(parser, indent)?;
                    pos.extend(&case.pos);
                    cases.push(case);
                    if let Some(Located { item: Token::Elif, pos: _ }) = parser.token_ref() {
                        parser.token_checked()?;
                    } else {
                        break;
                    }
                }
                let mut else_case = None;
                if let Some(Located { item: Token::Else, pos: _ }) = parser.token_ref() {
                    parser.token_checked()?;
                    else_case = Some(Block::parse(parser, indent)?);
                }
                Ok(Located::new(Statment::If(conds, cases, else_case), pos))
            }
            Token::While => {
                let Located { item: _, mut pos } = parser.token_checked()?;
                let cond = Expression::parse(parser, indent)?;
                let body = Block::parse(parser, indent)?;
                pos.extend(&body.pos);
                Ok(Located::new(Statment::While(cond, body), pos))
            }
            Token::Repeat => {
                let Located { item: _, mut pos } = parser.token_checked()?;
                let count = Expression::parse(parser, indent)?;
                let body = Block::parse(parser, indent)?;
                pos.extend(&body.pos);
                Ok(Located::new(Statment::While(count, body), pos))
            }
            token => Err(Error::new(format!("unexpected {}", token.name()), parser.path.clone(), Some(pos.clone())))
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Block(Vec<Located<Statment>>);
impl Parsable for Block {
    fn parse(parser: &mut Parser, indent: usize) -> Result<Located<Self>, Error> {
        parser.expect_end()?;
        if parser.indent() <= indent {
            return Err(Error::new("expected indented code block", parser.path.clone(), Some(parser.pos())))
        }
        let block_indent = parser.indent();
        let mut nodes = vec![];
        let mut pos: Option<Position> = None;
        while parser.indent() >= block_indent {
            let stat = Statment::parse(parser, block_indent)?;
            pos = if let Some(mut pos) = pos {
                pos.extend(&stat.pos);
                Some(pos)
            } else {
                Some(stat.pos.clone())
            };
            nodes.push(stat);
        }
        Ok(Located::new(Self(nodes), pos.unwrap()))
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Chunk(pub Vec<Located<Statment>>);