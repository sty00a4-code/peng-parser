use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    ID(String), Int(i64), Float(f64), Bool(bool), Char(char), String(String),
    ExprIn, ExprOut, IndexIn, IndexOut, ObjIn, ObjOut,
    Equal, Seperate, Represent, Field, Out,

    Add, Sub, Mul, Div, Mod, Pow,
    AddEqual, SubEqual, DivEqual, MulEqual, ModEqual, PowEqual,
    EQ, NE, LT, GT, LE, GE,
    And, Or, Not,

    If, Elif, Else, Match, While, Repeat, For, In,
    Func, Return, Break, Continue, Pass,
}
impl Token {
    pub fn from_id(id: String) -> Self {
        match id.as_str() {
            "true" => Self::Bool(true),
            "false" => Self::Bool(false),
            "and" => Self::And,
            "or" => Self::Or,
            "not" => Self::Not,
            "if" => Self::If,
            "elif" => Self::Elif,
            "else" => Self::Else,
            "match" => Self::Match,
            "while" => Self::While,
            "repeat" => Self::Repeat,
            "for" => Self::For,
            "in" => Self::In,
            "func" => Self::Func,
            "return" => Self::Return,
            "break" => Self::Break,
            "continue" => Self::Continue,
            "pass" => Self::Pass,
            _ => Self::ID(id)
        }
    }
    pub fn name(&self) -> String {
        match self {
            Self::ID(_) => format!("identifier"),
            Self::Int(_) => format!("integer"),
            Self::Float(_) => format!("decimal point number"),
            Self::Bool(_) => format!("boolean"),
            Self::Char(_) => format!("character"),
            Self::String(_) => format!("string"),
            _ => format!("'{self}'")
        }
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ID(id) => write!(f, "{id}"),
            Self::Int(v) => write!(f, "{v:?}"),
            Self::Float(v) => write!(f, "{v:?}"),
            Self::Bool(v) => write!(f, "{v:?}"),
            Self::Char(v) => write!(f, "{v:?}"),
            Self::String(v) => write!(f, "{v:?}"),
            Self::ExprIn => write!(f, "("),
            Self::ExprOut => write!(f, ")"),
            Self::IndexIn => write!(f, "["),
            Self::IndexOut => write!(f, "]"),
            Self::ObjIn => write!(f, "{{"),
            Self::ObjOut => write!(f, "}}"),
            Self::Equal => write!(f, "="),
            Self::Seperate => write!(f, ","),
            Self::Represent => write!(f, ":"),
            Self::Field => write!(f, "."),
            Self::Out => write!(f, "->"),
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),
            Self::Pow => write!(f, "^"),
            Self::AddEqual => write!(f, "+="),
            Self::SubEqual => write!(f, "-="),
            Self::MulEqual => write!(f, "*="),
            Self::DivEqual => write!(f, "/="),
            Self::ModEqual => write!(f, "%="),
            Self::PowEqual => write!(f, "^="),
            Self::EQ => write!(f, "=="),
            Self::NE => write!(f, "!="),
            Self::LT => write!(f, "<"),
            Self::GT => write!(f, ">"),
            Self::LE => write!(f, "<="),
            Self::GE => write!(f, ">="),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::Not => write!(f, "not"),
            Self::If => write!(f, "if"),
            Self::Elif => write!(f, "elif"),
            Self::Else => write!(f, "else"),
            Self::Match => write!(f, "match"),
            Self::While => write!(f, "while"),
            Self::Repeat => write!(f, "repeat"),
            Self::For => write!(f, "for"),
            Self::In => write!(f, "in"),
            Self::Func => write!(f, "func"),
            Self::Return => write!(f, "return"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::Pass => write!(f, "pass"),
        }
    }
}