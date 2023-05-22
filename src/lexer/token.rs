use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    ID(String), Int(i64), Float(f64), Bool(bool), Char(char), String(String),
    ExprIn, ExprOut, IndexIn, IndexOut, ObjIn, ObjOut,
    Equal
}
impl Token {
    pub fn from_id(id: String) -> Self {
        match id.as_str() {
            "true" => Self::Bool(true),
            "false" => Self::Bool(false),
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
        }
    }
}