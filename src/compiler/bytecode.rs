use std::fmt::Display;

pub type CodeAddr = usize;
pub type Addr = usize;
pub type VarAddr = usize;
pub type ConstAddr = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ByteCode {
    None, Halt, Break, Continue,
    Copy, Swap, Drop,
    Jump(CodeAddr), JumpIf(CodeAddr), JumpIfNot(CodeAddr),
    Call(usize), CallReturn(usize), Return(usize),
    Load(Addr), Store(Addr),
    Int(i64), Float(ConstAddr), Bool(bool), Char(char), String(ConstAddr),
    Add, Sub, Mul, Div, Mod, Pow,
    EQ, NE, LT, GT, LE, GE,
    And, Or, In,
    Not, Neg,

    Object, ObjectField(CodeAddr), Vector(usize),
    Index, Field(ConstAddr),
}
impl ByteCode {
    pub fn to_u8(self) -> Vec<u8> {
        match self {
            Self::None => vec![0],
            Self::Halt => vec![1],
            Self::Break => vec![2],
            Self::Continue => vec![3],
            Self::Copy => vec![4],
            Self::Swap => vec![5],
            Self::Drop => vec![6],
            Self::Jump(addr) => vec![7, (addr >> 8) as u8, addr as u8],
            Self::JumpIf(addr) => vec![8, (addr >> 8) as u8, addr as u8],
            Self::JumpIfNot(addr) => vec![9, (addr >> 8) as u8, addr as u8],
            Self::Call(n) => vec![10, n as u8],
            Self::CallReturn(n) => vec![11, n as u8],
            Self::Return(n) => vec![12, n as u8],
            Self::Load(addr) => vec![13, (addr >> 8) as u8, addr as u8],
            Self::Store(addr) => vec![14, (addr >> 8) as u8, addr as u8],
            Self::Int(v) => vec![15, (v >> 56) as u8, (v >> 48) as u8, (v >> 40) as u8, (v >> 32) as u8, (v >> 24) as u8, (v >> 16) as u8, (v >> 8) as u8, v as u8],
            Self::Float(addr) => vec![16, (addr >> 8) as u8, addr as u8],
            Self::Bool(v) => vec![17, v as u8],
            Self::Char(v) => vec![18, v as u8],
            Self::String(addr) => vec![19, (addr >> 8) as u8, addr as u8],
            Self::Add => vec![20],
            Self::Sub => vec![21],
            Self::Mul => vec![22],
            Self::Div => vec![23],
            Self::Mod => vec![24],
            Self::Pow => vec![25],
            Self::EQ => vec![26],
            Self::NE => vec![27],
            Self::LT => vec![28],
            Self::GT => vec![29],
            Self::LE => vec![30],
            Self::GE => vec![31],
            Self::And => vec![32],
            Self::Or => vec![33],
            Self::In => vec![34],
            Self::Not => vec![35],
            Self::Neg => vec![36],
            Self::Object => vec![37],
            Self::ObjectField(addr) => vec![38, (addr >> 8) as u8, addr as u8],
            Self::Vector(n) => vec![39, n as u8],
            Self::Index => vec![40],
            Self::Field(addr) => vec![41, (addr >> 8) as u8, addr as u8],
        }
    }
    pub fn from_u8(code: &[u8]) -> Option<Self> {
        match code {
            [0] => Some(Self::None),
            [1] => Some(Self::Halt),
            [2] => Some(Self::Break),
            [3] => Some(Self::Continue),
            [4] => Some(Self::Copy),
            [5] => Some(Self::Swap),
            [6] => Some(Self::Drop),
            [7, a, b] => Some(Self::Jump(((*a as usize) << 8) | (*b as usize))),
            [8, a, b] => Some(Self::JumpIf(((*a as usize) << 8) | (*b as usize))),
            [9, a, b] => Some(Self::JumpIfNot(((*a as usize) << 8) | (*b as usize))),
            [10, n] => Some(Self::Call(*n as usize)),
            [11, n] => Some(Self::CallReturn(*n as usize)),
            [12, n] => Some(Self::Return(*n as usize)),
            [13, a, b] => Some(Self::Load(((*a as usize) << 8) | (*b as usize))),
            [14, a, b] => Some(Self::Store(((*a as usize) << 8) | (*b as usize))),
            [15, a, b, c, d, e, f, g, h] => Some(Self::Int(((*a as i64) << 56) | ((*b as i64) << 48) | ((*c as i64) << 40) | ((*d as i64) << 32) | ((*e as i64) << 24) | ((*f as i64) << 16) | ((*g as i64) << 8) | (*h as i64))),
            [16, a, b] => Some(Self::Float(((*a as usize) << 8) | (*b as usize))),
            [17, v] => Some(Self::Bool(*v != 0)),
            [18, v] => Some(Self::Char(*v as char)),
            [19, a, b] => Some(Self::String(((*a as usize) << 8) | (*b as usize))),
            [20] => Some(Self::Add),
            [21] => Some(Self::Sub),
            [22] => Some(Self::Mul),
            [23] => Some(Self::Div),
            [24] => Some(Self::Mod),
            [25] => Some(Self::Pow),
            [26] => Some(Self::EQ),
            [27] => Some(Self::NE),
            [28] => Some(Self::LT),
            [29] => Some(Self::GT),
            [30] => Some(Self::LE),
            [31] => Some(Self::GE),
            [32] => Some(Self::And),
            [33] => Some(Self::Or),
            [34] => Some(Self::In),
            [35] => Some(Self::Not),
            [36] => Some(Self::Neg),
            [37] => Some(Self::Object),
            [38, a, b] => Some(Self::ObjectField(((*a as usize) << 8) | (*b as usize))),
            [39, n] => Some(Self::Vector(*n as usize)),
            [40] => Some(Self::Index),
            [41, a, b] => Some(Self::Field(((*a as usize) << 8) | (*b as usize))),
            _ => None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Code {
    pub code: Vec<ByteCode>,
}
impl Code {
    pub fn new() -> Self {
        Self { code: Vec::new() }
    }
    pub fn push(&mut self, code: ByteCode) {
        self.code.push(code);
    }
    pub fn overwrite(&mut self, index: usize, code: ByteCode) {
        self.code[index] = code;
    }
    pub fn insert(&mut self, index: usize, code: ByteCode) {
        self.code.insert(index, code);
    }
}
impl Display for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, code) in self.code.iter().enumerate() {
            write!(f, "{:04} {:?}\n", i, code)?;
        }
        Ok(())
    }
}