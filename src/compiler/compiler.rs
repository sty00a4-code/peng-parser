use std::collections::{HashMap, HashSet};

use crate::{location::{path::FilePath, position::Located}, error::Error, parser::ast::*};
use super::bytecode::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub variables: HashMap<ID, VarAddr>,
}
impl Scope {
    pub fn new() -> Self {
        Self { variables: HashMap::new() }
    }
    pub fn set_variable(&mut self, name: ID, index: usize) -> Option<usize> {
        self.variables.insert(name, index)
    }
    pub fn get_variable(&self, name: &ID) -> Option<usize> {
        self.variables.get(name).copied()
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Frame {
    pub path: FilePath,
    pub scopes: Vec<Scope>,
}
impl Frame {
    pub fn new(path: FilePath) -> Self {
        Self { path, scopes: vec![Scope::new()] }
    }
    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }
    pub fn pop_scope(&mut self) -> Option<Scope> {
        self.scopes.pop()
    }
    pub fn set_variable(&mut self, name: ID, addr: VarAddr) -> Option<VarAddr> {
        self.scopes.last_mut()?.set_variable(name, addr)
    }
    pub fn get_variable(&self, name: &ID) -> Option<VarAddr> {
        for scope in self.scopes.iter().rev() {
            if let Some(addr) = scope.get_variable(name) {
                return Some(addr)
            }
        }
        None
    }
}

pub struct Compiler {
    pub path: FilePath,
    pub code: Code,
    pub floats: Vec<f64>,
    pub strings: Vec<String>,
    pub functions: HashMap<VarAddr, CodeAddr>,
    pub frames: Vec<Frame>,
    pub memory_max: usize,
    pub used_addrs: HashSet<VarAddr>,
}
impl Compiler {
    pub fn new(path: FilePath) -> Self {
        Self {
            path,
            code: Code::new(),
            floats: vec![],
            strings: vec![],
            functions: HashMap::new(),
            frames: vec![],
            memory_max: 0,
            used_addrs: HashSet::new(),
        }
    }

    pub fn new_addr(&mut self) -> VarAddr {
        self.code.new_addr()
    }
    pub fn new_float(&mut self, float: f64) -> CodeAddr {
        self.code.new_float(float)
    }
    pub fn new_string(&mut self, string: String) -> CodeAddr {
        self.code.new_string(string)
    }
    pub fn set_function(&mut self, addr: VarAddr, code: CodeAddr) -> Option<CodeAddr> {
        self.code.set_function(addr, code)
    }
    pub fn create_variable(&mut self, name: ID) -> VarAddr {
        let addr = self.new_addr();
        self.set_variable(name, addr);
        addr
    }
    pub fn set_variable(&mut self, name: ID, addr: VarAddr) -> Option<VarAddr> {
        self.frames.last_mut()?.set_variable(name, addr)
    }
    pub fn get_variable(&self, name: &ID) -> Option<VarAddr> {
        for frame in self.frames.iter().rev() {
            if let Some(addr) = frame.get_variable(name) {
                return Some(addr)
            }
        }
        None
    }
    pub fn push_scope(&mut self) -> Option<()> {
        self.frames.last_mut()?.push_scope();
        Some(())
    }
    pub fn pop_scope(&mut self) -> Option<()> {
        let scope = self.frames.last_mut()?.pop_scope()?;
        for (_, addr) in scope.variables {
            self.used_addrs.remove(&addr);
        }
        Some(())
    }

    pub fn compile(&mut self, ast: Chunk) -> Result<Code, Error> {
        self.compile_chunk(ast)?;
        self.code.push(ByteCode::Halt);
        Ok(self.code.clone())
    }

    pub fn compile_chunk(&mut self, ast: Chunk) -> Result<(), Error> {
        self.frames.push(Frame::new(self.path.clone()));
        for statement in ast.0 {
            self.compile_statement(statement)?;
        }
        self.frames.pop();
        Ok(())
    }
    pub fn compile_statement(&mut self, statement: Located<Statment>) -> Result<(), Error> {
        let Located { item: statement, pos } = statement;
        match statement {
            // untested
            Statment::Return(expression) => {
                self.code.push(ByteCode::Return(1));
            }
            // untested
            Statment::Break => {
                self.code.push(ByteCode::Break);
            }
            // untested
            Statment::Continue => {
                self.code.push(ByteCode::Continue);
            }
            // untested
            Statment::Pass => {
                self.code.push(ByteCode::None);
            }
            // untested
            Statment::Variable(Located { item: id, pos }, typ, expression) => {
                let addr = self.create_variable(id);
                self.compile_expression(expression)?;
                self.code.push(ByteCode::Store(addr));
            }
            Statment::Assign(path, Located { item: op, pos: op_pos }, expression) => {
                let addr = self.get_path(path.clone(), true)?;
                self.compile_expression(expression)?;
                if op != AssignOperator::Equal {
                    self.compile_path(path)?;
                    self.code.push(match op {
                        AssignOperator::Equal => panic!(),
                        AssignOperator::Add => ByteCode::Add,
                        AssignOperator::Sub => ByteCode::Sub,
                        AssignOperator::Mul => ByteCode::Mul,
                        AssignOperator::Div => ByteCode::Div,
                        AssignOperator::Mod => ByteCode::Mod,
                        AssignOperator::Pow => ByteCode::Pow,
                    });
                }
                self.code.push(ByteCode::Store(addr))
            }
            Statment::If(conditions, cases, else_case) => {
                let mut indexes = vec![];
                for (condition, body) in conditions.into_iter().zip(cases.into_iter()) {
                    self.compile_expression(condition)?;
                    let condition_index = self.code.code.len();
                    self.code.push(ByteCode::None);
                    self.compile_block(body)?;
                    indexes.push(self.code.code.len());
                    self.code.push(ByteCode::None);
                    self.code.overwrite(condition_index, ByteCode::JumpIfNot(self.code.code.len()));
                }
                if let Some(body) = else_case {
                    self.compile_block(body)?;
                }
                for index in indexes {
                    self.code.overwrite(index, ByteCode::Jump(self.code.code.len()));
                }
            }
            // untested
            Statment::Match(expression, match_cases) => {
                self.compile_expression(expression)?;
                let mut indexes = vec![];
                for Located { item: match_case, pos } in match_cases {
                    let MatchCase { pattern, guard, body } = match_case;
                    self.code.push(ByteCode::Copy);
                    self.compile_pattern(pattern)?;
                    if let Some(guard) = guard {
                        self.compile_expression(guard)?;
                        self.code.push(ByteCode::And);
                    }
                    self.code.push(ByteCode::JumpIfNot(self.code.code.len() + 2));
                    self.code.push(ByteCode::Drop);
                    self.compile_block(body)?;
                    indexes.push(self.code.code.len());
                    self.code.push(ByteCode::None);
                }
                for index in indexes {
                    self.code.overwrite(index, ByteCode::Jump(self.code.code.len()));
                }
            }
            // untested
            Statment::While(condition, body) => {
                let start_index = self.code.code.len();
                self.compile_expression(condition)?;
                let skip_index = self.code.code.len();
                self.code.push(ByteCode::None);
                self.compile_block(body)?;
                self.code.push(ByteCode::Jump(start_index));
                self.code.overwrite(skip_index, ByteCode::Jump(self.code.code.len()));
            }
            // untested
            Statment::Repeat(count, body) => {
                let start_index = self.code.code.len();
                self.compile_expression(count)?;
                let skip_index = self.code.code.len();
                self.code.push(ByteCode::None);
                self.compile_block(body)?;
                self.code.push(ByteCode::Copy);
                self.code.push(ByteCode::Int(1));
                self.code.push(ByteCode::Sub);
                self.code.push(ByteCode::Copy);
                self.code.push(ByteCode::Int(0));
                self.code.push(ByteCode::EQ);
                self.code.push(ByteCode::JumpIfNot(start_index));
                self.code.overwrite(skip_index, ByteCode::Jump(self.code.code.len()));
            }
            Statment::For(params, iter, body) => todo!("for loop compiling"),
            // untested
            Statment::Function(path, params, return_type, body) => {
                let addr = self.get_path(path, true)?;
                self.set_function(addr, self.code.code.len());
                self.code.push(ByteCode::None);
                let index = self.code.code.len();
                self.compile_block(body)?;
                self.code.push(ByteCode::Return(0));
                self.code.overwrite(index, ByteCode::Jump(self.code.code.len()));
            }
            // untested
            Statment::Call(func, args) => {
                self.compile_path(func)?;
                let len = args.item.0.len();
                self.compile_args(args)?;
                self.code.push(ByteCode::Call(len));
            }
        }
        Ok(())
    }

    pub fn compile_block(&mut self, block: Located<Block>) -> Result<(), Error> {
        self.frames.push(Frame::new(self.path.clone()));
        for statement in block.item.0 {
            self.compile_statement(statement)?;
        }
        self.frames.pop();
        Ok(())
    }

    pub fn compile_expression(&mut self, expression: Located<Expression>) -> Result<(), Error> {
        let Located { item: expression, pos } = expression;
        match expression {
            Expression::Atom(atom) => {
                return self.compile_atom(atom)
            }
            // untested
            Expression::Binary { op, left, right } => {
                self.compile_expression(*left)?;
                self.compile_expression(*right)?;
                match op {
                    BinaryOperator::Add => self.code.push(ByteCode::Add),
                    BinaryOperator::Sub => self.code.push(ByteCode::Sub),
                    BinaryOperator::Mul => self.code.push(ByteCode::Mul),
                    BinaryOperator::Div => self.code.push(ByteCode::Div),
                    BinaryOperator::Mod => self.code.push(ByteCode::Mod),
                    BinaryOperator::Pow => self.code.push(ByteCode::Pow),
                    BinaryOperator::And => self.code.push(ByteCode::And),
                    BinaryOperator::Or => self.code.push(ByteCode::Or),
                    BinaryOperator::EQ => self.code.push(ByteCode::EQ),
                    BinaryOperator::NE => self.code.push(ByteCode::NE),
                    BinaryOperator::LT => self.code.push(ByteCode::LT),
                    BinaryOperator::LE => self.code.push(ByteCode::LE),
                    BinaryOperator::GT => self.code.push(ByteCode::GT),
                    BinaryOperator::GE => self.code.push(ByteCode::GE),
                    BinaryOperator::In => self.code.push(ByteCode::In),
                }
            }
            Expression::UnaryLeft { op, right } => {
                self.compile_expression(*right)?;
                match op {
                    UnaryLeftOperator::Not => self.code.push(ByteCode::Not),
                    UnaryLeftOperator::Neg => self.code.push(ByteCode::Neg),
                }
            }
            Expression::UnaryRight { op, left } => {
                self.compile_expression(*left)?;
                match op {
                }
            }
            // untested
            Expression::Call { func, args } => {
                self.compile_path(func)?;
                let len = args.item.0.len();
                self.compile_args(args)?;
                self.code.push(ByteCode::Call(len));
            }
        }
        Ok(())
    }
    pub fn compile_atom(&mut self, atom: Located<Atom>) -> Result<(), Error> {
        let Located { item: atom, pos } = atom;
        match atom {
            Atom::Path(path) => {
                self.compile_path(path)?;
            }
            Atom::Int(value) => {
                self.code.push(ByteCode::Int(value));
            }
            Atom::Float(value) => {
                let addr = self.new_float(value);
                self.code.push(ByteCode::Float(addr));
            }
            Atom::Bool(value) => {
                self.code.push(ByteCode::Bool(value));
            }
            Atom::Char(value) => {
                self.code.push(ByteCode::Char(value));
            }
            Atom::String(value) => {
                let addr = self.new_string(value);
                self.code.push(ByteCode::String(addr));
            }
            Atom::Expression(expression) => {
                self.compile_expression(*expression)?;
            }
            // untested
            Atom::Object(object) => {
                self.code.push(ByteCode::Object);
                for object_entry in object {
                    self.compile_object_entry(object_entry)?;
                }
            }
            // untested
            Atom::Vector(values) => {
                let len = values.len();
                for expression in values {
                    self.compile_expression(expression)?;
                }
                // might need to be reversed for the interpreter
                self.code.push(ByteCode::Vector(len));
            }
        }
        Ok(())
    }
    pub fn compile_path(&mut self, path: Located<Path>) -> Result<(), Error> {
        let Located { item: path, pos } = path;
        match path {
            Path::ID(id) => {
                let Some(addr) = self.get_variable(&id) else {
                    return Err(Error::new(format!("variable '{id}' not found"), self.path.clone(), Some(pos)))
                };
                self.code.push(ByteCode::Load(addr));
            }
            _ => todo!("path compiling")
        }
        Ok(())
    }
    pub fn get_path(&mut self, path: Located<Path>, create: bool) -> Result<VarAddr, Error> {
        let Located { item: path, pos } = path;
        match path {
            Path::ID(id) => {
                let Some(addr) = self.get_variable(&id) else {
                    if create {
                        return Ok(self.create_variable(id))
                    } else {
                        return Err(Error::new(format!("variable '{id}' not found"), self.path.clone(), Some(pos)))
                    }
                };
                Ok(addr)
            }
            _ => todo!("path compiling")
        }
    }
    /// assumes a new object is on the stack
    pub fn compile_object_entry(&mut self, path: Located<ObjectEntry>) -> Result<(), Error> {
        todo!("object entry compiling")
    }
    /// assumes the call arguments are on the stack
    pub fn compile_args(&mut self, path: Located<Arguments>) -> Result<(), Error> {
        todo!("args compiling")
    }
    pub fn compile_pattern(&mut self, path: Located<Pattern>) -> Result<(), Error> {
        todo!("pattern compiling")
    }
}