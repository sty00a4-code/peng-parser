use crate::{location::path::FilePath, compiler::bytecode::*, error::Error, parser::ast::ID};
use super::value::*;

pub struct Interpreter {
    pub path: FilePath,
    pub code: Code,
    pub stack: Vec<Value>,
    // (return address, return values)
    pub call_stack: Vec<(CodeAddr, bool)>,
    pub memory: Vec<Value>,
    pub vectors: Vec<Vec<Value>>,
    pub objects: Vec<Object>,
    pub ip: usize,
    pub halt: bool,
}
impl Interpreter {
    pub fn new(path: FilePath, code: Code) -> Self {
        let memory = vec![Value::default(); code.memory_max + 1];
        Self {
            path,
            code,
            stack: vec![],
            call_stack: vec![],
            memory,
            vectors: vec![],
            objects: vec![],
            ip: 0,
            halt: false,
        }
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(value);
    }
    pub fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }
    pub fn copy(&mut self) {
        let value = self.pop();
        self.push(value);
        self.push(value);
    }
    pub fn swap(&mut self) {
        let value1 = self.pop();
        let value2 = self.pop();
        self.push(value1);
        self.push(value2);
    }
    pub fn get(&self, addr: usize) -> Value {
        self.memory[addr]
    }
    pub fn set(&mut self, addr: usize, value: Value) {
        self.memory[addr] = value;
    }
    pub fn new_string(&mut self, string: String) -> StringAddr {
        let addr = self.code.strings.len();
        self.code.strings.push(string);
        addr
    }
    pub fn new_vector(&mut self, values: Vec<Value>) -> ObjectAddr {
        let addr = self.memory.len();
        self.memory.push(Value::Vector(self.vectors.len()));
        self.vectors.push(values);
        addr
    }
    pub fn new_object(&mut self) -> ObjectAddr {
        let addr = self.memory.len();
        self.memory.push(Value::Object(self.objects.len()));
        self.objects.push(Object::default());
        addr
    }
    pub fn get_string(&self, addr: StringAddr) -> &String {
        &self.code.strings[addr]
    }
    pub fn get_string_mut(&mut self, addr: StringAddr) -> &mut String {
        &mut self.code.strings[addr]
    }
    pub fn get_vector(&self, addr: VectorAddr) -> &Vec<Value> {
        &self.vectors[addr]
    }
    pub fn get_vector_mut(&mut self, addr: VectorAddr) -> &mut Vec<Value> {
        &mut self.vectors[addr]
    }
    pub fn get_object(&self, addr: ObjectAddr) -> &Object {
        &self.objects[addr]
    }
    pub fn get_object_mut(&mut self, addr: ObjectAddr) -> &mut Object {
        &mut self.objects[addr]
    }

    pub fn run(&mut self) -> Result<Option<Value>, Error> {
        while !self.halt {
            self.step()?;
        }
        Ok(self.stack.pop())
    }
    pub fn step(&mut self) -> Result<(), Error> {
        let code = self.code.code[self.ip];
        self.ip += 1;
        match code {
            ByteCode::Halt => self.halt = true,
            ByteCode::None => (),
            ByteCode::Jump(addr) => self.ip = addr,
            ByteCode::JumpIf(addr) => if self.pop().to_bool() { self.ip = addr; },
            ByteCode::JumpIfNot(addr) => if !self.pop().to_bool() { self.ip = addr; },
            ByteCode::Break => {
                let (addr, _) = self.call_stack.pop().unwrap();
                self.ip = addr;
            }
            ByteCode::Continue => {
                let (addr, _) = self.call_stack.last_mut().unwrap();
                self.ip = *addr;
            }

            ByteCode::Null => self.push(Value::Null),
            ByteCode::Bool(b) => self.push(Value::Bool(b)),
            ByteCode::Int(i) => self.push(Value::Int(i)),
            ByteCode::Float(addr) => self.push(Value::Float(self.code.floats[addr])),
            ByteCode::Char(c) => self.push(Value::Char(c)),
            ByteCode::String(addr) => self.push(Value::String(addr)),
            ByteCode::Vector(amount) => {
                let mut vector = vec![];
                for _ in 0..amount {
                    vector.push(self.pop());
                }
                let vector_addr = self.new_vector(vector);
                self.push(Value::Vector(vector_addr));
            }
            ByteCode::Object => {
                let object_addr = self.new_object();
                self.push(Value::Object(object_addr))
            }
            ByteCode::ObjectField(addr) => {
                let object_addr = self.pop().to_object_addr();
                let field = self.get_string(addr).clone();
                let value = self.pop();
                self.get_object_mut(object_addr).set(ID(field), value);
                self.push(Value::Object(object_addr));
            }

            ByteCode::Drop => { self.pop(); },
            ByteCode::Copy => self.copy(),
            ByteCode::Swap => self.swap(),

            ByteCode::Load(addr) => self.push(self.get(addr)),
            ByteCode::Store(addr) => {
                let value = self.pop();
                self.set(addr, value);
            }

            ByteCode::Call(args) => {
                let addr = self.pop().to_function_addr(self, args);
                self.call_stack.push((self.ip, false));
                self.ip = addr;
            }
            ByteCode::CallReturn(args) => {
                let addr = self.pop().to_function_addr(self, args);
                self.call_stack.push((self.ip, true));
                self.ip = addr;
            }
            ByteCode::Return(amount) => {
                let (addr, return_values) = self.call_stack.pop().unwrap();
                self.ip = addr;
                if !return_values {
                    for _ in 0..amount {
                        self.pop();
                    }
                }
            }

            ByteCode::Add => {
                let value2 = self.pop();
                let value1 = self.pop();
                let value = value1.add(&value2, self);
                self.push(value);
            }
            ByteCode::Sub => {
                let value2 = self.pop();
                let value1 = self.pop();
                self.push(value1.sub(&value2, self));
            }
            ByteCode::Mul => {
                let value2 = self.pop();
                let value1 = self.pop();
                self.push(value1.mul(&value2, self));
            }
            ByteCode::Div => {
                let value2 = self.pop();
                let value1 = self.pop();
                self.push(value1.div(&value2, self));
            }
            ByteCode::Mod => {
                let value2 = self.pop();
                let value1 = self.pop();
                self.push(value1.mod_(&value2, self));
            }
            ByteCode::Pow => {
                let value2 = self.pop();
                let value1 = self.pop();
                self.push(value1.pow(&value2, self));
            }
            ByteCode::Neg => {
                let value = self.pop();
                self.push(value.neg(self));
            }
            ByteCode::Not => {
                let value = self.pop();
                self.push(value.not(self));
            }
            ByteCode::EQ => {
                let value2 = self.pop();
                let value1 = self.pop();
                self.push(value1.eq(&value2, self));
            }
            ByteCode::NE => {
                let value2 = self.pop();
                let value1 = self.pop();
                self.push(value1.ne(&value2, self));
            }
            ByteCode::LT => {
                let value2 = self.pop();
                let value1 = self.pop();
                self.push(value1.lt(&value2, self));
            }
            ByteCode::LE => {
                let value2 = self.pop();
                let value1 = self.pop();
                self.push(value1.le(&value2, self));
            }
            ByteCode::GT => {
                let value2 = self.pop();
                let value1 = self.pop();
                self.push(value1.gt(&value2, self));
            }
            ByteCode::GE => {
                let value2 = self.pop();
                let value1 = self.pop();
                self.push(value1.ge(&value2, self));
            }
            ByteCode::And => {
                let value2 = self.pop();
                let value1 = self.pop();
                self.push(value1.and(&value2, self));
            }
            ByteCode::Or => {
                let value2 = self.pop();
                let value1 = self.pop();
                self.push(value1.or(&value2, self));
            }
            ByteCode::In => {
                let value = self.pop();
                let vector = self.pop(); let vector = vector.to_vector(self);
                self.push(Value::Bool(vector.contains(&value)));
            }

            ByteCode::Index => {
                let index = self.pop().to_usize();
                let vector = self.pop(); let vector = vector.to_vector(self);
                self.push(vector[index]);
            }
            ByteCode::Field(addr) => {
                let object = self.pop(); let object = object.to_object(self);
                let field = self.get_string(addr).clone();
                let value = object.get(&ID(field.clone()));
                if let Some(value) = value {

                } else {
                    return Err(Error::new(format!("object does not have field '{}'", field), self.path.clone(), Some(self.code.poses[self.ip].clone())));
                }
            }
        }
        Ok(())
    }
}