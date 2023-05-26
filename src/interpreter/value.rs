use std::{collections::HashMap, cmp::Ordering};
use crate::{compiler::bytecode::*, parser::ast::ID};
use super::interpreter::*;

pub type StringAddr = usize;
pub type VectorAddr = usize;
pub type ObjectAddr = usize;


#[derive(Debug, Clone, PartialEq, Default)]
pub struct Object(pub HashMap<ID, Value>);
impl Object {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
    pub fn get(&self, key: &ID) -> Option<&Value> {
        self.0.get(key)
    }
    pub fn set(&mut self, key: ID, value: Value) {
        self.0.insert(key, value);
    }
    pub fn remove(&mut self, key: &ID) {
        self.0.remove(key);
    }
    pub fn iter(&self) -> impl Iterator<Item = (&ID, &Value)> {
        self.0.iter()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum Value {
    #[default]
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    Char(char),
    String(StringAddr),
    Vector(VectorAddr),
    Object(ObjectAddr),
    Function(VarAddr),
}
impl Value {
    pub fn display(&self, interpreter: &Interpreter) -> String {
        match self {
            Value::Null => "null".to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Char(c) => c.to_string(),
            Value::String(addr) => {
                let string = interpreter.get_string(*addr);
                format!("\"{}\"", string)
            },
            Value::Vector(addr) => {
                let vector = interpreter.get_vector(*addr);
                let mut string = "[".to_string();
                for value in vector {
                    string += &value.display(interpreter);
                    string += ", ";
                }
                string += "]";
                string
            },
            Value::Object(addr) => {
                let object = interpreter.get_object(*addr);
                let mut string = "{".to_string();
                for (key, value) in object.iter() {
                    string += &key.to_string();
                    string += " = ";
                    string += &value.display(interpreter);
                    string += ", ";
                }
                string += "}";
                string
            },
            Value::Function(addr) => format!("fn:{:08x}", addr),
        }
    }
    pub fn to_bool(&self) -> bool {
        match self {
            Value::Null => false,
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::Char(c) => *c != '\0',
            Value::String(_) => true,
            Value::Vector(_) => true,
            Value::Object(_) => true,
            Value::Function(_) => true,
        }
    }
    pub fn to_function_addr(&self, interpreter: &Interpreter, args_amount: usize) -> VarAddr {
        // todo: args
        match self {
            Value::Function(addr) => *addr,
            _ => panic!("expected function"),
        }
    }
    pub fn to_object_addr(&self) -> ObjectAddr {
        match self {
            Value::Object(addr) => *addr,
            _ => panic!("expected object"),
        }
    }
    pub fn to_object<'a>(&'a self, interpreter: &'a Interpreter) -> &'a Object {
        match self {
            Value::Object(addr) => interpreter.get_object(*addr),
            _ => panic!("expected object"),
        }
    }
    pub fn to_object_mut<'a>(&'a self, interpreter: &'a mut Interpreter) -> &'a mut Object {
        match self {
            Value::Object(addr) => interpreter.get_object_mut(*addr),
            _ => panic!("expected object"),
        }
    }
    pub fn to_usize(&self) -> usize {
        match self {
            Value::Int(i) => *i as usize,
            Value::Float(f) => *f as usize,
            _ => panic!("expected int or float"),
        }
    }
    pub fn to_vector<'a>(&'a self, interpreter: &'a Interpreter) -> &'a Vec<Value> {
        match self {
            Value::Vector(addr) => interpreter.get_vector(*addr),
            _ => panic!("expected vector"),
        }
    }
    pub fn add(&self, other: &Value, interpreter: &mut Interpreter) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            (Value::Int(a), Value::Float(b)) => Value::Float(*a as f64 + b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a + *b as f64),
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            (Value::String(a), Value::String(b)) => {
                let mut string = String::new();
                string.push_str(&interpreter.get_string(*a));
                string.push_str(&interpreter.get_string(*b));
                Value::String(interpreter.new_string(string))
            },
            _ => panic!("expected int, float or string"),
        }
    }
    pub fn sub(&self, other: &Value, interpreter: &Interpreter) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
            (Value::Int(a), Value::Float(b)) => Value::Float(*a as f64 - b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a - *b as f64),
            (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
            _ => panic!("expected int or float"),
        }
    }
    pub fn mul(&self, other: &Value, interpreter: &Interpreter) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
            (Value::Int(a), Value::Float(b)) => Value::Float(*a as f64 * b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a * *b as f64),
            (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
            _ => panic!("expected int or float"),
        }
    }
    pub fn div(&self, other: &Value, interpreter: &Interpreter) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a / b),
            (Value::Int(a), Value::Float(b)) => Value::Float(*a as f64 / b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a / *b as f64),
            (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
            _ => panic!("expected int or float"),
        }
    }
    pub fn mod_(&self, other: &Value, interpreter: &Interpreter) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
            (Value::Int(a), Value::Float(b)) => Value::Float(*a as f64 % b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a % *b as f64),
            (Value::Float(a), Value::Float(b)) => Value::Float(a % b),
            _ => panic!("expected int or float"),
        }
    }
    pub fn eq(&self, other: &Value, interpreter: &Interpreter) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Bool(a == b),
            (Value::Int(a), Value::Float(b)) => Value::Bool(*a as f64 == *b),
            (Value::Float(a), Value::Int(b)) => Value::Bool(*a == *b as f64),
            (Value::Float(a), Value::Float(b)) => Value::Bool(*a == *b),
            (Value::String(a), Value::String(b)) => Value::Bool(interpreter.get_string(*a) == interpreter.get_string(*b)),
            _ => panic!("expected int, float or string"),
        }
    }
    pub fn ne(&self, other: &Value, interpreter: &Interpreter) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Bool(a != b),
            (Value::Int(a), Value::Float(b)) => Value::Bool(*a as f64 != *b),
            (Value::Float(a), Value::Int(b)) => Value::Bool(*a != *b as f64),
            (Value::Float(a), Value::Float(b)) => Value::Bool(*a != *b),
            (Value::String(a), Value::String(b)) => Value::Bool(interpreter.get_string(*a) != interpreter.get_string(*b)),
            _ => panic!("expected int, float or string"),
        }
    }
    pub fn lt(&self, other: &Value, interpreter: &Interpreter) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Bool(a < b),
            (Value::Int(a), Value::Float(b)) => Value::Bool((*a as f64) < *b),
            (Value::Float(a), Value::Int(b)) => Value::Bool(*a < *b as f64),
            (Value::Float(a), Value::Float(b)) => Value::Bool(*a < *b),
            _ => panic!("expected int, float or string"),
        }
    }
    pub fn gt(&self, other: &Value, interpreter: &Interpreter) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Bool(a > b),
            (Value::Int(a), Value::Float(b)) => Value::Bool(*a as f64 > *b),
            (Value::Float(a), Value::Int(b)) => Value::Bool(*a > *b as f64),
            (Value::Float(a), Value::Float(b)) => Value::Bool(*a > *b),
            _ => panic!("expected int, float or string"),
        }
    }
    pub fn le(&self, other: &Value, interpreter: &Interpreter) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Bool(a <= b),
            (Value::Int(a), Value::Float(b)) => Value::Bool(*a as f64 <= *b),
            (Value::Float(a), Value::Int(b)) => Value::Bool(*a <= *b as f64),
            (Value::Float(a), Value::Float(b)) => Value::Bool(*a <= *b),
            _ => panic!("expected int, float or string"),
        }
    }
    pub fn ge(&self, other: &Value, interpreter: &Interpreter) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Bool(a >= b),
            (Value::Int(a), Value::Float(b)) => Value::Bool(*a as f64 >= *b),
            (Value::Float(a), Value::Int(b)) => Value::Bool(*a >= *b as f64),
            (Value::Float(a), Value::Float(b)) => Value::Bool(*a >= *b),
            _ => panic!("expected int, float or string"),
        }
    }
    pub fn and(&self, other: &Value, interpreter: &Interpreter) -> Value {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => Value::Bool(*a && *b),
            _ => panic!("expected bool"),
        }
    }
    pub fn or(&self, other: &Value, interpreter: &Interpreter) -> Value {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => Value::Bool(*a || *b),
            _ => panic!("expected bool"),
        }
    }
    pub fn neg(&self, interpreter: &Interpreter) -> Value {
        match self {
            Value::Int(a) => Value::Int(-a),
            Value::Float(a) => Value::Float(-a),
            _ => panic!("expected int or float"),
        }
    }
    pub fn not(&self, interpreter: &Interpreter) -> Value {
        match self {
            Value::Bool(b) => Value::Bool(!b),
            _ => panic!("expected bool"),
        }
    }
    pub fn pow(&self, other: &Value, interpreter: &Interpreter) -> Value {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a.pow(*b as u32)),
            (Value::Int(a), Value::Float(b)) => Value::Float((*a as f64).powf(*b)),
            (Value::Float(a), Value::Int(b)) => Value::Float((*a).powi(*b as i32)),
            (Value::Float(a), Value::Float(b)) => Value::Float((*a).powf(*b)),
            _ => panic!("expected int or float"),
        }
    }
}
impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}
impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::Int(i)
    }
}
impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Value::Float(f)
    }
}
impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a.partial_cmp(b),
            (Value::Int(a), Value::Float(b)) => (*a as f64).partial_cmp(b),
            (Value::Float(a), Value::Int(b)) => a.partial_cmp(&(*b as f64)),
            (Value::Float(a), Value::Float(b)) => a.partial_cmp(b),
            (Value::String(a), Value::String(b)) => a.partial_cmp(b),
            (Value::Bool(a), Value::Bool(b)) => a.partial_cmp(b),
            (Value::Null, Value::Null) => Some(Ordering::Equal),
            _ => None,
        }
    }
}