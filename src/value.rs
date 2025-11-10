use num_bigint::BigInt;
use num_rational::BigRational;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::ast::*;

/// Lamina运行时值类型
#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    BigInt(BigInt),
    Rational(BigRational),
    Irrational(IrrationalValue),
    Bool(bool),
    String(String),
    Null,
    Array(Rc<RefCell<Vec<Value>>>),
    Struct(Rc<RefCell<HashMap<String, Value>>>),
    Lambda {
        params: Vec<String>,
        body: Box<Stmt>,
        closure: Rc<RefCell<HashMap<String, Value>>>,
    },
    Function {
        name: String,
        params: Vec<String>,
        body: Box<Stmt>,
    },
    Module(Rc<RefCell<HashMap<String, Value>>>),
    NativeFunction {
        name: String,
        func: fn(&[Value]) -> Result<Value, String>,
    },
}

/// 无理数表示（符号形式）
#[derive(Debug, Clone)]
pub enum IrrationalValue {
    Sqrt(Box<Value>),                          // √n
    Pi,                                        // π
    E,                                         // e
    Product(Box<Value>, Box<IrrationalValue>), // a * irr
    Sum(Box<IrrationalValue>, Box<IrrationalValue>),
}

impl Value {
    pub fn type_name(&self) -> &str {
        match self {
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::BigInt(_) => "bigint",
            Value::Rational(_) => "rational",
            Value::Irrational(_) => "irrational",
            Value::Bool(_) => "bool",
            Value::String(_) => "string",
            Value::Null => "null",
            Value::Array(_) => "array",
            Value::Struct(_) => "struct",
            Value::Lambda { .. } => "lambda",
            Value::Function { .. } => "function",
            Value::Module(_) => "module",
            Value::NativeFunction { .. } => "native_function",
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Null => false,
            Value::Int(0) => false,
            Value::Float(f) if *f == 0.0 => false,
            _ => true,
        }
    }

    pub fn to_float(&self) -> Result<f64, String> {
        match self {
            Value::Int(i) => Ok(*i as f64),
            Value::Float(f) => Ok(*f),
            Value::Rational(r) => {
                let num = r.numer().to_string().parse::<f64>().unwrap_or(0.0);
                let den = r.denom().to_string().parse::<f64>().unwrap_or(1.0);
                Ok(num / den)
            }
            _ => Err(format!("Cannot convert {} to float", self.type_name())),
        }
    }

    pub fn to_int(&self) -> Result<i64, String> {
        match self {
            Value::Int(i) => Ok(*i),
            Value::Float(f) => Ok(*f as i64),
            Value::Bool(b) => Ok(if *b { 1 } else { 0 }),
            _ => Err(format!("Cannot convert {} to int", self.type_name())),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::BigInt(n) => write!(f, "{}", n),
            Value::Rational(r) => write!(f, "{}/{}", r.numer(), r.denom()),
            Value::Irrational(irr) => write!(f, "{}", format_irrational(irr)),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Null => write!(f, "null"),
            Value::Array(arr) => {
                let arr = arr.borrow();
                write!(f, "[")?;
                for (i, v) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Struct(s) => {
                let s = s.borrow();
                write!(f, "{{")?;
                for (i, (k, v)) in s.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{} = {}", k, v)?;
                }
                write!(f, "}}")
            }
            Value::Lambda { params, .. } => {
                write!(f, "<lambda ({})>", params.join(", "))
            }
            Value::Function { name, params, .. } => {
                write!(f, "<function {}({})>", name, params.join(", "))
            }
            Value::Module(_) => write!(f, "<module>"),
            Value::NativeFunction { name, .. } => write!(f, "<native function {}>", name),
        }
    }
}

fn format_irrational(irr: &IrrationalValue) -> String {
    match irr {
        IrrationalValue::Sqrt(n) => format!("√{}", n),
        IrrationalValue::Pi => "π".to_string(),
        IrrationalValue::E => "e".to_string(),
        IrrationalValue::Product(coef, irr) => format!("{} * {}", coef, format_irrational(irr)),
        IrrationalValue::Sum(a, b) => {
            format!("({} + {})", format_irrational(a), format_irrational(b))
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Null, Value::Null) => true,
            _ => false,
        }
    }
}
