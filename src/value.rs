use num::BigInt;
use num::BigRational;
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
    Complex(Box<Value>, Box<Value>), // (real, imaginary) - symbolic representation
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
        decorators: Vec<String>, // LSR-011: Decorators applied to this function
    },
    Module(Rc<RefCell<HashMap<String, Value>>>),
    NativeFunction {
        name: String,
        func: fn(&[Value]) -> Result<Value, String>,
    },
    CurriedFunction {
        original: Box<Value>,       // 原始函数
        collected_args: Vec<Value>, // 已收集的参数
        total_params: usize,        // 总参数数量
    },
    MemoizedFunction {
        original: Box<Value>,                       // 原始函数
        cache: Rc<RefCell<HashMap<String, Value>>>, // 参数->结果的缓存
    },
}

/// 无理数表示（符号形式）
#[derive(Debug, Clone)]
pub enum IrrationalValue {
    Sqrt(Box<Value>),                          // √n (square root)
    Root(u32, Box<Value>),                     // n-th root: ⁿ√value
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
            Value::Complex(_, _) => "complex",
            Value::Bool(_) => "bool",
            Value::String(_) => "string",
            Value::Null => "null",
            Value::Array(_) => "array",
            Value::Struct(_) => "struct",
            Value::Lambda { .. } => "lambda",
            Value::Function { .. } => "function",
            Value::Module(_) => "module",
            Value::NativeFunction { .. } => "native_function",
            Value::CurriedFunction { .. } => "curried_function",
            Value::MemoizedFunction { .. } => "memoized_function",
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Null => false,
            Value::Int(0) => false,
            Value::Float(f) if *f == 0.0 => false,
            Value::Complex(re, im) => {
                // Complex is truthy if either real or imaginary part is non-zero
                re.is_truthy() || im.is_truthy()
            }
            _ => true,
        }
    }

    pub fn to_float(&self) -> Result<f64, String> {
        match self {
            Value::Int(i) => Ok(*i as f64),
            Value::Float(f) => Ok(*f),
            Value::BigInt(b) => {
                // Convert BigInt to f64 via string parsing
                b.to_string()
                    .parse::<f64>()
                    .map_err(|_| format!("BigInt {} is too large to convert to float", b))
            }
            Value::Rational(r) => {
                let num = r.numer().to_string().parse::<f64>().unwrap_or(0.0);
                let den = r.denom().to_string().parse::<f64>().unwrap_or(1.0);
                Ok(num / den)
            }
            Value::Irrational(irr) => {
                // Convert irrational values to their float approximations
                Ok(irrational_to_float(irr))
            }
            Value::Complex(re, im) => {
                // For complex numbers, only return real part if imaginary is zero
                let im_float = im.to_float()?;
                if im_float == 0.0 {
                    re.to_float()
                } else {
                    Err("Cannot convert non-real complex number to float".to_string())
                }
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
            Value::Complex(re, im) => {
                // Format complex numbers symbolically
                let re_str = format!("{}", re);
                let im_str = format!("{}", im);

                // Check if real part is zero
                let re_is_zero = match re.as_ref() {
                    Value::Int(0) => true,
                    Value::Rational(r) => r.numer().to_string() == "0",
                    _ => false,
                };

                // Check if imaginary part is zero
                let im_is_zero = match im.as_ref() {
                    Value::Int(0) => true,
                    Value::Rational(r) => r.numer().to_string() == "0",
                    _ => false,
                };

                // Check if imaginary coefficient is 1 or -1
                let im_is_one = match im.as_ref() {
                    Value::Int(1) => true,
                    Value::Rational(r) => {
                        r.numer().to_string() == "1" && r.denom().to_string() == "1"
                    }
                    _ => false,
                };

                let im_is_neg_one = match im.as_ref() {
                    Value::Int(-1) => true,
                    Value::Rational(r) => {
                        r.numer().to_string() == "-1" && r.denom().to_string() == "1"
                    }
                    _ => false,
                };

                if im_is_zero {
                    // Pure real number
                    write!(f, "{}", re_str)
                } else if re_is_zero {
                    // Pure imaginary number
                    if im_is_one {
                        write!(f, "i")
                    } else if im_is_neg_one {
                        write!(f, "-i")
                    } else {
                        write!(f, "{}i", im_str)
                    }
                } else {
                    // Both real and imaginary parts
                    if im_is_one {
                        write!(f, "{}+i", re_str)
                    } else if im_is_neg_one {
                        write!(f, "{}-i", re_str)
                    } else {
                        // Check if imaginary part is negative
                        let im_is_negative = match im.as_ref() {
                            Value::Int(n) if *n < 0 => true,
                            Value::Rational(r) if r.numer().to_string().starts_with('-') => true,
                            _ => false,
                        };

                        if im_is_negative {
                            write!(f, "{}{}i", re_str, im_str)
                        } else {
                            write!(f, "{}+{}i", re_str, im_str)
                        }
                    }
                }
            }
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
            Value::CurriedFunction {
                collected_args,
                total_params,
                ..
            } => {
                write!(
                    f,
                    "<curried function {}/{} args>",
                    collected_args.len(),
                    total_params
                )
            }
            Value::MemoizedFunction { cache, .. } => {
                write!(f, "<memoized function ({} cached)>", cache.borrow().len())
            }
        }
    }
}

// Helper function to convert irrational values to float approximations
pub fn irrational_to_float(irr: &IrrationalValue) -> f64 {
    match irr {
        IrrationalValue::Pi => std::f64::consts::PI,
        IrrationalValue::E => std::f64::consts::E,
        IrrationalValue::Sqrt(n) => {
            // Convert the inner value to float and take square root
            if let Ok(val) = value_to_float_helper(n) {
                val.sqrt()
            } else {
                f64::NAN
            }
        }
        IrrationalValue::Root(degree, n) => {
            // Convert to float and take n-th root
            if let Ok(val) = value_to_float_helper(n) {
                val.powf(1.0 / (*degree as f64))
            } else {
                f64::NAN
            }
        }
        IrrationalValue::Product(coef, inner_irr) => {
            // Multiply coefficient by the inner irrational
            if let Ok(coef_val) = value_to_float_helper(coef) {
                coef_val * irrational_to_float(inner_irr)
            } else {
                f64::NAN
            }
        }
        IrrationalValue::Sum(left, right) => {
            // Add two irrational values
            irrational_to_float(left) + irrational_to_float(right)
        }
    }
}

// Helper function to convert Value to float (used internally)
fn value_to_float_helper(val: &Value) -> Result<f64, ()> {
    match val {
        Value::Int(i) => Ok(*i as f64),
        Value::Float(f) => Ok(*f),
        Value::Rational(r) => {
            let num = r.numer().to_string().parse::<f64>().unwrap_or(0.0);
            let den = r.denom().to_string().parse::<f64>().unwrap_or(1.0);
            Ok(num / den)
        }
        Value::Irrational(irr) => Ok(irrational_to_float(irr)),
        _ => Err(()),
    }
}

fn format_irrational(irr: &IrrationalValue) -> String {
    fn simplify_sqrt(n: &Value) -> (i64, i64) {
        // Try to simplify √n into a*√b where a is the largest perfect square factor
        if let Value::Int(num) = n {
            if *num < 0 {
                return (1, *num);
            }
            let mut n = *num;
            let mut coef = 1i64;

            // Extract perfect squares
            let mut i = 2i64;
            while i * i <= n {
                let square = i * i;
                while n % square == 0 {
                    n /= square;
                    coef *= i;
                }
                i += 1;
            }

            (coef, n)
        } else {
            (1, 0)
        }
    }

    fn format_sqrt(n: &Value) -> String {
        let (coef, remaining) = simplify_sqrt(n);
        if remaining == 1 {
            format!("{}", coef)
        } else if coef == 1 {
            format!("√{}", remaining)
        } else {
            format!("{}√{}", coef, remaining)
        }
    }

    fn format_product(coef: &Value, irr: &IrrationalValue) -> String {
        // Check if this is a square (e.g., π*π or e*e)
        if let Value::Irrational(coef_irr) = coef {
            match (coef_irr, irr) {
                (IrrationalValue::Pi, IrrationalValue::Pi) => return "π^2".to_string(),
                (IrrationalValue::E, IrrationalValue::E) => return "e^2".to_string(),
                _ => {}
            }
        }

        // Flatten nested products: (a * (b * irr)) => format as a*b*irr
        if let IrrationalValue::Product(inner_coef, inner_irr) = irr {
            // If outer coef is Int/Rational and inner coef is Int/Rational, multiply them
            match (coef, inner_coef.as_ref()) {
                (Value::Int(a), Value::Int(b)) => {
                    let combined = Value::Int(a * b);
                    return format_product(&combined, inner_irr);
                }
                (Value::Rational(a), Value::Rational(b)) => {
                    let combined = Value::Rational(a * b);
                    return format_product(&combined, inner_irr);
                }
                (Value::Int(a), Value::Rational(b)) => {
                    let combined = Value::Rational(BigRational::from_integer(BigInt::from(*a)) * b);
                    return format_product(&combined, inner_irr);
                }
                (Value::Rational(a), Value::Int(b)) => {
                    let combined = Value::Rational(a * BigRational::from_integer(BigInt::from(*b)));
                    return format_product(&combined, inner_irr);
                }
                // If outer coef is Int/Rational but inner is Irrational, format as outer*inner*innerirr
                (Value::Int(_) | Value::Rational(_), Value::Irrational(_)) => {
                    let coef_str = match coef {
                        Value::Int(1) => "".to_string(),
                        Value::Int(n) => n.to_string(),
                        Value::Rational(r) => format!("{}", r),
                        _ => unreachable!(),
                    };
                    let inner_str = format_irrational(irr);
                    if coef_str.is_empty() {
                        return inner_str;
                    } else {
                        return format!("{}{}", coef_str, inner_str);
                    }
                }
                _ => {
                    // Other cases, format as-is
                    let coef_str = match coef {
                        Value::Int(1) => return format_irrational(irr),
                        Value::Int(n) => n.to_string(),
                        Value::Irrational(i) => format_irrational(i),
                        other => other.to_string(),
                    };
                    let irr_str = format_irrational(irr);
                    return format!("{}*{}", coef_str, irr_str);
                }
            }
        }

        let coef_str = match coef {
            Value::Int(1) => return format_irrational(irr),
            Value::Int(n) => n.to_string(),
            Value::Rational(r)
                if r.numer() == &BigInt::from(1) && r.denom() == &BigInt::from(1) =>
            {
                return format_irrational(irr);
            }
            Value::Irrational(i) => format_irrational(i),
            other => other.to_string(),
        };

        let irr_str = format_irrational(irr);

        // Special formatting for cleaner output
        match irr {
            IrrationalValue::Pi | IrrationalValue::E | IrrationalValue::Sqrt(_) => {
                format!("{}{}", coef_str, irr_str)
            }
            _ => format!("{}*{}", coef_str, irr_str),
        }
    }

    fn format_sum_flat(irr: &IrrationalValue, terms: &mut Vec<String>) {
        match irr {
            IrrationalValue::Sum(a, b) => {
                format_sum_flat(a, terms);
                format_sum_flat(b, terms);
            }
            other => {
                terms.push(format_irrational(other));
            }
        }
    }

    match irr {
        IrrationalValue::Sqrt(n) => format_sqrt(n),
        IrrationalValue::Root(degree, n) => {
            // Format n-th root
            match degree {
                2 => format_sqrt(n), // Use sqrt notation for square roots
                _ => format!("{}√{}", degree, n),
            }
        }
        IrrationalValue::Pi => "π".to_string(),
        IrrationalValue::E => "e".to_string(),
        IrrationalValue::Product(coef, inner_irr) => format_product(coef, inner_irr),
        IrrationalValue::Sum(_, _) => {
            let mut terms = Vec::new();
            format_sum_flat(irr, &mut terms);
            terms.join("+")
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Complex(a_re, a_im), Value::Complex(b_re, b_im)) => {
                a_re == b_re && a_im == b_im
            }
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Null, Value::Null) => true,
            _ => false,
        }
    }
}
