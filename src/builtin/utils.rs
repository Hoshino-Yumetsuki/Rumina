// 工具函数模块
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{self, Write};
use std::rc::Rc;

pub fn print(args: &[Value]) -> Result<Value, String> {
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        // 如果是浮点数，尝试转换为分数显示
        match arg {
            Value::Float(f) => {
                if let Some(rational) = float_to_rational(*f) {
                    print!("{}", rational);
                } else {
                    print!("{}", f);
                }
            }
            _ => print!("{}", arg),
        }
    }
    println!();
    Ok(Value::Null)
}

// 将浮点数转换为分数（简单实现）
fn float_to_rational(f: f64) -> Option<String> {
    if !f.is_finite() {
        return None;
    }

    // 使用continued fraction方法转换
    let precision = 1e-10;
    let mut h1 = 1i64;
    let mut h2 = 0i64;
    let mut k1 = 0i64;
    let mut k2 = 1i64;
    let mut b = f;

    for _ in 0..100 {
        let a = b.floor() as i64;
        let mut aux = h1;
        h1 = a * h1 + h2;
        h2 = aux;
        aux = k1;
        k1 = a * k1 + k2;
        k2 = aux;

        if (f - h1 as f64 / k1 as f64).abs() < precision {
            return Some(format!("{}/{}", h1, k1));
        }

        b = 1.0 / (b - a as f64);
        if !b.is_finite() {
            break;
        }
    }

    None
}

pub fn input(args: &[Value]) -> Result<Value, String> {
    if args.len() > 0 {
        print!("{}", args[0]);
        io::stdout().flush().unwrap();
    }

    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    let input = input.trim().to_string();

    // 尝试解析为数字
    if let Ok(n) = input.parse::<i64>() {
        Ok(Value::Int(n))
    } else if let Ok(f) = input.parse::<f64>() {
        Ok(Value::Float(f))
    } else {
        Ok(Value::String(input))
    }
}

pub fn typeof_fn(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("typeof expects 1 argument".to_string());
    }
    Ok(Value::String(args[0].type_name().to_string()))
}

pub fn size(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("size expects 1 argument".to_string());
    }

    match &args[0] {
        Value::Array(arr) => Ok(Value::Int(arr.borrow().len() as i64)),
        Value::Struct(s) => Ok(Value::Int(s.borrow().len() as i64)),
        Value::String(s) => Ok(Value::Int(s.chars().count() as i64)),
        _ => Err(format!(
            "size expects array/struct/string, got {}",
            args[0].type_name()
        )),
    }
}

pub fn tostring(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("tostring expects 1 argument".to_string());
    }
    Ok(Value::String(format!("{}", args[0])))
}

pub fn to_string(args: &[Value]) -> Result<Value, String> {
    tostring(args)
}

pub fn exit(args: &[Value]) -> Result<Value, String> {
    let code = if args.len() > 0 {
        args[0].to_int().unwrap_or(0)
    } else {
        0
    };
    std::process::exit(code as i32);
}

pub fn new_fn(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("new expects 1 argument (struct)".to_string());
    }

    match &args[0] {
        Value::Struct(s) => {
            // 创建结构体的深拷贝
            let original = s.borrow();
            let mut new_struct = HashMap::new();

            for (key, value) in original.iter() {
                new_struct.insert(key.clone(), value.clone());
            }

            // 添加 __parent__ 引用
            new_struct.insert("__parent__".to_string(), Value::Struct(Rc::clone(s)));

            Ok(Value::Struct(Rc::new(RefCell::new(new_struct))))
        }
        _ => Err(format!("new expects struct, got {}", args[0].type_name())),
    }
}

pub fn same(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("same expects 2 arguments".to_string());
    }

    let result = match (&args[0], &args[1]) {
        (Value::Struct(a), Value::Struct(b)) => Rc::ptr_eq(a, b),
        _ => args[0] == args[1],
    };

    Ok(Value::Bool(result))
}

pub fn setattr(args: &[Value]) -> Result<Value, String> {
    if args.len() != 3 {
        return Err("setattr expects 3 arguments (object, key, value)".to_string());
    }

    match &args[0] {
        Value::Struct(s) => {
            if let Value::String(key) = &args[1] {
                s.borrow_mut().insert(key.clone(), args[2].clone());
                Ok(Value::Null)
            } else {
                Err("setattr expects string key".to_string())
            }
        }
        _ => Err(format!(
            "setattr expects struct, got {}",
            args[0].type_name()
        )),
    }
}

pub fn update(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("update expects 2 arguments (target, source)".to_string());
    }

    match (&args[0], &args[1]) {
        (Value::Struct(target), Value::Struct(source)) => {
            let source_borrowed = source.borrow();
            let mut target_borrowed = target.borrow_mut();

            for (key, value) in source_borrowed.iter() {
                target_borrowed.insert(key.clone(), value.clone());
            }

            Ok(Value::Null)
        }
        _ => Err("update expects two structs".to_string()),
    }
}

// Lamina-compliant: fraction() - convert float to rational
pub fn fraction(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("fraction expects 1 argument".to_string());
    }

    match &args[0] {
        Value::Float(f) => {
            if !f.is_finite() {
                return Err("Cannot convert infinite or NaN to fraction".to_string());
            }

            // Use continued fraction method to convert
            let precision = 1e-10;
            let mut h1 = 1i64;
            let mut h2 = 0i64;
            let mut k1 = 0i64;
            let mut k2 = 1i64;
            let mut b = *f;

            for _ in 0..100 {
                let a = b.floor() as i64;
                let mut aux = h1;
                h1 = a * h1 + h2;
                h2 = aux;
                aux = k1;
                k1 = a * k1 + k2;
                k2 = aux;

                if (f - h1 as f64 / k1 as f64).abs() < precision {
                    use num::BigInt;
                    return Ok(Value::Rational(num::rational::Ratio::new(
                        BigInt::from(h1),
                        BigInt::from(k1),
                    )));
                }

                b = 1.0 / (b - a as f64);
                if !b.is_finite() {
                    break;
                }
            }

            // Fallback: return as rational approximation
            use num::BigInt;
            Ok(Value::Rational(
                num::rational::Ratio::from_float(*f)
                    .unwrap_or(num::rational::Ratio::new(BigInt::from(0), BigInt::from(1))),
            ))
        }
        Value::Int(i) => {
            use num::BigInt;
            Ok(Value::Rational(num::rational::Ratio::new(
                BigInt::from(*i),
                BigInt::from(1),
            )))
        }
        Value::Rational(r) => Ok(Value::Rational(r.clone())),
        _ => Err(format!(
            "Cannot convert {} to fraction",
            args[0].type_name()
        )),
    }
}

// Lamina-compliant: decimal() - convert rational to float
pub fn decimal(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("decimal expects 1 argument".to_string());
    }

    match &args[0] {
        Value::Rational(r) => {
            use num::ToPrimitive;
            let numer = r.numer().to_f64().ok_or("Numerator too large to convert")?;
            let denom = r
                .denom()
                .to_f64()
                .ok_or("Denominator too large to convert")?;
            Ok(Value::Float(numer / denom))
        }
        Value::Int(i) => Ok(Value::Float(*i as f64)),
        Value::Float(f) => Ok(Value::Float(*f)),
        Value::Complex(re, im) => {
            // Convert symbolic complex to float-based representation
            let re_float = match re.as_ref() {
                Value::Int(i) => *i as f64,
                Value::Float(f) => *f,
                Value::Rational(r) => {
                    use num::ToPrimitive;
                    let numer = r.numer().to_f64().ok_or("Numerator too large")?;
                    let denom = r.denom().to_f64().ok_or("Denominator too large")?;
                    numer / denom
                }
                Value::Irrational(irr) => {
                    // Simple conversions for basic irrationals
                    match irr {
                        crate::value::IrrationalValue::Pi => std::f64::consts::PI,
                        crate::value::IrrationalValue::E => std::f64::consts::E,
                        crate::value::IrrationalValue::Sqrt(n) => {
                            let n_val = match n.as_ref() {
                                Value::Int(i) => *i as f64,
                                Value::Float(f) => *f,
                                _ => return Err("Cannot convert complex irrational to decimal".to_string()),
                            };
                            n_val.sqrt()
                        }
                        _ => return Err("Cannot convert composite irrational to decimal".to_string()),
                    }
                }
                _ => return Err("Cannot convert complex real part to decimal".to_string()),
            };

            let im_float = match im.as_ref() {
                Value::Int(i) => *i as f64,
                Value::Float(f) => *f,
                Value::Rational(r) => {
                    use num::ToPrimitive;
                    let numer = r.numer().to_f64().ok_or("Numerator too large")?;
                    let denom = r.denom().to_f64().ok_or("Denominator too large")?;
                    numer / denom
                }
                Value::Irrational(irr) => {
                    match irr {
                        crate::value::IrrationalValue::Pi => std::f64::consts::PI,
                        crate::value::IrrationalValue::E => std::f64::consts::E,
                        crate::value::IrrationalValue::Sqrt(n) => {
                            let n_val = match n.as_ref() {
                                Value::Int(i) => *i as f64,
                                Value::Float(f) => *f,
                                _ => return Err("Cannot convert complex irrational to decimal".to_string()),
                            };
                            n_val.sqrt()
                        }
                        _ => return Err("Cannot convert composite irrational to decimal".to_string()),
                    }
                }
                _ => return Err("Cannot convert complex imaginary part to decimal".to_string()),
            };

            // Return as a string representation of complex number in float form
            use num::complex::Complex64;
            let c = Complex64::new(re_float, im_float);
            if c.im >= 0.0 {
                Ok(Value::String(format!("{}+{}i", c.re, c.im)))
            } else {
                Ok(Value::String(format!("{}{}i", c.re, c.im)))
            }
        }
        _ => Err(format!("Cannot convert {} to decimal", args[0].type_name())),
    }
}
