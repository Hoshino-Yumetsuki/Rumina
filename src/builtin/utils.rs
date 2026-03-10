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
    if !args.is_empty() {
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
    let code = if !args.is_empty() {
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
    if args.is_empty() || args.len() > 2 {
        return Err("decimal expects 1 or 2 arguments".to_string());
    }

    let precision = if args.len() == 2 {
        match &args[1] {
            Value::Int(n) if *n >= 0 && *n <= 15 => *n as i32,
            _ => return Err("decimal precision must be a non-negative integer <= 15".to_string()),
        }
    } else {
        -1
    };

    let apply_precision = |f: f64| -> Value {
        if precision >= 0 {
            let factor = 10_f64.powi(precision);
            Value::Float((f * factor).round() / factor)
        } else {
            Value::Float(f)
        }
    };

    match &args[0] {
        Value::Rational(r) => {
            use num::ToPrimitive;
            let numer = r.numer().to_f64().ok_or("Numerator too large to convert")?;
            let denom = r
                .denom()
                .to_f64()
                .ok_or("Denominator too large to convert")?;
            Ok(apply_precision(numer / denom))
        }
        Value::Int(i) => Ok(apply_precision(*i as f64)),
        Value::Float(f) => Ok(apply_precision(*f)),
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
                                _ => {
                                    return Err(
                                        "Cannot convert complex irrational to decimal".to_string()
                                    );
                                }
                            };
                            n_val.sqrt()
                        }
                        _ => {
                            return Err(
                                "Cannot convert composite irrational to decimal".to_string()
                            );
                        }
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
                Value::Irrational(irr) => match irr {
                    crate::value::IrrationalValue::Pi => std::f64::consts::PI,
                    crate::value::IrrationalValue::E => std::f64::consts::E,
                    crate::value::IrrationalValue::Sqrt(n) => {
                        let n_val = match n.as_ref() {
                            Value::Int(i) => *i as f64,
                            Value::Float(f) => *f,
                            _ => {
                                return Err(
                                    "Cannot convert complex irrational to decimal".to_string()
                                );
                            }
                        };
                        n_val.sqrt()
                    }
                    _ => return Err("Cannot convert composite irrational to decimal".to_string()),
                },
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

// Lamina-compliant: assert() - assertion function
// Usage: assert(condition) or assert(condition, "error message")
pub fn assert(args: &[Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("assert expects at least 1 argument".to_string());
    }

    if args.len() > 2 {
        return Err("assert expects at most 2 arguments".to_string());
    }

    let condition = &args[0];

    // Check if condition is truthy
    if !condition.is_truthy() {
        // Get error message (use provided message or default)
        let message = if args.len() == 2 {
            match &args[1] {
                Value::String(s) => s.clone(),
                other => format!("Assertion failed: {}", other),
            }
        } else {
            "Assertion failed".to_string()
        };

        return Err(message);
    }

    Ok(Value::Null)
}

// LSR-005: Type conversion functions
pub fn to_int(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("int expects 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(_) => Ok(args[0].clone()),
        Value::BigInt(n) => {
            use num::ToPrimitive;
            n.to_i64()
                .map(Value::Int)
                .ok_or_else(|| "BigInt too large to convert to int".to_string())
        }
        Value::Float(f) => Ok(Value::Int(*f as i64)),
        Value::Bool(b) => Ok(Value::Int(if *b { 1 } else { 0 })),
        Value::String(s) => s
            .parse::<i64>()
            .map(Value::Int)
            .map_err(|_| format!("Cannot convert string '{}' to int", s)),
        Value::Rational(r) => {
            use num::ToPrimitive;
            let n = r.to_f64().ok_or("Cannot convert rational to float")? as i64;
            Ok(Value::Int(n))
        }
        _ => Err(format!("Cannot convert {} to int", args[0].type_name())),
    }
}

pub fn to_float(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("float expects 1 argument".to_string());
    }

    match &args[0] {
        Value::Float(_) => Ok(args[0].clone()),
        Value::Int(n) => Ok(Value::Float(*n as f64)),
        Value::BigInt(n) => {
            use num::ToPrimitive;
            n.to_f64()
                .map(Value::Float)
                .ok_or_else(|| "BigInt too large to convert to float".to_string())
        }
        Value::Bool(b) => Ok(Value::Float(if *b { 1.0 } else { 0.0 })),
        Value::String(s) => s
            .parse::<f64>()
            .map(Value::Float)
            .map_err(|_| format!("Cannot convert string '{}' to float", s)),
        Value::Rational(r) => {
            use num::ToPrimitive;
            let n = r.to_f64().ok_or("Cannot convert rational to float")?;
            Ok(Value::Float(n))
        }
        _ => Err(format!("Cannot convert {} to float", args[0].type_name())),
    }
}

pub fn to_bool(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("bool expects 1 argument".to_string());
    }

    Ok(Value::Bool(args[0].is_truthy()))
}

pub fn to_string_fn(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("string expects 1 argument".to_string());
    }

    Ok(Value::String(format!("{}", args[0])))
}

pub fn to_rational(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("rational expects 1 argument".to_string());
    }

    match &args[0] {
        Value::Rational(_) => Ok(args[0].clone()),
        Value::Int(n) => {
            use num::BigInt;
            Ok(Value::Rational(num::rational::Ratio::new(
                BigInt::from(*n),
                BigInt::from(1),
            )))
        }
        Value::Float(f) => {
            use num::BigInt;
            Ok(Value::Rational(
                num::rational::Ratio::from_float(*f)
                    .unwrap_or(num::rational::Ratio::new(BigInt::from(0), BigInt::from(1))),
            ))
        }
        Value::Bool(b) => {
            use num::BigInt;
            let n = if *b { 1 } else { 0 };
            Ok(Value::Rational(num::rational::Ratio::new(
                BigInt::from(n),
                BigInt::from(1),
            )))
        }
        _ => Err(format!(
            "Cannot convert {} to rational",
            args[0].type_name()
        )),
    }
}

pub fn to_complex(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("complex expects 1 argument".to_string());
    }

    match &args[0] {
        Value::Complex(_, _) => Ok(args[0].clone()),
        Value::Int(n) => Ok(Value::Complex(
            Box::new(Value::Int(*n)),
            Box::new(Value::Int(0)),
        )),
        Value::Float(f) => Ok(Value::Complex(
            Box::new(Value::Float(*f)),
            Box::new(Value::Int(0)),
        )),
        _ => Err(format!("Cannot convert {} to complex", args[0].type_name())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_typeof_fn() {
        assert_eq!(
            typeof_fn(&[Value::Int(42)]).unwrap(),
            Value::String("int".to_string())
        );
        assert_eq!(
            typeof_fn(&[Value::Float(3.14)]).unwrap(),
            Value::String("float".to_string())
        );
        assert!(typeof_fn(&[]).is_err());
        assert!(typeof_fn(&[Value::Int(1), Value::Int(2)]).is_err());
    }

    #[test]
    fn test_size() {
        let arr = Value::Array(Rc::new(RefCell::new(vec![Value::Int(1), Value::Int(2)])));
        assert_eq!(size(&[arr]).unwrap(), Value::Int(2));

        let s = Value::String("hello".to_string());
        assert_eq!(size(&[s]).unwrap(), Value::Int(5));

        assert!(size(&[Value::Int(42)]).is_err());
        assert!(size(&[]).is_err());
    }

    #[test]
    fn test_tostring() {
        assert_eq!(
            tostring(&[Value::Int(42)]).unwrap(),
            Value::String("42".to_string())
        );
        assert!(tostring(&[]).is_err());
    }

    #[test]
    fn test_same() {
        let s1 = Rc::new(RefCell::new(HashMap::new()));
        let s2 = Rc::clone(&s1);
        assert_eq!(
            same(&[Value::Struct(s1.clone()), Value::Struct(s2)]).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            same(&[Value::Int(5), Value::Int(5)]).unwrap(),
            Value::Bool(true)
        );
        assert!(same(&[Value::Int(1)]).is_err());
    }

    #[test]
    fn test_assert() {
        assert!(assert(&[Value::Bool(true)]).is_ok());
        assert!(assert(&[Value::Bool(false)]).is_err());
        assert!(assert(&[Value::Bool(false), Value::String("custom".to_string())]).is_err());
        assert!(assert(&[]).is_err());
    }

    #[test]
    fn test_to_int() {
        assert_eq!(to_int(&[Value::Int(42)]).unwrap(), Value::Int(42));
        assert_eq!(to_int(&[Value::Float(3.7)]).unwrap(), Value::Int(3));
        assert_eq!(to_int(&[Value::Bool(true)]).unwrap(), Value::Int(1));
        assert!(to_int(&[Value::String("abc".to_string())]).is_err());
        assert!(to_int(&[]).is_err());
    }

    #[test]
    fn test_to_float() {
        assert_eq!(to_float(&[Value::Float(3.14)]).unwrap(), Value::Float(3.14));
        assert_eq!(to_float(&[Value::Int(42)]).unwrap(), Value::Float(42.0));
        assert!(to_float(&[]).is_err());
    }

    #[test]
    fn test_to_bool() {
        assert_eq!(to_bool(&[Value::Bool(true)]).unwrap(), Value::Bool(true));
        assert_eq!(to_bool(&[Value::Int(0)]).unwrap(), Value::Bool(false));
        assert_eq!(to_bool(&[Value::Int(1)]).unwrap(), Value::Bool(true));
        assert!(to_bool(&[]).is_err());
    }

    #[test]
    fn test_to_rational() {
        use num::BigInt;
        let result = to_rational(&[Value::Int(5)]).unwrap();
        if let Value::Rational(r) = result {
            assert_eq!(*r.numer(), BigInt::from(5));
            assert_eq!(*r.denom(), BigInt::from(1));
        } else {
            panic!("Expected Rational");
        }
        assert!(to_rational(&[]).is_err());
    }

    #[test]
    fn test_new_fn() {
        let mut map = HashMap::new();
        map.insert("x".to_string(), Value::Int(10));
        let s = Value::Struct(Rc::new(RefCell::new(map)));

        let result = new_fn(&[s.clone()]).unwrap();
        if let Value::Struct(new_s) = result {
            assert_eq!(new_s.borrow().get("x"), Some(&Value::Int(10)));
            assert!(new_s.borrow().contains_key("__parent__"));
        }

        assert!(new_fn(&[]).is_err());
        assert!(new_fn(&[Value::Int(1)]).is_err());
    }

    #[test]
    fn test_setattr() {
        let s = Value::Struct(Rc::new(RefCell::new(HashMap::new())));
        assert!(setattr(&[s.clone(), Value::String("key".to_string()), Value::Int(42)]).is_ok());

        if let Value::Struct(st) = s {
            assert_eq!(st.borrow().get("key"), Some(&Value::Int(42)));
        }

        assert!(setattr(&[Value::Int(1), Value::String("k".to_string()), Value::Int(1)]).is_err());
        assert!(setattr(&[]).is_err());
    }

    #[test]
    fn test_update() {
        let target = Value::Struct(Rc::new(RefCell::new(HashMap::new())));
        let mut source_map = HashMap::new();
        source_map.insert("a".to_string(), Value::Int(1));
        let source = Value::Struct(Rc::new(RefCell::new(source_map)));

        assert!(update(&[target.clone(), source]).is_ok());

        if let Value::Struct(t) = target {
            assert_eq!(t.borrow().get("a"), Some(&Value::Int(1)));
        }

        assert!(update(&[Value::Int(1), Value::Int(2)]).is_err());
    }

    #[test]
    fn test_fraction() {
        use num::BigInt;

        let result = fraction(&[Value::Int(5)]).unwrap();
        if let Value::Rational(r) = result {
            assert_eq!(*r.numer(), BigInt::from(5));
        }

        assert!(fraction(&[Value::Float(0.5)]).is_ok());
        assert!(fraction(&[Value::Float(f64::INFINITY)]).is_err());
        assert!(fraction(&[]).is_err());
    }

    #[test]
    fn test_decimal() {
        let result = decimal(&[Value::Int(5)]).unwrap();
        assert_eq!(result, Value::Float(5.0));

        let result2 = decimal(&[Value::Float(3.14159), Value::Int(2)]).unwrap();
        assert_eq!(result2, Value::Float(3.14));

        assert!(decimal(&[]).is_err());
        assert!(decimal(&[Value::Int(1), Value::Int(20)]).is_err());
    }

    #[test]
    fn test_to_string_fn() {
        assert_eq!(
            to_string_fn(&[Value::Int(42)]).unwrap(),
            Value::String("42".to_string())
        );
        assert!(to_string_fn(&[]).is_err());
    }

    #[test]
    fn test_to_complex() {
        let result = to_complex(&[Value::Int(5)]).unwrap();
        if let Value::Complex(re, im) = result {
            assert_eq!(*re, Value::Int(5));
            assert_eq!(*im, Value::Int(0));
        }

        assert!(to_complex(&[]).is_err());
    }

    #[test]
    fn test_to_string() {
        assert_eq!(
            to_string(&[Value::Int(99)]).unwrap(),
            Value::String("99".to_string())
        );
    }

    #[test]
    fn test_size_struct() {
        let mut map = HashMap::new();
        map.insert("a".to_string(), Value::Int(1));
        map.insert("b".to_string(), Value::Int(2));
        let s = Value::Struct(Rc::new(RefCell::new(map)));
        assert_eq!(size(&[s]).unwrap(), Value::Int(2));
    }
}
