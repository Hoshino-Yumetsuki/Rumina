// 数学函数模块
use crate::value::{IrrationalValue, Value};
use num_bigint::BigInt;
use num_traits::{One, Zero};

pub fn sqrt(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("sqrt expects 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => {
            if *n < 0 {
                return Err("Cannot take square root of negative number".to_string());
            }
            let sqrt = (*n as f64).sqrt();
            if sqrt.fract() == 0.0 {
                Ok(Value::Int(sqrt as i64))
            } else {
                Ok(Value::Irrational(IrrationalValue::Sqrt(Box::new(
                    Value::Int(*n),
                ))))
            }
        }
        Value::Float(f) => Ok(Value::Float(f.sqrt())),
        _ => Err(format!("sqrt expects number, got {}", args[0].type_name())),
    }
}

pub fn pi(_args: &[Value]) -> Result<Value, String> {
    Ok(Value::Irrational(IrrationalValue::Pi))
}

pub fn e(_args: &[Value]) -> Result<Value, String> {
    Ok(Value::Irrational(IrrationalValue::E))
}

pub fn sin(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("sin expects 1 argument".to_string());
    }
    let val = args[0].to_float()?;
    Ok(Value::Float(val.sin()))
}

pub fn cos(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("cos expects 1 argument".to_string());
    }
    let val = args[0].to_float()?;
    Ok(Value::Float(val.cos()))
}

pub fn abs_fn(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("abs expects 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Int(n.abs())),
        Value::Float(f) => Ok(Value::Float(f.abs())),
        _ => Err(format!("abs expects number, got {}", args[0].type_name())),
    }
}

pub fn log(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("log expects 1 argument".to_string());
    }
    let val = args[0].to_float()?;
    Ok(Value::Float(val.ln()))
}

pub fn factorial(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("factorial expects 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => {
            if *n < 0 {
                return Err("Factorial of negative number".to_string());
            }
            let mut result = BigInt::one();
            for i in 2..=*n {
                result *= i;
            }
            Ok(Value::BigInt(result))
        }
        Value::BigInt(n) => {
            if n < &BigInt::zero() {
                return Err("Factorial of negative number".to_string());
            }
            let mut result = BigInt::one();
            let mut i = BigInt::from(2);
            while &i <= n {
                result *= &i;
                i += 1;
            }
            Ok(Value::BigInt(result))
        }
        _ => Err(format!(
            "factorial expects integer, got {}",
            args[0].type_name()
        )),
    }
}
