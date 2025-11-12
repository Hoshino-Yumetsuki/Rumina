/// Type conversion implementations for LSR-005
use crate::ast::DeclaredType;
use crate::value::{IrrationalValue, Value};
use num::BigInt;

pub(crate) fn convert_to_declared_type(val: Value, dtype: &DeclaredType) -> Result<Value, String> {
    match dtype {
        DeclaredType::Int => convert_to_int(val),
        DeclaredType::Float => convert_to_float(val),
        DeclaredType::Bool => convert_to_bool(val),
        DeclaredType::String => convert_to_string(val),
        DeclaredType::Rational => convert_to_rational(val),
        DeclaredType::Irrational => convert_to_irrational(val),
        DeclaredType::Complex => convert_to_complex(val),
        DeclaredType::Array => convert_to_array(val),
        DeclaredType::BigInt => convert_to_bigint(val),
    }
}

pub(super) fn convert_to_int(val: Value) -> Result<Value, String> {
    match val {
        Value::Int(_) => Ok(val),
        Value::BigInt(n) => {
            use num::ToPrimitive;
            n.to_i64()
                .map(Value::Int)
                .ok_or_else(|| "BigInt too large to convert to int".to_string())
        }
        Value::Float(f) => Ok(Value::Int(f as i64)),
        Value::Bool(b) => Ok(Value::Int(if b { 1 } else { 0 })),
        Value::String(s) => s
            .parse::<i64>()
            .map(Value::Int)
            .map_err(|_| format!("Cannot convert string '{}' to int", s)),
        Value::Rational(r) => {
            use num::ToPrimitive;
            let n = r.to_f64().ok_or("Cannot convert rational to float")? as i64;
            Ok(Value::Int(n))
        }
        _ => Err(format!("Cannot convert {} to int", val.type_name())),
    }
}

pub(super) fn convert_to_float(val: Value) -> Result<Value, String> {
    match val {
        Value::Float(_) => Ok(val),
        Value::Int(n) => Ok(Value::Float(n as f64)),
        Value::BigInt(n) => {
            use num::ToPrimitive;
            n.to_f64()
                .map(Value::Float)
                .ok_or_else(|| "BigInt too large to convert to float".to_string())
        }
        Value::Bool(b) => Ok(Value::Float(if b { 1.0 } else { 0.0 })),
        Value::String(s) => s
            .parse::<f64>()
            .map(Value::Float)
            .map_err(|_| format!("Cannot convert string '{}' to float", s)),
        Value::Rational(r) => {
            use num::ToPrimitive;
            let n = r.to_f64().ok_or("Cannot convert rational to float")?;
            Ok(Value::Float(n))
        }
        _ => Err(format!("Cannot convert {} to float", val.type_name())),
    }
}

pub(super) fn convert_to_bool(val: Value) -> Result<Value, String> {
    Ok(Value::Bool(val.is_truthy()))
}

pub(super) fn convert_to_string(val: Value) -> Result<Value, String> {
    Ok(Value::String(format!("{}", val)))
}

pub(super) fn convert_to_rational(val: Value) -> Result<Value, String> {
    match val {
        Value::Rational(_) => Ok(val),
        Value::Int(n) => Ok(Value::Rational(num::rational::Ratio::new(
            BigInt::from(n),
            BigInt::from(1),
        ))),
        Value::Float(f) => Ok(Value::Rational(
            num::rational::Ratio::from_float(f)
                .unwrap_or(num::rational::Ratio::new(BigInt::from(0), BigInt::from(1))),
        )),
        Value::Bool(b) => {
            let n = if b { 1 } else { 0 };
            Ok(Value::Rational(num::rational::Ratio::new(
                BigInt::from(n),
                BigInt::from(1),
            )))
        }
        _ => Err(format!("Cannot convert {} to rational", val.type_name())),
    }
}

pub(super) fn convert_to_irrational(val: Value) -> Result<Value, String> {
    match val {
        Value::Irrational(_) => Ok(val),
        Value::Int(n) => Ok(Value::Irrational(IrrationalValue::Sqrt(Box::new(
            Value::Int(n * n),
        )))),
        _ => Err(format!("Cannot convert {} to irrational", val.type_name())),
    }
}

pub(super) fn convert_to_complex(val: Value) -> Result<Value, String> {
    match val {
        Value::Complex(_, _) => Ok(val),
        Value::Int(n) => Ok(Value::Complex(
            Box::new(Value::Int(n)),
            Box::new(Value::Int(0)),
        )),
        Value::Float(f) => Ok(Value::Complex(
            Box::new(Value::Float(f)),
            Box::new(Value::Int(0)),
        )),
        _ => Err(format!("Cannot convert {} to complex", val.type_name())),
    }
}

pub(super) fn convert_to_array(val: Value) -> Result<Value, String> {
    match val {
        Value::Array(_) => Ok(val),
        _ => Err(format!("Cannot convert {} to array", val.type_name())),
    }
}

pub(super) fn convert_to_bigint(val: Value) -> Result<Value, String> {
    match val {
        Value::Int(n) => Ok(Value::BigInt(BigInt::from(n))),
        Value::BigInt(_) => Ok(val),
        _ => Err(format!("Cannot convert {} to bigint", val.type_name())),
    }
}
