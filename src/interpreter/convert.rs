/// Type conversion implementations for LSR-005
use crate::ast::DeclaredType;
use crate::numeric::{BigInt, BigIntExt, BigRationalExt, rational_from_f64, rational_new};
use crate::value::{IrrationalValue, Value};

pub(crate) fn convert_to_declared_type(val: Value, dtype: &DeclaredType) -> Result<Value, String> {
    match dtype {
        DeclaredType::Num => convert_to_num(val),
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

fn convert_to_num(val: Value) -> Result<Value, String> {
    match val {
        Value::Int(_)
        | Value::BigInt(_)
        | Value::Float(_)
        | Value::Rational(_)
        | Value::Irrational(_)
        | Value::Complex(_, _) => Ok(val),
        _ => Err(format!("Cannot convert {} to num", val.type_name())),
    }
}

pub(super) fn convert_to_int(val: Value) -> Result<Value, String> {
    match val {
        Value::Int(_) => Ok(val),
        Value::BigInt(n) => n
            .to_i64_checked()
            .map(Value::Int)
            .ok_or_else(|| "BigInt too large to convert to int".to_string()),
        Value::Float(f) => Ok(Value::Int(f as i64)),
        Value::Bool(b) => Ok(Value::Int(if b { 1 } else { 0 })),
        Value::String(s) => s
            .parse::<i64>()
            .map(Value::Int)
            .map_err(|_| format!("Cannot convert string '{}' to int", s)),
        Value::Rational(r) => {
            let n = r
                .to_f64_checked()
                .ok_or("Cannot convert rational to float")? as i64;
            Ok(Value::Int(n))
        }
        _ => Err(format!("Cannot convert {} to int", val.type_name())),
    }
}

pub(super) fn convert_to_float(val: Value) -> Result<Value, String> {
    match val {
        Value::Float(_) => Ok(val),
        Value::Int(n) => Ok(Value::Float(n as f64)),
        Value::BigInt(n) => n
            .to_f64_checked()
            .map(Value::Float)
            .ok_or_else(|| "BigInt too large to convert to float".to_string()),
        Value::Bool(b) => Ok(Value::Float(if b { 1.0 } else { 0.0 })),
        Value::String(s) => s
            .parse::<f64>()
            .map(Value::Float)
            .map_err(|_| format!("Cannot convert string '{}' to float", s)),
        Value::Rational(r) => {
            let n = r
                .to_f64_checked()
                .ok_or("Cannot convert rational to float")?;
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
        Value::Int(n) => Ok(Value::Rational(rational_new(
            BigInt::from(n),
            BigInt::from(1),
        ))),
        Value::Float(f) => {
            Ok(Value::Rational(rational_from_f64(f).unwrap_or_else(|| {
                rational_new(BigInt::from(0), BigInt::from(1))
            })))
        }
        Value::Bool(b) => {
            let n = if b { 1 } else { 0 };
            Ok(Value::Rational(rational_new(
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

#[cfg(test)]
#[allow(clippy::approx_constant)]
mod tests {
    use super::*;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn test_convert_to_int() {
        assert!(matches!(convert_to_int(Value::Int(42)), Ok(Value::Int(42))));
        assert!(matches!(
            convert_to_int(Value::Float(3.14)),
            Ok(Value::Int(3))
        ));
        assert!(matches!(
            convert_to_int(Value::Bool(true)),
            Ok(Value::Int(1))
        ));
        assert!(matches!(
            convert_to_int(Value::Bool(false)),
            Ok(Value::Int(0))
        ));
        assert!(matches!(
            convert_to_int(Value::String("123".to_string())),
            Ok(Value::Int(123))
        ));
        assert!(convert_to_int(Value::String("abc".to_string())).is_err());
    }

    #[test]
    fn test_convert_to_float() {
        assert!(matches!(
            convert_to_float(Value::Float(3.14)),
            Ok(Value::Float(_))
        ));
        assert!(matches!(
            convert_to_float(Value::Int(42)),
            Ok(Value::Float(_))
        ));
        assert!(matches!(
            convert_to_float(Value::Bool(true)),
            Ok(Value::Float(_))
        ));
        assert!(matches!(
            convert_to_float(Value::String("3.14".to_string())),
            Ok(Value::Float(_))
        ));
        assert!(convert_to_float(Value::String("abc".to_string())).is_err());
    }

    #[test]
    fn test_convert_to_bool() {
        assert!(matches!(
            convert_to_bool(Value::Int(1)),
            Ok(Value::Bool(true))
        ));
        assert!(matches!(
            convert_to_bool(Value::Int(0)),
            Ok(Value::Bool(false))
        ));
    }

    #[test]
    fn test_convert_to_string() {
        assert!(matches!(
            convert_to_string(Value::Int(42)),
            Ok(Value::String(_))
        ));
        assert!(matches!(
            convert_to_string(Value::Float(3.14)),
            Ok(Value::String(_))
        ));
    }

    #[test]
    fn test_convert_to_rational() {
        assert!(matches!(
            convert_to_rational(Value::Int(42)),
            Ok(Value::Rational(_))
        ));
        assert!(matches!(
            convert_to_rational(Value::Float(3.14)),
            Ok(Value::Rational(_))
        ));
        assert!(matches!(
            convert_to_rational(Value::Bool(true)),
            Ok(Value::Rational(_))
        ));
        assert!(convert_to_rational(Value::String("test".to_string())).is_err());
    }

    #[test]
    fn test_convert_to_irrational() {
        assert!(matches!(
            convert_to_irrational(Value::Int(4)),
            Ok(Value::Irrational(_))
        ));
        assert!(convert_to_irrational(Value::Float(3.14)).is_err());
    }

    #[test]
    fn test_convert_to_complex() {
        assert!(matches!(
            convert_to_complex(Value::Int(42)),
            Ok(Value::Complex(_, _))
        ));
        assert!(matches!(
            convert_to_complex(Value::Float(3.14)),
            Ok(Value::Complex(_, _))
        ));
        assert!(convert_to_complex(Value::String("test".to_string())).is_err());
    }

    #[test]
    fn test_convert_to_array() {
        assert!(matches!(
            convert_to_array(Value::Array(Rc::new(RefCell::new(vec![])))),
            Ok(Value::Array(_))
        ));
        assert!(convert_to_array(Value::Int(42)).is_err());
    }

    #[test]
    fn test_convert_to_bigint() {
        assert!(matches!(
            convert_to_bigint(Value::Int(42)),
            Ok(Value::BigInt(_))
        ));
        assert!(matches!(
            convert_to_bigint(Value::BigInt(BigInt::from(100))),
            Ok(Value::BigInt(_))
        ));
        assert!(convert_to_bigint(Value::Float(3.14)).is_err());
    }

    #[test]
    fn test_convert_to_declared_type() {
        assert!(convert_to_declared_type(Value::Int(42), &DeclaredType::Int).is_ok());
        assert!(convert_to_declared_type(Value::Int(42), &DeclaredType::Float).is_ok());
        assert!(convert_to_declared_type(Value::Int(42), &DeclaredType::String).is_ok());
    }
}
