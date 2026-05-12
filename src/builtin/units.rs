use crate::value::Value;

fn ensure_number(value: &Value) -> Result<(), String> {
    match value {
        Value::Int(_)
        | Value::BigInt(_)
        | Value::Float(_)
        | Value::Rational(_)
        | Value::Irrational(_)
        | Value::Complex(_, _) => Ok(()),
        _ => Err(format!("units expects num, got {}", value.type_name())),
    }
}

pub fn strip(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("units.strip expects 1 argument".to_string());
    }
    ensure_number(&args[0])?;
    Ok(args[0].clone())
}

pub fn is_dimensionless(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("units.is_dimensionless expects 1 argument".to_string());
    }
    ensure_number(&args[0])?;
    Ok(Value::Bool(true))
}

pub fn convert(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("units.convert expects 2 arguments".to_string());
    }
    ensure_number(&args[0])?;
    let Value::String(_) = &args[1] else {
        return Err(format!("units.convert expects unit text, got {}", args[1].type_name()));
    };
    Ok(args[0].clone())
}
