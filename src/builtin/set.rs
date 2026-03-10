// LSR-006: Set 类型函数
use crate::ast::BinOp;
use crate::value::Value;

pub fn set_constructor(args: &[Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Err("Set constructor expects at least 1 argument".to_string());
    }

    match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            Ok(Value::Set(arr.clone()))
        }
        Value::Set(values) => Ok(Value::Set(values.clone())),
        other => Ok(Value::Set(vec![other.clone()])),
    }
}

pub fn set_get(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("set_get expects 2 arguments (set, index)".to_string());
    }

    match (&args[0], &args[1]) {
        (Value::Set(values), Value::Int(index)) => {
            let idx = *index as usize;
            values.get(idx).cloned().ok_or_else(|| {
                format!(
                    "Index {} out of bounds for set of size {}",
                    index,
                    values.len()
                )
            })
        }
        _ => Err(format!(
            "set_get expects (set, int), got ({}, {})",
            args[0].type_name(),
            args[1].type_name()
        )),
    }
}

pub fn set_main_value(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("set_main_value expects 1 argument (set)".to_string());
    }

    match &args[0] {
        Value::Set(values) => values
            .first()
            .cloned()
            .ok_or_else(|| "Cannot get main value from empty set".to_string()),
        _ => Err(format!(
            "set_main_value expects set, got {}",
            args[0].type_name()
        )),
    }
}

pub fn set_get_real(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("set_get_real expects 1 argument (set)".to_string());
    }

    match &args[0] {
        Value::Set(values) => {
            let real_values: Vec<Value> = values
                .iter()
                .filter(|v| {
                    matches!(
                        v,
                        Value::Int(_) | Value::Float(_) | Value::BigInt(_) | Value::Rational(_)
                    )
                })
                .cloned()
                .collect();
            Ok(Value::Set(real_values))
        }
        _ => Err(format!(
            "set_get_real expects set, got {}",
            args[0].type_name()
        )),
    }
}

pub fn set_to_add(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("toAdd expects 2 arguments (set, value)".to_string());
    }

    match &args[0] {
        Value::Set(values) => {
            let operand = &args[1];
            let result: Result<Vec<Value>, String> = match operand {
                Value::Set(other_values) => {
                    let mut results = Vec::new();
                    for v1 in values {
                        for v2 in other_values {
                            results.push(crate::value_ops::value_binary_op(v1, BinOp::Add, v2)?);
                        }
                    }
                    Ok(results)
                }
                _ => values
                    .iter()
                    .map(|v| crate::value_ops::value_binary_op(v, BinOp::Add, operand))
                    .collect(),
            };
            Ok(Value::Set(result?))
        }
        _ => Err(format!("toAdd expects set, got {}", args[0].type_name())),
    }
}

pub fn set_to_sub(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("toSub expects 2 arguments (set, value)".to_string());
    }

    match &args[0] {
        Value::Set(values) => {
            let operand = &args[1];
            let result: Result<Vec<Value>, String> = match operand {
                Value::Set(other_values) => {
                    let mut results = Vec::new();
                    for v1 in values {
                        for v2 in other_values {
                            results.push(crate::value_ops::value_binary_op(v1, BinOp::Sub, v2)?);
                        }
                    }
                    Ok(results)
                }
                _ => values
                    .iter()
                    .map(|v| crate::value_ops::value_binary_op(v, BinOp::Sub, operand))
                    .collect(),
            };
            Ok(Value::Set(result?))
        }
        _ => Err(format!("toSub expects set, got {}", args[0].type_name())),
    }
}

pub fn set_to_multiply(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("toMultiply expects 2 arguments (set, value)".to_string());
    }

    match &args[0] {
        Value::Set(values) => {
            let operand = &args[1];
            let result: Result<Vec<Value>, String> = match operand {
                Value::Set(other_values) => {
                    let mut results = Vec::new();
                    for v1 in values {
                        for v2 in other_values {
                            results.push(crate::value_ops::value_binary_op(v1, BinOp::Mul, v2)?);
                        }
                    }
                    Ok(results)
                }
                _ => values
                    .iter()
                    .map(|v| crate::value_ops::value_binary_op(v, BinOp::Mul, operand))
                    .collect(),
            };
            Ok(Value::Set(result?))
        }
        _ => Err(format!(
            "toMultiply expects set, got {}",
            args[0].type_name()
        )),
    }
}

pub fn set_to_divide(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("toDivide expects 2 arguments (set, value)".to_string());
    }

    match &args[0] {
        Value::Set(values) => {
            let operand = &args[1];
            let result: Result<Vec<Value>, String> = match operand {
                Value::Set(other_values) => {
                    let mut results = Vec::new();
                    for v1 in values {
                        for v2 in other_values {
                            results.push(crate::value_ops::value_binary_op(v1, BinOp::Div, v2)?);
                        }
                    }
                    Ok(results)
                }
                _ => values
                    .iter()
                    .map(|v| crate::value_ops::value_binary_op(v, BinOp::Div, operand))
                    .collect(),
            };
            Ok(Value::Set(result?))
        }
        _ => Err(format!("toDivide expects set, got {}", args[0].type_name())),
    }
}

pub fn set_to_pow(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("toPow expects 2 arguments (set, exponent)".to_string());
    }

    match &args[0] {
        Value::Set(values) => {
            let operand = &args[1];
            let result: Result<Vec<Value>, String> = match operand {
                Value::Set(other_values) => {
                    let mut results = Vec::new();
                    for v1 in values {
                        for v2 in other_values {
                            results.push(crate::value_ops::value_binary_op(v1, BinOp::Pow, v2)?);
                        }
                    }
                    Ok(results)
                }
                _ => values
                    .iter()
                    .map(|v| crate::value_ops::value_binary_op(v, BinOp::Pow, operand))
                    .collect(),
            };
            Ok(Value::Set(result?))
        }
        _ => Err(format!("toPow expects set, got {}", args[0].type_name())),
    }
}

pub fn set_to_sqrt(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("toSqrt expects 1 argument (set)".to_string());
    }

    match &args[0] {
        Value::Set(values) => {
            let result: Result<Vec<Value>, String> = values
                .iter()
                .map(|v| crate::builtin::math::sqrt(std::slice::from_ref(v)))
                .collect();
            Ok(Value::Set(result?))
        }
        _ => Err(format!("toSqrt expects set, got {}", args[0].type_name())),
    }
}

pub fn set_to_sin(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("toSin expects 1 argument (set)".to_string());
    }

    match &args[0] {
        Value::Set(values) => {
            let result: Result<Vec<Value>, String> = values
                .iter()
                .map(|v| crate::builtin::math::sin(std::slice::from_ref(v)))
                .collect();
            Ok(Value::Set(result?))
        }
        _ => Err(format!("toSin expects set, got {}", args[0].type_name())),
    }
}

pub fn set_to_cos(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("toCos expects 1 argument (set)".to_string());
    }

    match &args[0] {
        Value::Set(values) => {
            let result: Result<Vec<Value>, String> = values
                .iter()
                .map(|v| crate::builtin::math::cos(std::slice::from_ref(v)))
                .collect();
            Ok(Value::Set(result?))
        }
        _ => Err(format!("toCos expects set, got {}", args[0].type_name())),
    }
}

pub fn set_to_tangent(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("toTangent expects 1 argument (set)".to_string());
    }

    match &args[0] {
        Value::Set(values) => {
            let result: Result<Vec<Value>, String> = values
                .iter()
                .map(|v| crate::builtin::math::tan(std::slice::from_ref(v)))
                .collect();
            Ok(Value::Set(result?))
        }
        _ => Err(format!(
            "toTangent expects set, got {}",
            args[0].type_name()
        )),
    }
}
