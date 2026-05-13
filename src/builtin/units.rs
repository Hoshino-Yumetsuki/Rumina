use crate::value::Value;

fn unit_scale(unit: &str) -> Result<i64, String> {
    match unit {
        "" => Ok(1),
        "m" => Ok(1),
        "km" => Ok(1000),
        "s" => Ok(1),
        "h" => Ok(3600),
        "m/s" => Ok(1),
        "km/h" => Ok(1),
        _ => Err(format!("unknown unit '{}'", unit)),
    }
}

fn units_are_compatible(source: &str, target: &str) -> bool {
    source == target
        || (matches!(source, "m" | "km") && matches!(target, "m" | "km"))
        || (matches!(source, "m/s" | "km/h") && matches!(target, "m/s" | "km/h"))
}

fn scale_unit_value(value: Value, scale: i64) -> Result<Value, String> {
    match value {
        Value::Int(n) => n
            .checked_mul(scale)
            .map(Value::Int)
            .ok_or_else(|| "UnitStripOverflow: unit strip scaling overflowed i64".to_string()),
        Value::BigInt(n) => Ok(Value::BigInt(n * crate::numeric::BigInt::from(scale))),
        Value::Float(n) => Ok(Value::Float(n * scale as f64)),
        Value::Rational(n) => Ok(Value::Rational(
            n * crate::numeric::rational_from_integer(crate::numeric::BigInt::from(scale)),
        )),
        other => Err(format!("Cannot strip units from {}", other.type_name())),
    }
}

fn divide_unit_value(value: Value, scale: i64) -> Result<Value, String> {
    match value {
        Value::Int(n) if n % scale == 0 => Ok(Value::Int(n / scale)),
        Value::Int(n) => Ok(Value::Rational(crate::numeric::rational_new(
            crate::numeric::BigInt::from(n),
            crate::numeric::BigInt::from(scale),
        ))),
        Value::BigInt(n) => Ok(Value::Rational(crate::numeric::rational_new(
            n,
            crate::numeric::BigInt::from(scale),
        ))),
        Value::Float(n) => Ok(Value::Float(n / scale as f64)),
        Value::Rational(n) => Ok(Value::Rational(
            n / crate::numeric::rational_from_integer(crate::numeric::BigInt::from(scale)),
        )),
        other => Err(format!("Cannot convert units for {}", other.type_name())),
    }
}

fn multiply_unit_value_by_ratio(
    value: Value,
    numerator: i64,
    denominator: i64,
) -> Result<Value, String> {
    match value {
        Value::Int(n) if (n * numerator) % denominator == 0 => {
            Ok(Value::Int((n * numerator) / denominator))
        }
        Value::Int(n) => Ok(Value::Rational(crate::numeric::rational_new(
            crate::numeric::BigInt::from(n * numerator),
            crate::numeric::BigInt::from(denominator),
        ))),
        Value::BigInt(n) => Ok(Value::Rational(crate::numeric::rational_new(
            n * crate::numeric::BigInt::from(numerator),
            crate::numeric::BigInt::from(denominator),
        ))),
        Value::Float(n) => Ok(Value::Float(n * numerator as f64 / denominator as f64)),
        Value::Rational(n) => Ok(Value::Rational(
            n * crate::numeric::rational_new(
                crate::numeric::BigInt::from(numerator),
                crate::numeric::BigInt::from(denominator),
            ),
        )),
        other => Err(format!("Cannot convert units for {}", other.type_name())),
    }
}

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
    if let Value::UnitNumber { value, scale, .. } = &args[0] {
        return scale_unit_value((**value).clone(), *scale);
    }
    ensure_number(&args[0])?;
    Ok(args[0].clone())
}

pub fn is_dimensionless(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("units.is_dimensionless expects 1 argument".to_string());
    }
    if matches!(args[0], Value::UnitNumber { .. }) {
        return Ok(Value::Bool(false));
    }
    ensure_number(&args[0])?;
    Ok(Value::Bool(true))
}

pub fn convert(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("units.convert expects 2 arguments".to_string());
    }
    let Value::String(target_unit) = &args[1] else {
        return Err(format!(
            "units.convert expects unit text, got {}",
            args[1].type_name()
        ));
    };
    let target_scale = unit_scale(target_unit).map_err(|err| {
        format!(
            "UnitStripInvalid: invalid target unit '{}': {}",
            target_unit, err
        )
    })?;
    if let Value::UnitNumber {
        value,
        unit: source_unit,
        scale: source_scale,
    } = &args[0]
    {
        if !units_are_compatible(source_unit, target_unit) {
            return Err(format!(
                "UnitStripInvalid: cannot convert from '{}' to '{}'",
                source_unit, target_unit
            ));
        }
        if source_unit == "m/s" && target_unit == "km/h" {
            let scaled = scale_unit_value((**value).clone(), *source_scale)?;
            let converted = multiply_unit_value_by_ratio(scaled, 18, 5)?;
            return Ok(Value::UnitNumber {
                value: Box::new(converted),
                unit: target_unit.clone(),
                scale: 1,
            });
        }
        if source_unit == "km/h" && target_unit == "m/s" {
            let converted = multiply_unit_value_by_ratio((**value).clone(), 5, 18)?;
            return Ok(Value::UnitNumber {
                value: Box::new(converted),
                unit: target_unit.clone(),
                scale: 1,
            });
        }
        let scaled = scale_unit_value((**value).clone(), *source_scale)?;
        let converted = divide_unit_value(scaled, target_scale)?;
        return Ok(Value::UnitNumber {
            value: Box::new(converted),
            unit: target_unit.clone(),
            scale: 1,
        });
    }
    ensure_number(&args[0])?;
    Ok(args[0].clone())
}
