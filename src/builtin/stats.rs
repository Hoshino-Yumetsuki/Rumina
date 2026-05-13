use crate::value::Value;

fn numbers(value: &Value) -> Result<Vec<f64>, String> {
    let values = match value {
        Value::Vector(values) | Value::Array(values) => values,
        _ => return Err(format!("stats expects vector, got {}", value.type_name())),
    };

    let values = values.borrow();
    if values.is_empty() {
        return Err("EmptyInput: stats expects non-empty vector".to_string());
    }

    values.iter().map(Value::to_float).collect()
}

fn mean_values(values: &[f64]) -> f64 {
    values.iter().sum::<f64>() / values.len() as f64
}

pub fn mean(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("stats.mean expects 1 argument".to_string());
    }

    Ok(Value::Float(mean_values(&numbers(&args[0])?)))
}

pub fn median(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("stats.median expects 1 argument".to_string());
    }

    let mut values = numbers(&args[0])?;
    values.sort_by(f64::total_cmp);
    let mid = values.len() / 2;
    if values.len() % 2 == 0 {
        Ok(Value::Float((values[mid - 1] + values[mid]) / 2.0))
    } else {
        Ok(Value::Float(values[mid]))
    }
}

pub fn var(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("stats.var expects 1 argument".to_string());
    }

    let values = numbers(&args[0])?;
    let mean = mean_values(&values);
    Ok(Value::Float(
        values
            .iter()
            .map(|value| (value - mean).powi(2))
            .sum::<f64>()
            / values.len() as f64,
    ))
}

pub fn std(args: &[Value]) -> Result<Value, String> {
    let Value::Float(variance) = var(args)? else {
        unreachable!("variance returns float")
    };
    Ok(Value::Float(variance.sqrt()))
}

pub fn quantile(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("stats.quantile expects 2 arguments".to_string());
    }

    let mut values = numbers(&args[0])?;
    let q = args[1].to_float()?;
    if !(0.0..=1.0).contains(&q) {
        return Err("stats.quantile expects q in [0, 1]".to_string());
    }

    values.sort_by(f64::total_cmp);
    let position = q * (values.len() - 1) as f64;
    let lower = position.floor() as usize;
    let upper = position.ceil() as usize;
    let weight = position - lower as f64;
    Ok(Value::Float(
        values[lower] * (1.0 - weight) + values[upper] * weight,
    ))
}

pub fn cov(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("stats.cov expects 2 arguments".to_string());
    }

    let xs = numbers(&args[0])?;
    let ys = numbers(&args[1])?;
    if xs.len() != ys.len() {
        return Err("stats.cov expects vectors with same length".to_string());
    }

    let mean_x = mean_values(&xs);
    let mean_y = mean_values(&ys);
    Ok(Value::Float(
        xs.iter()
            .zip(ys.iter())
            .map(|(x, y)| (x - mean_x) * (y - mean_y))
            .sum::<f64>()
            / xs.len() as f64,
    ))
}

pub fn corr(args: &[Value]) -> Result<Value, String> {
    let Value::Float(covariance) = cov(args)? else {
        unreachable!("covariance returns float")
    };
    let Value::Float(std_x) = std(std::slice::from_ref(&args[0]))? else {
        unreachable!("std returns float")
    };
    let Value::Float(std_y) = std(std::slice::from_ref(&args[1]))? else {
        unreachable!("std returns float")
    };

    if std_x == 0.0 || std_y == 0.0 {
        return Err("stats.corr undefined for zero variance".to_string());
    }

    Ok(Value::Float(covariance / (std_x * std_y)))
}
