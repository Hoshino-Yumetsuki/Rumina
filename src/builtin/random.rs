// 随机函数模块
use std::cell::RefCell;

use crate::value::Value;

thread_local! {
    static RNG_STATE: RefCell<u64> = const { RefCell::new(0x853c_49e6_748f_ea9b) };
}

fn next_u64() -> u64 {
    RNG_STATE.with(|state| {
        let mut state = state.borrow_mut();
        *state = state.wrapping_mul(6364136223846793005).wrapping_add(1);
        *state
    })
}

fn next_f64() -> f64 {
    const SCALE: f64 = (1u64 << 53) as f64;
    ((next_u64() >> 11) as f64) / SCALE
}

pub fn seed(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("random.seed expects 1 argument".to_string());
    }

    let seed = args[0].to_int()? as u64;
    RNG_STATE.with(|state| *state.borrow_mut() = seed);
    Ok(Value::Bool(true))
}

pub fn rand(_args: &[Value]) -> Result<Value, String> {
    Ok(Value::Float(next_f64()))
}

pub fn randint(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("random::randint expects 2 arguments (start, end)".to_string());
    }

    let start = args[0].to_int()?;
    let end = args[1].to_int()?;
    if start > end {
        return Err("random::randint expects start <= end".to_string());
    }

    let span = (end - start + 1) as u64;
    Ok(Value::Int(start + (next_u64() % span) as i64))
}

pub fn random(_args: &[Value]) -> Result<Value, String> {
    rand(&[])
}

pub fn normal(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("random.normal expects 2 arguments".to_string());
    }

    let mu = args[0].to_float()?;
    let sigma = args[1].to_float()?;
    if sigma < 0.0 {
        return Err("random.normal expects non-negative sigma".to_string());
    }

    let u1 = next_f64().max(f64::MIN_POSITIVE);
    let u2 = next_f64();
    let z0 = (-2.0 * u1.ln()).sqrt() * (std::f64::consts::TAU * u2).cos();
    Ok(Value::Float(mu + sigma * z0))
}

pub fn choice(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("random.choice expects 1 argument".to_string());
    }

    let Value::Array(values) = &args[0] else {
        return Err(format!(
            "random.choice expects array, got {}",
            args[0].type_name()
        ));
    };

    let values = values.borrow();
    if values.is_empty() {
        return Err("random.choice expects non-empty array".to_string());
    }

    let index = (next_u64() % values.len() as u64) as usize;
    Ok(values[index].clone())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rand() {
        let result = rand(&[]);
        assert!(result.is_ok());
        if let Ok(Value::Float(f)) = result {
            assert!((0.0..1.0).contains(&f));
        }
    }

    #[test]
    fn test_randint() {
        let result = randint(&[Value::Int(1), Value::Int(10)]);
        assert!(result.is_ok());
        if let Ok(Value::Int(i)) = result {
            assert!((1..=10).contains(&i));
        }
    }

    #[test]
    fn test_randint_wrong_args() {
        assert!(randint(&[Value::Int(1)]).is_err());
    }

    #[test]
    fn test_random() {
        let result = random(&[]);
        assert!(result.is_ok());
    }
}
