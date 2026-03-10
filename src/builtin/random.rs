// 随机函数模块
use crate::value::Value;

pub fn rand(_args: &[Value]) -> Result<Value, String> {
    use rand::Rng;
    let mut rng = rand::rng();
    Ok(Value::Float(rng.random::<f64>()))
}

pub fn randint(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("random::randint expects 2 arguments (start, end)".to_string());
    }

    use rand::Rng;
    let start = args[0].to_int()?;
    let end = args[1].to_int()?;
    let mut rng = rand::rng();
    Ok(Value::Int(rng.random_range(start..=end)))
}

pub fn random(_args: &[Value]) -> Result<Value, String> {
    use rand::Rng;
    let mut rng = rand::rng();
    Ok(Value::Float(rng.random::<f64>()))
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
