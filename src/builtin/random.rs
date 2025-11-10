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
