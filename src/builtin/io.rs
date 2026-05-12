use crate::value::Value;

pub fn format(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("io.format expects 1 argument".to_string());
    }

    Ok(Value::String(args[0].to_string()))
}
