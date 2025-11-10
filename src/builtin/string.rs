// 字符串函数模块
use crate::value::Value;

pub fn concat(args: &[Value]) -> Result<Value, String> {
    let mut result = String::new();
    for arg in args {
        result.push_str(&format!("{}", arg));
    }
    Ok(Value::String(result))
}

pub fn length(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("string::length expects 1 argument".to_string());
    }

    match &args[0] {
        Value::String(s) => Ok(Value::Int(s.chars().count() as i64)),
        _ => Err(format!(
            "string::length expects string, got {}",
            args[0].type_name()
        )),
    }
}

pub fn char_at(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("string::char_at expects 2 arguments (string, index)".to_string());
    }

    match (&args[0], &args[1]) {
        (Value::String(s), Value::Int(idx)) => {
            let chars: Vec<char> = s.chars().collect();
            let index = if *idx < 0 {
                (chars.len() as i64 + idx) as usize
            } else {
                *idx as usize
            };

            chars
                .get(index)
                .map(|c| Value::Int(*c as i64))
                .ok_or_else(|| format!("String index out of bounds: {}", idx))
        }
        _ => Err("string::char_at expects (string, int)".to_string()),
    }
}

pub fn at(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("string::at expects 2 arguments (string, index)".to_string());
    }

    match (&args[0], &args[1]) {
        (Value::String(s), Value::Int(idx)) => {
            let chars: Vec<char> = s.chars().collect();
            let index = if *idx < 0 {
                (chars.len() as i64 + idx) as usize
            } else {
                *idx as usize
            };

            chars
                .get(index)
                .map(|c| Value::String(c.to_string()))
                .ok_or_else(|| format!("String index out of bounds: {}", idx))
        }
        _ => Err("string::at expects (string, int)".to_string()),
    }
}

pub fn find(args: &[Value]) -> Result<Value, String> {
    if args.len() != 3 {
        return Err("string::find expects 3 arguments (string, start, substring)".to_string());
    }

    match (&args[0], &args[1], &args[2]) {
        (Value::String(s), Value::Int(start), Value::String(sub)) => {
            let start = *start as usize;
            if start > s.len() {
                return Ok(Value::Int(-1));
            }

            let search_str = &s[start..];
            match search_str.find(sub.as_str()) {
                Some(pos) => Ok(Value::Int((start + pos) as i64)),
                None => Ok(Value::Int(-1)),
            }
        }
        _ => Err("string::find expects (string, int, string)".to_string()),
    }
}

pub fn sub(args: &[Value]) -> Result<Value, String> {
    if args.len() != 3 {
        return Err("string::sub expects 3 arguments (string, start, length)".to_string());
    }

    match (&args[0], &args[1], &args[2]) {
        (Value::String(s), Value::Int(start), Value::Int(len)) => {
            let chars: Vec<char> = s.chars().collect();
            let start = *start as usize;
            let len = *len as usize;

            if start >= chars.len() {
                return Ok(Value::String(String::new()));
            }

            let end = (start + len).min(chars.len());
            let result: String = chars[start..end].iter().collect();
            Ok(Value::String(result))
        }
        _ => Err("string::sub expects (string, int, int)".to_string()),
    }
}

pub fn cat(args: &[Value]) -> Result<Value, String> {
    concat(args)
}
