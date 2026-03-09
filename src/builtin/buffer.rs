use crate::value::Value;
use base64::Engine;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

const BUFFER_DATA_KEY: &str = "__bytes";

pub fn create_buffer_module() -> Value {
    let mut ns = HashMap::new();
    ns.insert(
        "alloc".to_string(),
        Value::NativeFunction {
            name: "Buffer::alloc".to_string(),
            func: buffer_alloc,
        },
    );
    ns.insert(
        "from".to_string(),
        Value::NativeFunction {
            name: "Buffer::from".to_string(),
            func: buffer_from,
        },
    );
    ns.insert(
        "concat".to_string(),
        Value::NativeFunction {
            name: "Buffer::concat".to_string(),
            func: buffer_concat,
        },
    );
    Value::Module(Rc::new(RefCell::new(ns)))
}

pub fn new_buffer_from_bytes(bytes: Vec<u8>) -> Value {
    let mut fields = HashMap::new();
    let byte_values = bytes
        .into_iter()
        .map(|b| Value::Int(i64::from(b)))
        .collect::<Vec<_>>();

    fields.insert(
        BUFFER_DATA_KEY.to_string(),
        Value::Array(Rc::new(RefCell::new(byte_values))),
    );

    fields.insert(
        "length".to_string(),
        Value::NativeFunction {
            name: "Buffer::length".to_string(),
            func: buffer_length,
        },
    );
    fields.insert(
        "get".to_string(),
        Value::NativeFunction {
            name: "Buffer::get".to_string(),
            func: buffer_get,
        },
    );
    fields.insert(
        "set".to_string(),
        Value::NativeFunction {
            name: "Buffer::set".to_string(),
            func: buffer_set,
        },
    );
    fields.insert(
        "slice".to_string(),
        Value::NativeFunction {
            name: "Buffer::slice".to_string(),
            func: buffer_slice,
        },
    );
    fields.insert(
        "toText".to_string(),
        Value::NativeFunction {
            name: "Buffer::toText".to_string(),
            func: buffer_to_text,
        },
    );
    fields.insert(
        "toHex".to_string(),
        Value::NativeFunction {
            name: "Buffer::toHex".to_string(),
            func: buffer_to_hex,
        },
    );
    fields.insert(
        "toBase64".to_string(),
        Value::NativeFunction {
            name: "Buffer::toBase64".to_string(),
            func: buffer_to_base64,
        },
    );
    fields.insert(
        "toBase64Url".to_string(),
        Value::NativeFunction {
            name: "Buffer::toBase64Url".to_string(),
            func: buffer_to_base64_url,
        },
    );
    fields.insert(
        "copy".to_string(),
        Value::NativeFunction {
            name: "Buffer::copy".to_string(),
            func: buffer_copy,
        },
    );
    fields.insert(
        "fill".to_string(),
        Value::NativeFunction {
            name: "Buffer::fill".to_string(),
            func: buffer_fill,
        },
    );
    fields.insert(
        "indexOf".to_string(),
        Value::NativeFunction {
            name: "Buffer::indexOf".to_string(),
            func: buffer_index_of,
        },
    );
    fields.insert(
        "includes".to_string(),
        Value::NativeFunction {
            name: "Buffer::includes".to_string(),
            func: buffer_includes,
        },
    );
    fields.insert(
        "equals".to_string(),
        Value::NativeFunction {
            name: "Buffer::equals".to_string(),
            func: buffer_equals,
        },
    );
    fields.insert(
        "compare".to_string(),
        Value::NativeFunction {
            name: "Buffer::compare".to_string(),
            func: buffer_compare,
        },
    );
    fields.insert(
        "subarray".to_string(),
        Value::NativeFunction {
            name: "Buffer::subarray".to_string(),
            func: buffer_subarray,
        },
    );

    Value::Struct(Rc::new(RefCell::new(fields)))
}

pub fn buffer_to_bytes(value: &Value) -> Result<Vec<u8>, String> {
    let bytes_arr = get_buffer_array(value)?;
    let bytes = bytes_arr
        .borrow()
        .iter()
        .map(value_to_u8)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(bytes)
}

fn get_buffer_array(value: &Value) -> Result<Rc<RefCell<Vec<Value>>>, String> {
    let s = match value {
        Value::Struct(s) => s,
        _ => return Err("Expected Buffer object".to_string()),
    };

    let guard = s.borrow();
    let bytes = guard
        .get(BUFFER_DATA_KEY)
        .ok_or_else(|| "Expected Buffer object".to_string())?;

    match bytes {
        Value::Array(arr) => Ok(Rc::clone(arr)),
        _ => Err("Invalid Buffer internal storage".to_string()),
    }
}

fn value_to_u8(v: &Value) -> Result<u8, String> {
    match v {
        Value::Int(n) if (0..=255).contains(n) => Ok(*n as u8),
        Value::Int(n) => Err(format!("Byte value out of range: {}", n)),
        _ => Err("Byte must be int".to_string()),
    }
}

fn get_index(args: &[Value], pos: usize) -> Result<usize, String> {
    let idx = args
        .get(pos)
        .ok_or_else(|| "Missing index argument".to_string())?
        .to_int()?;
    if idx < 0 {
        return Err("Index must be non-negative".to_string());
    }
    Ok(idx as usize)
}

fn buffer_alloc(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("Buffer.alloc expects 1 argument (size)".to_string());
    }

    let size = args[1].to_int()?;
    if size < 0 {
        return Err("Buffer size must be non-negative".to_string());
    }

    Ok(new_buffer_from_bytes(vec![0; size as usize]))
}

fn buffer_from(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 && args.len() != 3 {
        return Err("Buffer.from expects 1 or 2 arguments (data, [encoding])".to_string());
    }

    let encoding = if args.len() == 3 {
        match &args[2] {
            Value::String(s) => s.as_str(),
            _ => return Err("Buffer.from encoding must be string".to_string()),
        }
    } else {
        "utf8"
    };

    match &args[1] {
        Value::String(s) => {
            let bytes = match encoding {
                "utf8" | "utf-8" => s.as_bytes().to_vec(),
                "hex" => decode_hex_nodeish(s),
                "base64" => decode_base64_nodeish(s)?,
                "base64url" => decode_base64_nodeish(s)?,
                _ => return Err(format!("Unsupported encoding: {}", encoding)),
            };
            Ok(new_buffer_from_bytes(bytes))
        }
        Value::Array(arr) => {
            let bytes = arr
                .borrow()
                .iter()
                .map(value_to_u8)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(new_buffer_from_bytes(bytes))
        }
        Value::Struct(_) => {
            let bytes = buffer_to_bytes(&args[1])?;
            Ok(new_buffer_from_bytes(bytes))
        }
        _ => Err("Buffer.from data must be String, List<Int>, or Buffer".to_string()),
    }
}

fn buffer_concat(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("Buffer.concat expects 1 argument (buffers)".to_string());
    }

    let list = match &args[1] {
        Value::Array(arr) => arr,
        _ => return Err("Buffer.concat expects List<Buffer>".to_string()),
    };

    let mut out = Vec::new();
    for item in list.borrow().iter() {
        out.extend_from_slice(&buffer_to_bytes(item)?);
    }
    Ok(new_buffer_from_bytes(out))
}

fn buffer_length(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("Buffer.length expects no arguments".to_string());
    }

    let arr = get_buffer_array(&args[0])?;
    Ok(Value::Int(arr.borrow().len() as i64))
}

fn buffer_get(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("Buffer.get expects 1 argument (index)".to_string());
    }

    let arr = get_buffer_array(&args[0])?;
    let idx = get_index(args, 1)?;
    let guard = arr.borrow();
    guard
        .get(idx)
        .cloned()
        .ok_or_else(|| format!("Buffer index out of bounds: {}", idx))
}

fn buffer_set(args: &[Value]) -> Result<Value, String> {
    if args.len() != 3 {
        return Err("Buffer.set expects 2 arguments (index, value)".to_string());
    }

    let arr = get_buffer_array(&args[0])?;
    let idx = get_index(args, 1)?;
    let value = value_to_u8(&args[2])?;

    let mut guard = arr.borrow_mut();
    if idx >= guard.len() {
        return Err(format!("Buffer index out of bounds: {}", idx));
    }
    guard[idx] = Value::Int(i64::from(value));

    Ok(Value::Null)
}

fn buffer_slice(args: &[Value]) -> Result<Value, String> {
    if args.len() != 3 {
        return Err("Buffer.slice expects 2 arguments (start, end)".to_string());
    }

    let arr = get_buffer_array(&args[0])?;
    let start = get_index(args, 1)?;
    let end = get_index(args, 2)?;

    let guard = arr.borrow();
    if start > end || end > guard.len() {
        return Err(format!(
            "Invalid slice range: start={}, end={}, length={}",
            start,
            end,
            guard.len()
        ));
    }

    let out = guard[start..end]
        .iter()
        .map(value_to_u8)
        .collect::<Result<Vec<_>, _>>()?;
    Ok(new_buffer_from_bytes(out))
}

fn buffer_to_text(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("Buffer.toText expects no arguments".to_string());
    }

    let bytes = buffer_to_bytes(&args[0])?;
    let text = String::from_utf8(bytes).map_err(|e| format!("Buffer is not valid UTF-8: {}", e))?;
    Ok(Value::String(text))
}

fn buffer_to_hex(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("Buffer.toHex expects no arguments".to_string());
    }
    let bytes = buffer_to_bytes(&args[0])?;
    Ok(Value::String(hex::encode(bytes)))
}

fn buffer_to_base64(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("Buffer.toBase64 expects no arguments".to_string());
    }
    let bytes = buffer_to_bytes(&args[0])?;
    Ok(Value::String(
        base64::engine::general_purpose::STANDARD.encode(bytes),
    ))
}

fn buffer_to_base64_url(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("Buffer.toBase64Url expects no arguments".to_string());
    }
    let bytes = buffer_to_bytes(&args[0])?;
    Ok(Value::String(
        base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(bytes),
    ))
}

fn decode_hex_nodeish(s: &str) -> Vec<u8> {
    fn hex_val(c: char) -> Option<u8> {
        c.to_digit(16).map(|v| v as u8)
    }

    let mut out = Vec::new();
    let mut hi: Option<u8> = None;
    for c in s.chars() {
        match hex_val(c) {
            Some(v) => {
                if let Some(h) = hi.take() {
                    out.push((h << 4) | v);
                } else {
                    hi = Some(v);
                }
            }
            None => break,
        }
    }
    out
}

fn decode_base64_nodeish(s: &str) -> Result<Vec<u8>, String> {
    let mut t = s.chars().filter(|c| !c.is_whitespace()).collect::<String>();
    t = t.replace('-', "+").replace('_', "/");

    match t.len() % 4 {
        2 => t.push_str("=="),
        3 => t.push('='),
        1 => return Err("Invalid base64 string length".to_string()),
        _ => {}
    }

    base64::engine::general_purpose::STANDARD
        .decode(t)
        .map_err(|e| format!("Invalid base64 string: {}", e))
}

fn buffer_fill(args: &[Value]) -> Result<Value, String> {
    if args.len() < 2 || args.len() > 4 {
        return Err("Buffer.fill expects 1-3 arguments (value, [start], [end])".to_string());
    }
    let arr = get_buffer_array(&args[0])?;
    let value = value_to_u8(&args[1])?;
    let len = arr.borrow().len();
    let start = if args.len() >= 3 {
        get_index(args, 2)?
    } else {
        0
    };
    let end = if args.len() >= 4 {
        get_index(args, 3)?
    } else {
        len
    };
    if start > end || end > len {
        return Err(format!(
            "Invalid fill range: start={}, end={}, length={}",
            start, end, len
        ));
    }

    let mut guard = arr.borrow_mut();
    for i in start..end {
        guard[i] = Value::Int(i64::from(value));
    }
    Ok(Value::Null)
}

fn search_pattern_from_value(value: &Value, encoding: Option<&str>) -> Result<Vec<u8>, String> {
    match value {
        Value::Int(_) => Ok(vec![value_to_u8(value)?]),
        Value::String(s) => {
            let enc = encoding.unwrap_or("utf8");
            let bytes = match enc {
                "utf8" | "utf-8" => s.as_bytes().to_vec(),
                "hex" => decode_hex_nodeish(s),
                "base64" | "base64url" => decode_base64_nodeish(s)?,
                _ => return Err(format!("Unsupported encoding: {}", enc)),
            };
            Ok(bytes)
        }
        Value::Struct(_) => buffer_to_bytes(value),
        _ => Err("pattern must be Int/String/Buffer".to_string()),
    }
}

fn find_subslice(haystack: &[u8], needle: &[u8], offset: usize) -> Option<usize> {
    if needle.is_empty() {
        return Some(offset.min(haystack.len()));
    }
    if offset >= haystack.len() || needle.len() > haystack.len() {
        return None;
    }
    (offset..=haystack.len() - needle.len()).find(|&i| haystack[i..i + needle.len()] == *needle)
}

fn parse_byte_offset(len: usize, offset_value: Option<&Value>) -> Result<usize, String> {
    match offset_value {
        None => Ok(0),
        Some(v) => {
            let raw = v.to_int()?;
            if raw >= 0 {
                Ok(raw as usize)
            } else {
                let from_end = len.saturating_sub(raw.unsigned_abs() as usize);
                Ok(from_end)
            }
        }
    }
}

fn parse_encoding_arg(value: Option<&Value>, fn_name: &str) -> Result<Option<String>, String> {
    match value {
        None => Ok(None),
        Some(Value::String(s)) => Ok(Some(s.clone())),
        Some(_) => Err(format!("{} encoding must be string", fn_name)),
    }
}

fn buffer_index_of(args: &[Value]) -> Result<Value, String> {
    if args.len() < 2 || args.len() > 4 {
        return Err(
            "Buffer.indexOf expects 1-3 arguments (pattern, [offset], [encoding])".to_string(),
        );
    }
    let bytes = buffer_to_bytes(&args[0])?;
    let offset = parse_byte_offset(bytes.len(), args.get(2))?;
    let enc = parse_encoding_arg(args.get(3), "Buffer.indexOf")?;
    let pat = search_pattern_from_value(&args[1], enc.as_deref())?;

    match find_subslice(&bytes, &pat, offset) {
        Some(i) => Ok(Value::Int(i as i64)),
        None => Ok(Value::Int(-1)),
    }
}

fn buffer_includes(args: &[Value]) -> Result<Value, String> {
    if args.len() < 2 || args.len() > 4 {
        return Err(
            "Buffer.includes expects 1-3 arguments (pattern, [offset], [encoding])".to_string(),
        );
    }
    let idx = buffer_index_of(args)?;
    match idx {
        Value::Int(i) => Ok(Value::Bool(i >= 0)),
        _ => Ok(Value::Bool(false)),
    }
}

fn buffer_copy(args: &[Value]) -> Result<Value, String> {
    if args.len() < 2 || args.len() > 5 {
        return Err(
            "Buffer.copy expects 1-4 arguments (target, [targetStart], [sourceStart], [sourceEnd])"
                .to_string(),
        );
    }

    let source_arr = get_buffer_array(&args[0])?;
    let target_arr = get_buffer_array(&args[1])?;
    let target_start = if args.len() >= 3 {
        get_index(args, 2)?
    } else {
        0
    };
    let source_start = if args.len() >= 4 {
        get_index(args, 3)?
    } else {
        0
    };

    let source_len = source_arr.borrow().len();
    let source_end = if args.len() >= 5 {
        get_index(args, 4)?
    } else {
        source_len
    };

    if source_start > source_end || source_end > source_len {
        return Err(format!(
            "Invalid source range: start={}, end={}, length={}",
            source_start, source_end, source_len
        ));
    }

    let mut target = target_arr.borrow_mut();
    if target_start > target.len() {
        return Err(format!(
            "Target start out of bounds: start={}, length={}",
            target_start,
            target.len()
        ));
    }

    let source = source_arr.borrow();
    let mut copied = 0usize;
    for (i, src_idx) in (source_start..source_end).enumerate() {
        let dst_idx = target_start + i;
        if dst_idx >= target.len() {
            break;
        }
        target[dst_idx] = source[src_idx].clone();
        copied += 1;
    }

    Ok(Value::Int(copied as i64))
}

fn buffer_equals(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("Buffer.equals expects 1 argument (other)".to_string());
    }
    let a = buffer_to_bytes(&args[0])?;
    let b = buffer_to_bytes(&args[1])?;
    Ok(Value::Bool(a == b))
}

fn buffer_compare(args: &[Value]) -> Result<Value, String> {
    if args.len() < 2 || args.len() > 6 {
        return Err(
            "Buffer.compare expects 1-5 arguments (other, [targetStart], [targetEnd], [sourceStart], [sourceEnd])"
                .to_string(),
        );
    }
    let a = buffer_to_bytes(&args[0])?;
    let b = buffer_to_bytes(&args[1])?;

    let target_start = if args.len() >= 3 {
        get_index(args, 2)?
    } else {
        0
    };
    let target_end = if args.len() >= 4 {
        get_index(args, 3)?
    } else {
        b.len()
    };
    let source_start = if args.len() >= 5 {
        get_index(args, 4)?
    } else {
        0
    };
    let source_end = if args.len() >= 6 {
        get_index(args, 5)?
    } else {
        a.len()
    };

    if target_start > target_end || target_end > b.len() {
        return Err(format!(
            "Invalid target compare range: start={}, end={}, length={}",
            target_start,
            target_end,
            b.len()
        ));
    }
    if source_start > source_end || source_end > a.len() {
        return Err(format!(
            "Invalid source compare range: start={}, end={}, length={}",
            source_start,
            source_end,
            a.len()
        ));
    }

    let a = &a[source_start..source_end];
    let b = &b[target_start..target_end];
    let out = match a.cmp(b) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    };
    Ok(Value::Int(out))
}

fn normalize_range_index(len: usize, idx: i64) -> usize {
    if idx >= 0 {
        (idx as usize).min(len)
    } else {
        len.saturating_sub(idx.unsigned_abs() as usize)
    }
}

fn buffer_subarray(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 && args.len() != 3 {
        return Err("Buffer.subarray expects 1 or 2 arguments (start, [end])".to_string());
    }
    let bytes = buffer_to_bytes(&args[0])?;
    let len = bytes.len();

    let start = normalize_range_index(len, args[1].to_int()?);
    let end = if args.len() == 3 {
        normalize_range_index(len, args[2].to_int()?)
    } else {
        len
    };

    if end < start {
        return Ok(new_buffer_from_bytes(Vec::new()));
    }
    Ok(new_buffer_from_bytes(bytes[start..end].to_vec()))
}
