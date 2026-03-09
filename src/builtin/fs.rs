use crate::builtin::buffer::{buffer_to_bytes, new_buffer_from_bytes};
use crate::value::Value;
use base64::Engine;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::{self, OpenOptions};
use std::path::Path;
use std::rc::Rc;
use std::time::UNIX_EPOCH;

#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;

pub fn create_fs_module() -> Value {
    let mut ns = HashMap::new();

    insert_fn(&mut ns, "readText", fs_read_text);
    insert_fn(&mut ns, "readBytes", fs_read_bytes);
    insert_fn(&mut ns, "writeText", fs_write_text);
    insert_fn(&mut ns, "writeBytes", fs_write_bytes);
    insert_fn(&mut ns, "append", fs_append);

    insert_fn(&mut ns, "exists", fs_exists);
    insert_fn(&mut ns, "isFile", fs_is_file);
    insert_fn(&mut ns, "isDir", fs_is_dir);
    insert_fn(&mut ns, "stat", fs_stat);

    insert_fn(&mut ns, "makeDir", fs_make_dir);
    insert_fn(&mut ns, "makeDirAll", fs_make_dir_all);
    insert_fn(&mut ns, "readDir", fs_read_dir);
    insert_fn(&mut ns, "remove", fs_remove);
    insert_fn(&mut ns, "removeAll", fs_remove_all);
    insert_fn(&mut ns, "rename", fs_rename);
    insert_fn(&mut ns, "copy", fs_copy);
    insert_fn(&mut ns, "realpath", fs_realpath);
    insert_fn(&mut ns, "readLink", fs_read_link);
    insert_fn(&mut ns, "link", fs_link);
    insert_fn(&mut ns, "symlink", fs_symlink);
    insert_fn(&mut ns, "chmod", fs_chmod);

    Value::Module(Rc::new(RefCell::new(ns)))
}

fn insert_fn(
    ns: &mut HashMap<String, Value>,
    name: &str,
    func: fn(&[Value]) -> Result<Value, String>,
) {
    ns.insert(
        name.to_string(),
        Value::NativeFunction {
            name: format!("fs::{}", name),
            func,
        },
    );
}

fn as_path(arg: &Value, fn_name: &str) -> Result<String, String> {
    match arg {
        Value::String(s) => Ok(s.clone()),
        _ => Err(format!("{} expects path as string", fn_name)),
    }
}

fn expect_arity(args: &[Value], expected: usize, sig: &str) -> Result<(), String> {
    if args.len() != expected {
        return Err(format!("{} expects {} arguments", sig, expected - 1));
    }
    Ok(())
}

fn fs_read_text(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 && args.len() != 3 {
        return Err("fs.readText(path, [options]) expects 1 or 2 arguments".to_string());
    }
    let path = as_path(&args[1], "fs.readText")?;
    let bytes = fs::read(&path).map_err(|e| format!("fs.readText failed for '{}': {}", path, e))?;
    let encoding = if args.len() == 3 {
        parse_encoding_opt(&args[2], "fs.readText", "utf8")?
    } else {
        "utf8".to_string()
    };
    let content = decode_text_with_encoding(&bytes, &encoding)?;
    Ok(Value::String(content))
}

fn fs_read_bytes(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 && args.len() != 3 {
        return Err("fs.readBytes(path, [options]) expects 1 or 2 arguments".to_string());
    }
    let path = as_path(&args[1], "fs.readBytes")?;
    if args.len() == 3 {
        let _ = parse_flag_opt(&args[2], "fs.readBytes", "r")?;
    }
    let bytes =
        fs::read(&path).map_err(|e| format!("fs.readBytes failed for '{}': {}", path, e))?;
    Ok(new_buffer_from_bytes(bytes))
}

fn fs_write_text(args: &[Value]) -> Result<Value, String> {
    if args.len() != 3 && args.len() != 4 {
        return Err("fs.writeText(path, text, [options]) expects 2 or 3 arguments".to_string());
    }
    let path = as_path(&args[1], "fs.writeText")?;
    let text = match &args[2] {
        Value::String(s) => s,
        _ => return Err("fs.writeText expects text as string".to_string()),
    };

    if args.len() == 4 {
        let flag = parse_flag_opt(&args[3], "fs.writeText", "w")?;
        write_with_flag(&path, text.as_bytes(), &flag)
            .map_err(|e| format!("fs.writeText failed for '{}': {}", path, e))?;
    } else {
        fs::write(&path, text).map_err(|e| format!("fs.writeText failed for '{}': {}", path, e))?;
    }
    Ok(Value::Null)
}

fn fs_write_bytes(args: &[Value]) -> Result<Value, String> {
    if args.len() != 3 && args.len() != 4 {
        return Err("fs.writeBytes(path, data, [options]) expects 2 or 3 arguments".to_string());
    }
    let path = as_path(&args[1], "fs.writeBytes")?;
    let bytes = match &args[2] {
        Value::Struct(_) => buffer_to_bytes(&args[2])?,
        _ => return Err("fs.writeBytes expects Buffer as data".to_string()),
    };

    if args.len() == 4 {
        let flag = parse_flag_opt(&args[3], "fs.writeBytes", "w")?;
        write_with_flag(&path, &bytes, &flag)
            .map_err(|e| format!("fs.writeBytes failed for '{}': {}", path, e))?;
    } else {
        fs::write(&path, bytes)
            .map_err(|e| format!("fs.writeBytes failed for '{}': {}", path, e))?;
    }
    Ok(Value::Null)
}

fn parse_flag_opt(value: &Value, fn_name: &str, default_flag: &str) -> Result<String, String> {
    match value {
        Value::String(_) => Ok(default_flag.to_string()),
        Value::Struct(s) => {
            let obj = s.borrow();
            match obj.get("flag") {
                Some(Value::String(flag)) => Ok(flag.clone()),
                Some(_) => Err(format!("{} options.flag must be string", fn_name)),
                None => Ok(default_flag.to_string()),
            }
        }
        _ => Err(format!(
            "{} options must be string or object struct",
            fn_name
        )),
    }
}

fn parse_encoding_opt(
    value: &Value,
    fn_name: &str,
    default_encoding: &str,
) -> Result<String, String> {
    match value {
        Value::String(enc) => Ok(enc.clone()),
        Value::Struct(s) => {
            let obj = s.borrow();
            match obj.get("encoding") {
                Some(Value::String(enc)) => Ok(enc.clone()),
                Some(_) => Err(format!("{} options.encoding must be string", fn_name)),
                None => Ok(default_encoding.to_string()),
            }
        }
        _ => Err(format!(
            "{} options must be string or object struct",
            fn_name
        )),
    }
}

fn decode_text_with_encoding(bytes: &[u8], encoding: &str) -> Result<String, String> {
    match encoding {
        "utf8" | "utf-8" => {
            String::from_utf8(bytes.to_vec()).map_err(|e| format!("Invalid UTF-8 text: {}", e))
        }
        "hex" => Ok(hex::encode(bytes)),
        "base64" => Ok(base64::engine::general_purpose::STANDARD.encode(bytes)),
        "base64url" => Ok(base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(bytes)),
        _ => Err(format!("Unsupported encoding: {}", encoding)),
    }
}

fn write_with_flag(path: &str, bytes: &[u8], flag: &str) -> std::io::Result<()> {
    let mut opts = OpenOptions::new();
    opts.create(true).write(true);
    match flag {
        "w" => {
            opts.truncate(true);
        }
        "a" => {
            opts.append(true);
        }
        _ => {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                format!("unsupported flag '{}' (supported: w, a)", flag),
            ));
        }
    }
    let mut f = opts.open(path)?;
    std::io::Write::write_all(&mut f, bytes)
}

fn fs_append(args: &[Value]) -> Result<Value, String> {
    expect_arity(args, 3, "fs.append(path, data)")?;
    let path = as_path(&args[1], "fs.append")?;
    let mut current = if Path::new(&path).exists() {
        fs::read(&path).map_err(|e| format!("fs.append read failed for '{}': {}", path, e))?
    } else {
        Vec::new()
    };

    match &args[2] {
        Value::String(s) => current.extend_from_slice(s.as_bytes()),
        _ => current.extend_from_slice(&buffer_to_bytes(&args[2])?),
    }

    fs::write(&path, current)
        .map_err(|e| format!("fs.append write failed for '{}': {}", path, e))?;
    Ok(Value::Null)
}

fn fs_exists(args: &[Value]) -> Result<Value, String> {
    expect_arity(args, 2, "fs.exists(path)")?;
    let path = as_path(&args[1], "fs.exists")?;
    Ok(Value::Bool(Path::new(&path).exists()))
}

fn fs_is_file(args: &[Value]) -> Result<Value, String> {
    expect_arity(args, 2, "fs.isFile(path)")?;
    let path = as_path(&args[1], "fs.isFile")?;
    Ok(Value::Bool(Path::new(&path).is_file()))
}

fn fs_is_dir(args: &[Value]) -> Result<Value, String> {
    expect_arity(args, 2, "fs.isDir(path)")?;
    let path = as_path(&args[1], "fs.isDir")?;
    Ok(Value::Bool(Path::new(&path).is_dir()))
}

fn fs_stat(args: &[Value]) -> Result<Value, String> {
    expect_arity(args, 2, "fs.stat(path)")?;
    let path = as_path(&args[1], "fs.stat")?;
    let meta = fs::metadata(&path).map_err(|e| format!("fs.stat failed for '{}': {}", path, e))?;

    let modified_secs = match meta.modified() {
        Ok(m) => m
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs() as i64)
            .unwrap_or(0),
        Err(_) => 0,
    };

    let mut map = HashMap::new();
    map.insert("size".to_string(), Value::Int(meta.len() as i64));
    map.insert("isFile".to_string(), Value::Bool(meta.is_file()));
    map.insert("isDir".to_string(), Value::Bool(meta.is_dir()));
    map.insert("modifiedTime".to_string(), Value::Int(modified_secs));
    Ok(Value::Struct(Rc::new(RefCell::new(map))))
}

fn fs_make_dir(args: &[Value]) -> Result<Value, String> {
    expect_arity(args, 2, "fs.makeDir(path)")?;
    let path = as_path(&args[1], "fs.makeDir")?;
    fs::create_dir(&path).map_err(|e| format!("fs.makeDir failed for '{}': {}", path, e))?;
    Ok(Value::Null)
}

fn fs_make_dir_all(args: &[Value]) -> Result<Value, String> {
    expect_arity(args, 2, "fs.makeDirAll(path)")?;
    let path = as_path(&args[1], "fs.makeDirAll")?;
    fs::create_dir_all(&path).map_err(|e| format!("fs.makeDirAll failed for '{}': {}", path, e))?;
    Ok(Value::Null)
}

fn fs_read_dir(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 && args.len() != 3 {
        return Err("fs.readDir(path, [withTypes]) expects 1 or 2 arguments".to_string());
    }
    let path = as_path(&args[1], "fs.readDir")?;
    let with_types = if args.len() == 3 {
        match &args[2] {
            Value::Bool(b) => *b,
            _ => return Err("fs.readDir withTypes must be bool".to_string()),
        }
    } else {
        false
    };

    let entries =
        fs::read_dir(&path).map_err(|e| format!("fs.readDir failed for '{}': {}", path, e))?;
    let mut list = Vec::new();
    for entry in entries {
        let entry = entry.map_err(|e| format!("fs.readDir entry error: {}", e))?;
        let name = entry.file_name().to_string_lossy().to_string();
        if with_types {
            let ty = entry
                .file_type()
                .map_err(|e| format!("fs.readDir file_type error: {}", e))?;
            let mut item = HashMap::new();
            item.insert("name".to_string(), Value::String(name));
            item.insert("isFile".to_string(), Value::Bool(ty.is_file()));
            item.insert("isDir".to_string(), Value::Bool(ty.is_dir()));
            item.insert("isSymlink".to_string(), Value::Bool(ty.is_symlink()));
            list.push(Value::Struct(Rc::new(RefCell::new(item))));
        } else {
            list.push(Value::String(name));
        }
    }

    Ok(Value::Array(Rc::new(RefCell::new(list))))
}

fn fs_remove(args: &[Value]) -> Result<Value, String> {
    expect_arity(args, 2, "fs.remove(path)")?;
    let path = as_path(&args[1], "fs.remove")?;
    let p = Path::new(&path);

    if p.is_file() {
        fs::remove_file(&path)
            .map_err(|e| format!("fs.remove file failed for '{}': {}", path, e))?;
    } else if p.is_dir() {
        fs::remove_dir(&path).map_err(|e| format!("fs.remove dir failed for '{}': {}", path, e))?;
    } else {
        return Err(format!("fs.remove: path does not exist '{}'", path));
    }

    Ok(Value::Null)
}

fn fs_remove_all(args: &[Value]) -> Result<Value, String> {
    expect_arity(args, 2, "fs.removeAll(path)")?;
    let path = as_path(&args[1], "fs.removeAll")?;
    let p = Path::new(&path);

    if p.is_file() {
        fs::remove_file(&path)
            .map_err(|e| format!("fs.removeAll file failed for '{}': {}", path, e))?;
    } else if p.is_dir() {
        fs::remove_dir_all(&path)
            .map_err(|e| format!("fs.removeAll dir failed for '{}': {}", path, e))?;
    } else {
        return Err(format!("fs.removeAll: path does not exist '{}'", path));
    }

    Ok(Value::Null)
}

fn fs_rename(args: &[Value]) -> Result<Value, String> {
    expect_arity(args, 3, "fs.rename(oldPath, newPath)")?;
    let old_path = as_path(&args[1], "fs.rename")?;
    let new_path = as_path(&args[2], "fs.rename")?;
    fs::rename(&old_path, &new_path)
        .map_err(|e| format!("fs.rename failed '{}'=> '{}': {}", old_path, new_path, e))?;
    Ok(Value::Null)
}

fn fs_copy(args: &[Value]) -> Result<Value, String> {
    expect_arity(args, 3, "fs.copy(srcPath, destPath)")?;
    let src = as_path(&args[1], "fs.copy")?;
    let dst = as_path(&args[2], "fs.copy")?;
    fs::copy(&src, &dst).map_err(|e| format!("fs.copy failed '{}'=> '{}': {}", src, dst, e))?;
    Ok(Value::Null)
}

fn fs_realpath(args: &[Value]) -> Result<Value, String> {
    expect_arity(args, 2, "fs.realpath(path)")?;
    let path = as_path(&args[1], "fs.realpath")?;
    let p =
        fs::canonicalize(&path).map_err(|e| format!("fs.realpath failed for '{}': {}", path, e))?;
    Ok(Value::String(p.to_string_lossy().to_string()))
}

fn fs_read_link(args: &[Value]) -> Result<Value, String> {
    expect_arity(args, 2, "fs.readLink(path)")?;
    let path = as_path(&args[1], "fs.readLink")?;
    let p =
        fs::read_link(&path).map_err(|e| format!("fs.readLink failed for '{}': {}", path, e))?;
    Ok(Value::String(p.to_string_lossy().to_string()))
}

fn fs_link(args: &[Value]) -> Result<Value, String> {
    expect_arity(args, 3, "fs.link(existingPath, newPath)")?;
    let src = as_path(&args[1], "fs.link")?;
    let dst = as_path(&args[2], "fs.link")?;
    fs::hard_link(&src, &dst)
        .map_err(|e| format!("fs.link failed '{}'=> '{}': {}", src, dst, e))?;
    Ok(Value::Null)
}

fn fs_symlink(args: &[Value]) -> Result<Value, String> {
    expect_arity(args, 3, "fs.symlink(target, path)")?;
    let _target = as_path(&args[1], "fs.symlink")?;
    let _path = as_path(&args[2], "fs.symlink")?;

    #[cfg(unix)]
    {
        std::os::unix::fs::symlink(&_target, &_path)
            .map_err(|e| format!("fs.symlink failed '{}'=> '{}': {}", _target, _path, e))?;
        return Ok(Value::Null);
    }

    #[cfg(windows)]
    {
        let target_path = Path::new(&_target);
        let result = if target_path.is_dir() {
            std::os::windows::fs::symlink_dir(&_target, &_path)
        } else {
            std::os::windows::fs::symlink_file(&_target, &_path)
        };
        result.map_err(|e| format!("fs.symlink failed '{}'=> '{}': {}", _target, _path, e))?;
        return Ok(Value::Null);
    }

    #[allow(unreachable_code)]
    Err("fs.symlink is not supported on this platform".to_string())
}

fn fs_chmod(args: &[Value]) -> Result<Value, String> {
    expect_arity(args, 3, "fs.chmod(path, mode)")?;
    let path = as_path(&args[1], "fs.chmod")?;
    let mode = args[2].to_int()?;
    if mode < 0 {
        return Err("fs.chmod mode must be non-negative".to_string());
    }

    #[cfg(unix)]
    {
        let perms = fs::Permissions::from_mode(mode as u32);
        fs::set_permissions(&path, perms)
            .map_err(|e| format!("fs.chmod failed for '{}': {}", path, e))?;
        Ok(Value::Null)
    }

    #[cfg(not(unix))]
    {
        let _ = (path, mode);
        Err("fs.chmod is not supported on this platform".to_string())
    }
}
