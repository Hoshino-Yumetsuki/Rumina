/// Bytecode serialization and deserialization for .rmc files
///
/// This module provides functionality to save and load compiled bytecode
/// to/from plain text format for the .rmc file format.

use crate::error::RuminaError;
use crate::value::Value;
use crate::vm::{ByteCode, OpCode};
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::Path;

/// Serialize bytecode to a .rmc file (plain text format)
pub fn save_bytecode(bytecode: &ByteCode, path: &Path) -> Result<(), RuminaError> {
    let file = File::create(path).map_err(|e| {
        RuminaError::runtime(format!("Failed to create file '{}': {}", path.display(), e))
    })?;
    let mut writer = BufWriter::new(file);

    // Write header
    writeln!(writer, "# Rumina Bytecode v1.0")
        .map_err(|e| RuminaError::runtime(format!("Write error: {}", e)))?;
    writeln!(writer)
        .map_err(|e| RuminaError::runtime(format!("Write error: {}", e)))?;

    // Write constants section
    writeln!(writer, "[CONSTANTS]")
        .map_err(|e| RuminaError::runtime(format!("Write error: {}", e)))?;
    writeln!(writer, "{}", bytecode.constants.len())
        .map_err(|e| RuminaError::runtime(format!("Write error: {}", e)))?;
    
    for constant in &bytecode.constants {
        let serialized = serialize_value(constant)?;
        writeln!(writer, "{}", serialized)
            .map_err(|e| RuminaError::runtime(format!("Write error: {}", e)))?;
    }
    writeln!(writer)
        .map_err(|e| RuminaError::runtime(format!("Write error: {}", e)))?;

    // Write instructions section
    writeln!(writer, "[INSTRUCTIONS]")
        .map_err(|e| RuminaError::runtime(format!("Write error: {}", e)))?;
    writeln!(writer, "{}", bytecode.instructions.len())
        .map_err(|e| RuminaError::runtime(format!("Write error: {}", e)))?;
    
    for (i, instruction) in bytecode.instructions.iter().enumerate() {
        let line_num = bytecode.line_numbers.get(i).and_then(|&n| n);
        let serialized = serialize_opcode(instruction)?;
        if let Some(line) = line_num {
            writeln!(writer, "{}:{}", line, serialized)
                .map_err(|e| RuminaError::runtime(format!("Write error: {}", e)))?;
        } else {
            writeln!(writer, ":{}", serialized)
                .map_err(|e| RuminaError::runtime(format!("Write error: {}", e)))?;
        }
    }

    writer.flush()
        .map_err(|e| RuminaError::runtime(format!("Flush error: {}", e)))?;

    Ok(())
}

/// Deserialize bytecode from a .rmc file (plain text format)
pub fn load_bytecode(path: &Path) -> Result<ByteCode, RuminaError> {
    let file = File::open(path).map_err(|e| {
        RuminaError::runtime(format!("Failed to open file '{}': {}", path.display(), e))
    })?;
    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    // Skip header
    while let Some(Ok(line)) = lines.next() {
        if line.trim().is_empty() || line.starts_with('#') {
            continue;
        }
        if line.trim() == "[CONSTANTS]" {
            break;
        }
    }

    // Read constants count
    let constants_count: usize = lines
        .next()
        .ok_or_else(|| RuminaError::runtime("Unexpected EOF: missing constants count".to_string()))?
        .map_err(|e| RuminaError::runtime(format!("Read error: {}", e)))?
        .trim()
        .parse()
        .map_err(|e| RuminaError::runtime(format!("Invalid constants count: {}", e)))?;

    // Read constants
    let mut constants = Vec::new();
    for _ in 0..constants_count {
        let line = lines
            .next()
            .ok_or_else(|| RuminaError::runtime("Unexpected EOF: missing constant".to_string()))?
            .map_err(|e| RuminaError::runtime(format!("Read error: {}", e)))?;
        let value = deserialize_value(&line)?;
        constants.push(value);
    }

    // Skip to instructions section
    while let Some(Ok(line)) = lines.next() {
        if line.trim() == "[INSTRUCTIONS]" {
            break;
        }
    }

    // Read instructions count
    let instructions_count: usize = lines
        .next()
        .ok_or_else(|| RuminaError::runtime("Unexpected EOF: missing instructions count".to_string()))?
        .map_err(|e| RuminaError::runtime(format!("Read error: {}", e)))?
        .trim()
        .parse()
        .map_err(|e| RuminaError::runtime(format!("Invalid instructions count: {}", e)))?;

    // Read instructions
    let mut instructions = Vec::new();
    let mut line_numbers = Vec::new();
    
    for _ in 0..instructions_count {
        let line = lines
            .next()
            .ok_or_else(|| RuminaError::runtime("Unexpected EOF: missing instruction".to_string()))?
            .map_err(|e| RuminaError::runtime(format!("Read error: {}", e)))?;
        
        let (line_num, opcode_str) = if let Some(colon_pos) = line.find(':') {
            let line_num_str = &line[..colon_pos];
            let opcode_str = &line[colon_pos + 1..];
            let line_num = if line_num_str.is_empty() {
                None
            } else {
                Some(line_num_str.parse().map_err(|e| {
                    RuminaError::runtime(format!("Invalid line number: {}", e))
                })?)
            };
            (line_num, opcode_str)
        } else {
            return Err(RuminaError::runtime(format!(
                "Invalid instruction format: {}",
                line
            )));
        };

        let opcode = deserialize_opcode(opcode_str)?;
        instructions.push(opcode);
        line_numbers.push(line_num);
    }

    Ok(ByteCode {
        instructions,
        line_numbers,
        constants,
    })
}

/// Serialize a Value to string
fn serialize_value(value: &Value) -> Result<String, RuminaError> {
    match value {
        Value::Int(n) => Ok(format!("INT:{}", n)),
        Value::Float(f) => Ok(format!("FLOAT:{}", f)),
        Value::Bool(b) => Ok(format!("BOOL:{}", b)),
        Value::String(s) => {
            // Escape special characters
            let escaped = s
                .replace('\\', "\\\\")
                .replace('\n', "\\n")
                .replace('\r', "\\r")
                .replace('\t', "\\t");
            Ok(format!("STRING:{}", escaped))
        }
        Value::Null => Ok("NULL".to_string()),
        _ => Err(RuminaError::runtime(format!(
            "Cannot serialize value type: {}",
            value.type_name()
        ))),
    }
}

/// Deserialize a Value from string
fn deserialize_value(s: &str) -> Result<Value, RuminaError> {
    let s = s.trim();
    
    if s == "NULL" {
        return Ok(Value::Null);
    }

    if let Some(rest) = s.strip_prefix("INT:") {
        let n = rest.parse().map_err(|e| {
            RuminaError::runtime(format!("Invalid int value: {}", e))
        })?;
        return Ok(Value::Int(n));
    }

    if let Some(rest) = s.strip_prefix("FLOAT:") {
        let f = rest.parse().map_err(|e| {
            RuminaError::runtime(format!("Invalid float value: {}", e))
        })?;
        return Ok(Value::Float(f));
    }

    if let Some(rest) = s.strip_prefix("BOOL:") {
        let b = rest.parse().map_err(|e| {
            RuminaError::runtime(format!("Invalid bool value: {}", e))
        })?;
        return Ok(Value::Bool(b));
    }

    if let Some(rest) = s.strip_prefix("STRING:") {
        // Unescape special characters
        let unescaped = rest
            .replace("\\n", "\n")
            .replace("\\r", "\r")
            .replace("\\t", "\t")
            .replace("\\\\", "\\");
        return Ok(Value::String(unescaped));
    }

    Err(RuminaError::runtime(format!("Unknown value format: {}", s)))
}

/// Serialize an OpCode to string
fn serialize_opcode(opcode: &OpCode) -> Result<String, RuminaError> {
    match opcode {
        OpCode::PushConst(value) => {
            let val_str = serialize_value(value)?;
            Ok(format!("PushConst {}", val_str))
        }
        OpCode::PushConstPooled(idx) => Ok(format!("PushConstPooled {}", idx)),
        OpCode::PushVar(name) => Ok(format!("PushVar {}", name)),
        OpCode::PopVar(name) => Ok(format!("PopVar {}", name)),
        OpCode::Dup => Ok("Dup".to_string()),
        OpCode::Pop => Ok("Pop".to_string()),
        OpCode::Add => Ok("Add".to_string()),
        OpCode::AddInt => Ok("AddInt".to_string()),
        OpCode::Sub => Ok("Sub".to_string()),
        OpCode::SubInt => Ok("SubInt".to_string()),
        OpCode::Mul => Ok("Mul".to_string()),
        OpCode::MulInt => Ok("MulInt".to_string()),
        OpCode::Div => Ok("Div".to_string()),
        OpCode::Mod => Ok("Mod".to_string()),
        OpCode::Pow => Ok("Pow".to_string()),
        OpCode::Neg => Ok("Neg".to_string()),
        OpCode::Factorial => Ok("Factorial".to_string()),
        OpCode::Not => Ok("Not".to_string()),
        OpCode::And => Ok("And".to_string()),
        OpCode::Or => Ok("Or".to_string()),
        OpCode::Eq => Ok("Eq".to_string()),
        OpCode::Neq => Ok("Neq".to_string()),
        OpCode::Gt => Ok("Gt".to_string()),
        OpCode::Gte => Ok("Gte".to_string()),
        OpCode::Lt => Ok("Lt".to_string()),
        OpCode::Lte => Ok("Lte".to_string()),
        OpCode::Jump(addr) => Ok(format!("Jump {}", addr)),
        OpCode::JumpIfFalse(addr) => Ok(format!("JumpIfFalse {}", addr)),
        OpCode::JumpIfTrue(addr) => Ok(format!("JumpIfTrue {}", addr)),
        OpCode::Call(addr) => Ok(format!("Call {}", addr)),
        OpCode::CallVar(name, argc) => Ok(format!("CallVar {} {}", name, argc)),
        OpCode::Return => Ok("Return".to_string()),
        OpCode::MakeArray(count) => Ok(format!("MakeArray {}", count)),
        OpCode::MakeStruct(count) => Ok(format!("MakeStruct {}", count)),
        OpCode::Index => Ok("Index".to_string()),
        OpCode::Member(name) => Ok(format!("Member {}", name)),
        OpCode::IndexAssign => Ok("IndexAssign".to_string()),
        OpCode::MemberAssign(name) => Ok(format!("MemberAssign {}", name)),
        OpCode::DefineFunc {
            name,
            params,
            body_start,
            body_end,
            decorators,
        } => {
            let params_str = params.join(",");
            let decorators_str = decorators.join(",");
            Ok(format!(
                "DefineFunc {} [{}] {} {} [{}]",
                name, params_str, body_start, body_end, decorators_str
            ))
        }
        OpCode::MakeLambda {
            params,
            body_start,
            body_end,
        } => {
            let params_str = params.join(",");
            Ok(format!(
                "MakeLambda [{}] {} {}",
                params_str, body_start, body_end
            ))
        }
        OpCode::EnterScope => Ok("EnterScope".to_string()),
        OpCode::ExitScope => Ok("ExitScope".to_string()),
        OpCode::LoopBegin => Ok("LoopBegin".to_string()),
        OpCode::LoopEnd => Ok("LoopEnd".to_string()),
        OpCode::Break => Ok("Break".to_string()),
        OpCode::Continue => Ok("Continue".to_string()),
        OpCode::Nop => Ok("Nop".to_string()),
        OpCode::Halt => Ok("Halt".to_string()),
    }
}

/// Deserialize an OpCode from string
fn deserialize_opcode(s: &str) -> Result<OpCode, RuminaError> {
    let s = s.trim();
    let parts: Vec<&str> = s.splitn(2, ' ').collect();
    let op_name = parts[0];

    match op_name {
        "PushConst" => {
            let val_str = parts.get(1).ok_or_else(|| {
                RuminaError::runtime("PushConst missing value".to_string())
            })?;
            let value = deserialize_value(val_str)?;
            Ok(OpCode::PushConst(value))
        }
        "PushConstPooled" => {
            let idx = parts
                .get(1)
                .ok_or_else(|| RuminaError::runtime("PushConstPooled missing index".to_string()))?
                .parse()
                .map_err(|e| RuminaError::runtime(format!("Invalid index: {}", e)))?;
            Ok(OpCode::PushConstPooled(idx))
        }
        "PushVar" => {
            let name = parts
                .get(1)
                .ok_or_else(|| RuminaError::runtime("PushVar missing name".to_string()))?
                .to_string();
            Ok(OpCode::PushVar(name))
        }
        "PopVar" => {
            let name = parts
                .get(1)
                .ok_or_else(|| RuminaError::runtime("PopVar missing name".to_string()))?
                .to_string();
            Ok(OpCode::PopVar(name))
        }
        "Dup" => Ok(OpCode::Dup),
        "Pop" => Ok(OpCode::Pop),
        "Add" => Ok(OpCode::Add),
        "AddInt" => Ok(OpCode::AddInt),
        "Sub" => Ok(OpCode::Sub),
        "SubInt" => Ok(OpCode::SubInt),
        "Mul" => Ok(OpCode::Mul),
        "MulInt" => Ok(OpCode::MulInt),
        "Div" => Ok(OpCode::Div),
        "Mod" => Ok(OpCode::Mod),
        "Pow" => Ok(OpCode::Pow),
        "Neg" => Ok(OpCode::Neg),
        "Factorial" => Ok(OpCode::Factorial),
        "Not" => Ok(OpCode::Not),
        "And" => Ok(OpCode::And),
        "Or" => Ok(OpCode::Or),
        "Eq" => Ok(OpCode::Eq),
        "Neq" => Ok(OpCode::Neq),
        "Gt" => Ok(OpCode::Gt),
        "Gte" => Ok(OpCode::Gte),
        "Lt" => Ok(OpCode::Lt),
        "Lte" => Ok(OpCode::Lte),
        "Jump" => {
            let addr = parts
                .get(1)
                .ok_or_else(|| RuminaError::runtime("Jump missing address".to_string()))?
                .parse()
                .map_err(|e| RuminaError::runtime(format!("Invalid address: {}", e)))?;
            Ok(OpCode::Jump(addr))
        }
        "JumpIfFalse" => {
            let addr = parts
                .get(1)
                .ok_or_else(|| RuminaError::runtime("JumpIfFalse missing address".to_string()))?
                .parse()
                .map_err(|e| RuminaError::runtime(format!("Invalid address: {}", e)))?;
            Ok(OpCode::JumpIfFalse(addr))
        }
        "JumpIfTrue" => {
            let addr = parts
                .get(1)
                .ok_or_else(|| RuminaError::runtime("JumpIfTrue missing address".to_string()))?
                .parse()
                .map_err(|e| RuminaError::runtime(format!("Invalid address: {}", e)))?;
            Ok(OpCode::JumpIfTrue(addr))
        }
        "Call" => {
            let addr = parts
                .get(1)
                .ok_or_else(|| RuminaError::runtime("Call missing address".to_string()))?
                .parse()
                .map_err(|e| RuminaError::runtime(format!("Invalid address: {}", e)))?;
            Ok(OpCode::Call(addr))
        }
        "CallVar" => {
            let rest = parts
                .get(1)
                .ok_or_else(|| RuminaError::runtime("CallVar missing arguments".to_string()))?;
            let mut call_parts = rest.splitn(2, ' ');
            let name = call_parts
                .next()
                .ok_or_else(|| RuminaError::runtime("CallVar missing name".to_string()))?
                .to_string();
            let argc = call_parts
                .next()
                .ok_or_else(|| RuminaError::runtime("CallVar missing argument count".to_string()))?
                .parse()
                .map_err(|e| RuminaError::runtime(format!("Invalid argument count: {}", e)))?;
            Ok(OpCode::CallVar(name, argc))
        }
        "Return" => Ok(OpCode::Return),
        "MakeArray" => {
            let count = parts
                .get(1)
                .ok_or_else(|| RuminaError::runtime("MakeArray missing count".to_string()))?
                .parse()
                .map_err(|e| RuminaError::runtime(format!("Invalid count: {}", e)))?;
            Ok(OpCode::MakeArray(count))
        }
        "MakeStruct" => {
            let count = parts
                .get(1)
                .ok_or_else(|| RuminaError::runtime("MakeStruct missing count".to_string()))?
                .parse()
                .map_err(|e| RuminaError::runtime(format!("Invalid count: {}", e)))?;
            Ok(OpCode::MakeStruct(count))
        }
        "Index" => Ok(OpCode::Index),
        "Member" => {
            let name = parts
                .get(1)
                .ok_or_else(|| RuminaError::runtime("Member missing name".to_string()))?
                .to_string();
            Ok(OpCode::Member(name))
        }
        "IndexAssign" => Ok(OpCode::IndexAssign),
        "MemberAssign" => {
            let name = parts
                .get(1)
                .ok_or_else(|| RuminaError::runtime("MemberAssign missing name".to_string()))?
                .to_string();
            Ok(OpCode::MemberAssign(name))
        }
        "DefineFunc" => {
            let rest = parts
                .get(1)
                .ok_or_else(|| RuminaError::runtime("DefineFunc missing arguments".to_string()))?;
            
            // Parse: name [params] body_start body_end [decorators]
            let name_end = rest.find('[').ok_or_else(|| {
                RuminaError::runtime("DefineFunc invalid format: missing '['".to_string())
            })?;
            let name = rest[..name_end].trim().to_string();
            
            let params_start = name_end + 1;
            let params_end = rest[params_start..].find(']').ok_or_else(|| {
                RuminaError::runtime("DefineFunc invalid format: missing ']' for params".to_string())
            })? + params_start;
            let params_str = &rest[params_start..params_end];
            let params: Vec<String> = if params_str.is_empty() {
                vec![]
            } else {
                params_str.split(',').map(|s| s.trim().to_string()).collect()
            };
            
            let after_params = &rest[params_end + 1..].trim();
            let mut parts_iter = after_params.split_whitespace();
            
            let body_start = parts_iter
                .next()
                .ok_or_else(|| RuminaError::runtime("DefineFunc missing body_start".to_string()))?
                .parse()
                .map_err(|e| RuminaError::runtime(format!("Invalid body_start: {}", e)))?;
            
            let body_end = parts_iter
                .next()
                .ok_or_else(|| RuminaError::runtime("DefineFunc missing body_end".to_string()))?
                .parse()
                .map_err(|e| RuminaError::runtime(format!("Invalid body_end: {}", e)))?;
            
            let decorators_part = parts_iter.next().unwrap_or("");
            let decorators: Vec<String> = if let Some(stripped) = decorators_part.strip_prefix('[') {
                if let Some(stripped) = stripped.strip_suffix(']') {
                    if stripped.is_empty() {
                        vec![]
                    } else {
                        stripped.split(',').map(|s| s.trim().to_string()).collect()
                    }
                } else {
                    vec![]
                }
            } else {
                vec![]
            };
            
            Ok(OpCode::DefineFunc {
                name,
                params,
                body_start,
                body_end,
                decorators,
            })
        }
        "MakeLambda" => {
            let rest = parts
                .get(1)
                .ok_or_else(|| RuminaError::runtime("MakeLambda missing arguments".to_string()))?;
            
            // Parse: [params] body_start body_end
            let params_start = rest.find('[').ok_or_else(|| {
                RuminaError::runtime("MakeLambda invalid format: missing '['".to_string())
            })? + 1;
            let params_end = rest[params_start..].find(']').ok_or_else(|| {
                RuminaError::runtime("MakeLambda invalid format: missing ']'".to_string())
            })? + params_start;
            let params_str = &rest[params_start..params_end];
            let params: Vec<String> = if params_str.is_empty() {
                vec![]
            } else {
                params_str.split(',').map(|s| s.trim().to_string()).collect()
            };
            
            let after_params = &rest[params_end + 1..].trim();
            let mut parts_iter = after_params.split_whitespace();
            
            let body_start = parts_iter
                .next()
                .ok_or_else(|| RuminaError::runtime("MakeLambda missing body_start".to_string()))?
                .parse()
                .map_err(|e| RuminaError::runtime(format!("Invalid body_start: {}", e)))?;
            
            let body_end = parts_iter
                .next()
                .ok_or_else(|| RuminaError::runtime("MakeLambda missing body_end".to_string()))?
                .parse()
                .map_err(|e| RuminaError::runtime(format!("Invalid body_end: {}", e)))?;
            
            Ok(OpCode::MakeLambda {
                params,
                body_start,
                body_end,
            })
        }
        "EnterScope" => Ok(OpCode::EnterScope),
        "ExitScope" => Ok(OpCode::ExitScope),
        "LoopBegin" => Ok(OpCode::LoopBegin),
        "LoopEnd" => Ok(OpCode::LoopEnd),
        "Break" => Ok(OpCode::Break),
        "Continue" => Ok(OpCode::Continue),
        "Nop" => Ok(OpCode::Nop),
        "Halt" => Ok(OpCode::Halt),
        _ => Err(RuminaError::runtime(format!("Unknown opcode: {}", op_name))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_serialize_deserialize_value() {
        let test_cases = vec![
            Value::Int(42),
            Value::Float(3.14),
            Value::Bool(true),
            Value::String("hello world".to_string()),
            Value::Null,
        ];

        for value in test_cases {
            let serialized = serialize_value(&value).unwrap();
            let deserialized = deserialize_value(&serialized).unwrap();
            
            match (&value, &deserialized) {
                (Value::Int(a), Value::Int(b)) => assert_eq!(a, b),
                (Value::Float(a), Value::Float(b)) => assert_eq!(a, b),
                (Value::Bool(a), Value::Bool(b)) => assert_eq!(a, b),
                (Value::String(a), Value::String(b)) => assert_eq!(a, b),
                (Value::Null, Value::Null) => {},
                _ => panic!("Value mismatch: {:?} != {:?}", value, deserialized),
            }
        }
    }

    #[test]
    fn test_save_load_bytecode() {
        let mut bytecode = ByteCode::new();
        
        // Add some constants
        let idx1 = bytecode.add_constant(Value::Int(10));
        let idx2 = bytecode.add_constant(Value::Int(20));
        
        // Add some instructions
        bytecode.emit(OpCode::PushConstPooled(idx1), Some(1));
        bytecode.emit(OpCode::PushConstPooled(idx2), Some(1));
        bytecode.emit(OpCode::Add, Some(1));
        bytecode.emit(OpCode::Halt, None);
        
        // Save to file
        let temp_path = PathBuf::from("/tmp/test_bytecode.rmc");
        save_bytecode(&bytecode, &temp_path).unwrap();
        
        // Load from file
        let loaded = load_bytecode(&temp_path).unwrap();
        
        // Verify
        assert_eq!(loaded.constants.len(), bytecode.constants.len());
        assert_eq!(loaded.instructions.len(), bytecode.instructions.len());
        
        // Clean up
        std::fs::remove_file(temp_path).ok();
    }
}
