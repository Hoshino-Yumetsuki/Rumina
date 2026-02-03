// Example demonstrating x86_64-style register-based VM bytecode
//
// This example shows how to create and execute register-based bytecode
// The bytecode computes: (10 + 5) * 2 = 30

use rumina::vm::{ByteCode, OpCode, Register, VM};
use rumina::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

fn main() {
    println!("=== x86_64-style Register-Based VM Example ===\n");
    
    // Create VM with empty globals
    let globals = Rc::new(RefCell::new(HashMap::new()));
    let mut vm = VM::new(globals);
    
    // Create bytecode program
    let mut bytecode = ByteCode::new();
    
    println!("Program: (10 + 5) * 2\n");
    println!("Assembly-style bytecode:");
    println!("  MOV RAX, 10      ; Load 10 into RAX");
    println!("  MOV RBX, 5       ; Load 5 into RBX");
    println!("  ADD RCX, RAX, RBX ; RCX = RAX + RBX (10 + 5 = 15)");
    println!("  MOV RDX, 2       ; Load 2 into RDX");
    println!("  MUL RAX, RCX, RDX ; RAX = RCX * RDX (15 * 2 = 30)");
    println!("  HALT             ; Stop execution\n");
    
    // Emit instructions
    bytecode.emit(OpCode::MovConst(Register::RAX, Value::Int(10)), Some(1));
    bytecode.emit(OpCode::MovConst(Register::RBX, Value::Int(5)), Some(2));
    bytecode.emit(OpCode::AddReg(Register::RCX, Register::RAX, Register::RBX), Some(3));
    bytecode.emit(OpCode::MovConst(Register::RDX, Value::Int(2)), Some(4));
    bytecode.emit(OpCode::MulReg(Register::RAX, Register::RCX, Register::RDX), Some(5));
    bytecode.emit(OpCode::Halt, Some(6));
    
    // Show serialized bytecode
    println!("Serialized bytecode format:");
    println!("{}", bytecode.serialize());
    
    // Load and execute
    vm.load(bytecode);
    match vm.run() {
        Ok(Some(result)) => {
            println!("Result from RAX register: {}", result);
        }
        Ok(None) => {
            println!("No result returned");
        }
        Err(e) => {
            println!("Error: {}", e);
        }
    }
}
