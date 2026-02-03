/// Tests for x86_64-style register-based VM instructions
use rumina::vm::{ByteCode, OpCode, Register, VM};
use rumina::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[test]
fn test_mov_const_to_register() {
    let globals = Rc::new(RefCell::new(HashMap::new()));
    let mut vm = VM::new(globals);
    
    let mut bytecode = ByteCode::new();
    // MOV RAX, 42
    bytecode.emit(OpCode::MovConst(Register::RAX, Value::Int(42)), Some(1));
    bytecode.emit(OpCode::Halt, Some(2));
    
    vm.load(bytecode);
    let result = vm.run().expect("VM should execute");
    
    assert_eq!(result, Some(Value::Int(42)));
}

#[test]
fn test_register_arithmetic() {
    let globals = Rc::new(RefCell::new(HashMap::new()));
    let mut vm = VM::new(globals);
    
    let mut bytecode = ByteCode::new();
    // MOV RAX, 10
    bytecode.emit(OpCode::MovConst(Register::RAX, Value::Int(10)), Some(1));
    // MOV RBX, 5
    bytecode.emit(OpCode::MovConst(Register::RBX, Value::Int(5)), Some(2));
    // ADD RCX, RAX, RBX  ; RCX = 10 + 5 = 15
    bytecode.emit(OpCode::AddReg(Register::RCX, Register::RAX, Register::RBX), Some(3));
    // MOV RAX, RCX  ; Return value
    bytecode.emit(OpCode::MovReg(Register::RAX, Register::RCX), Some(4));
    bytecode.emit(OpCode::Halt, Some(5));
    
    vm.load(bytecode);
    let result = vm.run().expect("VM should execute");
    
    assert_eq!(result, Some(Value::Int(15)));
}

#[test]
fn test_register_multiply_and_subtract() {
    let globals = Rc::new(RefCell::new(HashMap::new()));
    let mut vm = VM::new(globals);
    
    let mut bytecode = ByteCode::new();
    // MOV RAX, 7
    bytecode.emit(OpCode::MovConst(Register::RAX, Value::Int(7)), Some(1));
    // MOV RBX, 3
    bytecode.emit(OpCode::MovConst(Register::RBX, Value::Int(3)), Some(2));
    // MUL RCX, RAX, RBX  ; RCX = 7 * 3 = 21
    bytecode.emit(OpCode::MulReg(Register::RCX, Register::RAX, Register::RBX), Some(3));
    // MOV RDX, 6
    bytecode.emit(OpCode::MovConst(Register::RDX, Value::Int(6)), Some(4));
    // SUB RAX, RCX, RDX  ; RAX = 21 - 6 = 15
    bytecode.emit(OpCode::SubReg(Register::RAX, Register::RCX, Register::RDX), Some(5));
    bytecode.emit(OpCode::Halt, Some(6));
    
    vm.load(bytecode);
    let result = vm.run().expect("VM should execute");
    
    assert_eq!(result, Some(Value::Int(15)));
}

#[test]
fn test_register_comparison() {
    let globals = Rc::new(RefCell::new(HashMap::new()));
    let mut vm = VM::new(globals);
    
    let mut bytecode = ByteCode::new();
    // MOV RAX, 10
    bytecode.emit(OpCode::MovConst(Register::RAX, Value::Int(10)), Some(1));
    // MOV RBX, 5
    bytecode.emit(OpCode::MovConst(Register::RBX, Value::Int(5)), Some(2));
    // GT RCX, RAX, RBX  ; RCX = (10 > 5) = true
    bytecode.emit(OpCode::GtReg(Register::RCX, Register::RAX, Register::RBX), Some(3));
    // MOV RAX, RCX  ; Return value
    bytecode.emit(OpCode::MovReg(Register::RAX, Register::RCX), Some(4));
    bytecode.emit(OpCode::Halt, Some(5));
    
    vm.load(bytecode);
    let result = vm.run().expect("VM should execute");
    
    assert_eq!(result, Some(Value::Bool(true)));
}

#[test]
fn test_register_logical_operations() {
    let globals = Rc::new(RefCell::new(HashMap::new()));
    let mut vm = VM::new(globals);
    
    let mut bytecode = ByteCode::new();
    // MOV RAX, true
    bytecode.emit(OpCode::MovConst(Register::RAX, Value::Bool(true)), Some(1));
    // MOV RBX, false
    bytecode.emit(OpCode::MovConst(Register::RBX, Value::Bool(false)), Some(2));
    // AND RCX, RAX, RBX  ; RCX = true && false = false
    bytecode.emit(OpCode::AndReg(Register::RCX, Register::RAX, Register::RBX), Some(3));
    // OR RDX, RAX, RBX   ; RDX = true || false = true
    bytecode.emit(OpCode::OrReg(Register::RDX, Register::RAX, Register::RBX), Some(4));
    // NOT RAX, RBX       ; RAX = !false = true
    bytecode.emit(OpCode::NotReg(Register::RAX, Register::RBX), Some(5));
    bytecode.emit(OpCode::Halt, Some(6));
    
    vm.load(bytecode);
    let result = vm.run().expect("VM should execute");
    
    assert_eq!(result, Some(Value::Bool(true)));
}

#[test]
fn test_register_with_variables() {
    let globals = Rc::new(RefCell::new(HashMap::new()));
    let mut vm = VM::new(globals);
    
    let mut bytecode = ByteCode::new();
    // MOV RAX, 100
    bytecode.emit(OpCode::MovConst(Register::RAX, Value::Int(100)), Some(1));
    // MOV x, RAX  ; Store 100 in variable x
    bytecode.emit(OpCode::MovToVar("x".to_string(), Register::RAX), Some(2));
    // MOV RBX, x  ; Load x into RBX
    bytecode.emit(OpCode::MovVar(Register::RBX, "x".to_string()), Some(3));
    // MOV RCX, 50
    bytecode.emit(OpCode::MovConst(Register::RCX, Value::Int(50)), Some(4));
    // ADD RAX, RBX, RCX  ; RAX = 100 + 50 = 150
    bytecode.emit(OpCode::AddReg(Register::RAX, Register::RBX, Register::RCX), Some(5));
    bytecode.emit(OpCode::Halt, Some(6));
    
    vm.load(bytecode);
    let result = vm.run().expect("VM should execute");
    
    assert_eq!(result, Some(Value::Int(150)));
}

#[test]
fn test_register_constant_pool() {
    let globals = Rc::new(RefCell::new(HashMap::new()));
    let mut vm = VM::new(globals);
    
    let mut bytecode = ByteCode::new();
    // Add constants to pool
    let idx1 = bytecode.add_constant(Value::Int(42));
    let idx2 = bytecode.add_constant(Value::Int(8));
    
    // MOV RAX, [const_pool + idx1]
    bytecode.emit(OpCode::MovConstPooled(Register::RAX, idx1), Some(1));
    // MOV RBX, [const_pool + idx2]
    bytecode.emit(OpCode::MovConstPooled(Register::RBX, idx2), Some(2));
    // MUL RAX, RAX, RBX  ; RAX = 42 * 8 = 336
    bytecode.emit(OpCode::MulReg(Register::RAX, Register::RAX, Register::RBX), Some(3));
    bytecode.emit(OpCode::Halt, Some(4));
    
    vm.load(bytecode);
    let result = vm.run().expect("VM should execute");
    
    assert_eq!(result, Some(Value::Int(336)));
}

#[test]
fn test_bytecode_serialization_with_registers() {
    let mut bytecode = ByteCode::new();
    
    // Add some register-based instructions
    bytecode.emit(OpCode::MovConst(Register::RAX, Value::Int(42)), Some(1));
    bytecode.emit(OpCode::MovConst(Register::RBX, Value::Int(10)), Some(2));
    bytecode.emit(OpCode::AddReg(Register::RCX, Register::RAX, Register::RBX), Some(3));
    bytecode.emit(OpCode::MovReg(Register::RAX, Register::RCX), Some(4));
    bytecode.emit(OpCode::Halt, Some(5));
    
    // Serialize and deserialize
    let serialized = bytecode.serialize();
    let deserialized = ByteCode::deserialize(&serialized).expect("Should deserialize");
    
    // Verify instructions match
    assert_eq!(bytecode.instructions.len(), deserialized.instructions.len());
    assert_eq!(bytecode.instructions, deserialized.instructions);
}

#[test]
fn test_backward_compatibility_stack_and_registers() {
    let globals = Rc::new(RefCell::new(HashMap::new()));
    let mut vm = VM::new(globals);
    
    let mut bytecode = ByteCode::new();
    // Use register instruction to load value
    bytecode.emit(OpCode::MovConst(Register::RAX, Value::Int(10)), Some(1));
    bytecode.emit(OpCode::MovConst(Register::RBX, Value::Int(5)), Some(2));
    bytecode.emit(OpCode::AddReg(Register::RAX, Register::RAX, Register::RBX), Some(3));
    // RAX now has 15
    bytecode.emit(OpCode::Halt, Some(4));
    
    vm.load(bytecode);
    let result = vm.run().expect("VM should execute");
    
    assert_eq!(result, Some(Value::Int(15)));
}
