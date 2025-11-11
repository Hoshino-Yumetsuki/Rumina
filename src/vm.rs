/// Virtual Machine implementation for Rumina
///
/// This module implements a bytecode VM with an x86_64-inspired instruction set.
/// The VM uses a stack-based execution model with register-like local variables.
use crate::error::RuminaError;
use crate::value::Value;
use crate::vm_ops::VMOperations;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// VM Instruction Set (x86_64-inspired CISC design)
#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    // ===== Data Movement Instructions (MOV family) =====
    /// Push a constant value onto the stack
    /// Similar to: PUSH imm
    PushConst(Value),

    /// Push a variable value onto the stack
    /// Similar to: MOV reg, [addr]
    PushVar(String),

    /// Pop value from stack and store in variable
    /// Similar to: MOV [addr], reg
    PopVar(String),

    /// Duplicate top stack value
    /// Similar to: PUSH [rsp]
    Dup,

    /// Pop and discard top stack value
    /// Similar to: ADD rsp, 8
    Pop,

    // ===== Arithmetic Instructions (ADD, SUB, MUL, DIV family) =====
    /// Add top two stack values
    /// Similar to: ADD rax, rbx
    Add,

    /// Subtract top two stack values (TOS-1 - TOS)
    /// Similar to: SUB rax, rbx
    Sub,

    /// Multiply top two stack values
    /// Similar to: MUL rbx
    Mul,

    /// Divide top two stack values (TOS-1 / TOS)
    /// Similar to: DIV rbx
    Div,

    /// Modulo operation
    /// Similar to: IDIV (with remainder in rdx)
    Mod,

    /// Power operation (TOS-1 ^ TOS)
    Pow,

    /// Negate top stack value
    /// Similar to: NEG rax
    Neg,

    /// Factorial operation (postfix !)
    Factorial,

    // ===== Logical Instructions (CMP, TEST, AND, OR family) =====
    /// Logical NOT
    /// Similar to: NOT rax
    Not,

    /// Logical AND
    /// Similar to: AND rax, rbx
    And,

    /// Logical OR
    /// Similar to: OR rax, rbx
    Or,

    /// Compare equal
    /// Similar to: CMP + SETE
    Eq,

    /// Compare not equal
    /// Similar to: CMP + SETNE
    Neq,

    /// Compare greater than
    /// Similar to: CMP + SETG
    Gt,

    /// Compare greater or equal
    /// Similar to: CMP + SETGE
    Gte,

    /// Compare less than
    /// Similar to: CMP + SETL
    Lt,

    /// Compare less or equal
    /// Similar to: CMP + SETLE
    Lte,

    // ===== Control Flow Instructions (JMP, CALL, RET family) =====
    /// Unconditional jump to address
    /// Similar to: JMP addr
    Jump(usize),

    /// Jump if false (pop condition from stack)
    /// Similar to: TEST + JZ
    JumpIfFalse(usize),

    /// Jump if true (pop condition from stack)
    /// Similar to: TEST + JNZ
    JumpIfTrue(usize),

    /// Call function (address to jump to)
    /// Similar to: CALL addr
    Call(usize),

    /// Call function by name/variable
    /// Similar to: CALL [addr]
    CallVar(String, usize), // (function name, argument count)

    /// Return from function
    /// Similar to: RET
    Return,

    // ===== Array/Structure Instructions (LEA, MOV family) =====
    /// Create array from N stack values
    /// Similar to: MOV [addr], ... (repeated)
    MakeArray(usize),

    /// Create struct from N key-value pairs on stack
    MakeStruct(usize),

    /// Array index access (push array[index])
    /// Similar to: MOV rax, [rbx + rcx*8]
    Index,

    /// Member access (push obj.member)
    /// Similar to: MOV rax, [rbx + offset]
    Member(String),

    /// Assign to array element
    IndexAssign,

    /// Assign to struct member
    MemberAssign(String),

    // ===== Function Definition Instructions =====
    /// Define a function
    DefineFunc {
        name: String,
        params: Vec<String>,
        body_start: usize,
        body_end: usize,
        decorators: Vec<String>,
    },

    /// Create lambda/closure
    MakeLambda {
        params: Vec<String>,
        body_start: usize,
        body_end: usize,
    },

    // ===== Scope Management =====
    /// Enter new local scope
    /// Similar to: PUSH rbp; MOV rbp, rsp
    EnterScope,

    /// Exit local scope
    /// Similar to: MOV rsp, rbp; POP rbp
    ExitScope,

    // ===== Control Structures =====
    /// Begin loop (mark position for continue)
    LoopBegin,

    /// End loop (mark position for break)
    LoopEnd,

    /// Break from loop
    Break,

    /// Continue loop
    Continue,

    // ===== Special Instructions =====
    /// No operation
    /// Similar to: NOP
    Nop,

    /// Halt execution
    /// Similar to: HLT
    Halt,
}

/// Bytecode chunk - compiled function or program
#[derive(Debug, Clone)]
pub struct ByteCode {
    /// Sequence of instructions
    pub instructions: Vec<OpCode>,

    /// Debug information: instruction -> line number mapping
    pub line_numbers: Vec<Option<usize>>,

    /// Constants pool (for optimization)
    pub constants: Vec<Value>,
}

impl ByteCode {
    pub fn new() -> Self {
        ByteCode {
            instructions: Vec::new(),
            line_numbers: Vec::new(),
            constants: Vec::new(),
        }
    }

    /// Add an instruction
    pub fn emit(&mut self, op: OpCode, line: Option<usize>) {
        self.instructions.push(op);
        self.line_numbers.push(line);
    }

    /// Get current instruction pointer (for jumps)
    pub fn current_address(&self) -> usize {
        self.instructions.len()
    }

    /// Patch a jump instruction at given address
    pub fn patch_jump(&mut self, address: usize, target: usize) {
        match &mut self.instructions[address] {
            OpCode::Jump(addr) | OpCode::JumpIfFalse(addr) | OpCode::JumpIfTrue(addr) => {
                *addr = target;
            }
            _ => panic!("Attempted to patch non-jump instruction at {}", address),
        }
    }
}

/// Function metadata for user-defined functions
#[derive(Debug, Clone)]
struct FunctionInfo {
    /// Function name
    name: String,
    /// Parameter names
    params: Vec<String>,
    /// Start address in bytecode
    body_start: usize,
    /// End address in bytecode
    body_end: usize,
}

/// Call frame for function calls
#[derive(Debug, Clone)]
struct CallFrame {
    /// Return address (instruction pointer to return to)
    return_address: usize,

    /// Base pointer for local variables
    base_pointer: usize,

    /// Function name (for error reporting)
    function_name: String,

    /// Local variables in this frame
    locals: HashMap<String, Value>,
}

/// Virtual Machine state
pub struct VM {
    /// Bytecode being executed
    bytecode: ByteCode,

    /// Instruction pointer
    ip: usize,

    /// Data stack
    stack: Vec<Value>,

    /// Call stack (for function calls)
    call_stack: Vec<CallFrame>,

    /// Global variables
    globals: Rc<RefCell<HashMap<String, Value>>>,

    /// Current local variables (top of call stack)
    locals: HashMap<String, Value>,

    /// Loop break/continue targets
    loop_stack: Vec<(usize, usize)>, // (continue_target, break_target)

    /// Function table: maps function names to their bytecode locations
    functions: HashMap<String, FunctionInfo>,

    /// Halt flag
    halted: bool,

    /// Recursion depth tracking
    recursion_depth: usize,
    max_recursion_depth: usize,
}

impl VM {
    /// Create new VM instance
    pub fn new(globals: Rc<RefCell<HashMap<String, Value>>>) -> Self {
        VM {
            bytecode: ByteCode::new(),
            ip: 0,
            stack: Vec::new(),
            call_stack: Vec::new(),
            globals,
            locals: HashMap::new(),
            loop_stack: Vec::new(),
            functions: HashMap::new(),
            halted: false,
            recursion_depth: 0,
            max_recursion_depth: 4000,
        }
    }

    /// Load bytecode into VM
    pub fn load(&mut self, bytecode: ByteCode) {
        self.bytecode = bytecode;
        self.ip = 0;
        self.halted = false;
    }

    /// Execute loaded bytecode
    pub fn run(&mut self) -> Result<Option<Value>, RuminaError> {
        while !self.halted && self.ip < self.bytecode.instructions.len() {
            let op = self.bytecode.instructions[self.ip].clone();
            self.ip += 1;

            self.execute_instruction(op)?;
        }

        // Return top of stack if present, otherwise None
        Ok(self.stack.pop())
    }

    /// Execute a single instruction
    fn execute_instruction(&mut self, op: OpCode) -> Result<(), RuminaError> {
        match op {
            OpCode::PushConst(value) => {
                self.stack.push(value);
            }

            OpCode::PushVar(name) => {
                let value = self.get_variable(&name)?;
                self.stack.push(value);
            }

            OpCode::PopVar(name) => {
                let value = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;
                self.set_variable(name, value);
            }

            OpCode::Dup => {
                let value = self
                    .stack
                    .last()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?
                    .clone();
                self.stack.push(value);
            }

            OpCode::Pop => {
                self.stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;
            }

            OpCode::Add => self.binary_op(|a, b| a.vm_add(b))?,
            OpCode::Sub => self.binary_op(|a, b| a.vm_sub(b))?,
            OpCode::Mul => self.binary_op(|a, b| a.vm_mul(b))?,
            OpCode::Div => self.binary_op(|a, b| a.vm_div(b))?,
            OpCode::Mod => self.binary_op(|a, b| a.vm_mod(b))?,
            OpCode::Pow => self.binary_op(|a, b| a.vm_pow(b))?,

            OpCode::Neg => {
                let value = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;
                let result = value.vm_neg().map_err(|e| RuminaError::runtime(e))?;
                self.stack.push(result);
            }

            OpCode::Not => {
                let value = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;
                let result = value.vm_not().map_err(|e| RuminaError::runtime(e))?;
                self.stack.push(result);
            }

            OpCode::Factorial => {
                let value = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;
                let result = value.vm_factorial().map_err(|e| RuminaError::runtime(e))?;
                self.stack.push(result);
            }

            // Comparison operations
            OpCode::Eq => self.binary_op(|a, b| a.vm_eq(b))?,
            OpCode::Neq => self.binary_op(|a, b| a.vm_neq(b))?,
            OpCode::Gt => self.binary_op(|a, b| a.vm_gt(b))?,
            OpCode::Gte => self.binary_op(|a, b| a.vm_gte(b))?,
            OpCode::Lt => self.binary_op(|a, b| a.vm_lt(b))?,
            OpCode::Lte => self.binary_op(|a, b| a.vm_lte(b))?,

            // Logical operations
            OpCode::And => self.binary_op(|a, b| a.vm_and(b))?,
            OpCode::Or => self.binary_op(|a, b| a.vm_or(b))?,

            // Control flow
            OpCode::Jump(addr) => {
                self.ip = addr;
            }

            OpCode::JumpIfFalse(addr) => {
                let condition = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;
                if !condition.is_truthy() {
                    self.ip = addr;
                }
            }

            OpCode::JumpIfTrue(addr) => {
                let condition = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;
                if condition.is_truthy() {
                    self.ip = addr;
                }
            }

            // Array/Struct operations
            OpCode::MakeArray(count) => {
                let mut elements = Vec::new();
                for _ in 0..count {
                    let elem = self
                        .stack
                        .pop()
                        .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;
                    elements.push(elem);
                }
                elements.reverse(); // Restore original order
                self.stack
                    .push(Value::Array(Rc::new(RefCell::new(elements))));
            }

            OpCode::Index => {
                let index = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;
                let array = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;

                match &array {
                    Value::Array(arr) => {
                        if let Value::Int(idx) = index {
                            let arr_ref = arr.borrow();
                            let idx = if idx < 0 {
                                (arr_ref.len() as i64 + idx) as usize
                            } else {
                                idx as usize
                            };

                            if idx < arr_ref.len() {
                                self.stack.push(arr_ref[idx].clone());
                            } else {
                                return Err(RuminaError::runtime(format!(
                                    "Array index out of bounds: {} (length: {})",
                                    idx,
                                    arr_ref.len()
                                )));
                            }
                        } else {
                            return Err(RuminaError::runtime(
                                "Array index must be an integer".to_string(),
                            ));
                        }
                    }
                    Value::String(s) => {
                        if let Value::Int(idx) = index {
                            let idx = if idx < 0 {
                                (s.len() as i64 + idx) as usize
                            } else {
                                idx as usize
                            };

                            if idx < s.len() {
                                let ch = s.chars().nth(idx).unwrap();
                                self.stack.push(Value::String(ch.to_string()));
                            } else {
                                return Err(RuminaError::runtime(format!(
                                    "String index out of bounds: {} (length: {})",
                                    idx,
                                    s.len()
                                )));
                            }
                        } else {
                            return Err(RuminaError::runtime(
                                "String index must be an integer".to_string(),
                            ));
                        }
                    }
                    _ => {
                        return Err(RuminaError::runtime(format!(
                            "Cannot index type {}",
                            array.type_name()
                        )));
                    }
                }
            }

            OpCode::Member(member_name) => {
                let object = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;

                match &object {
                    Value::Struct(s) => {
                        let s_ref = s.borrow();
                        if let Some(value) = s_ref.get(&member_name) {
                            self.stack.push(value.clone());
                        } else {
                            return Err(RuminaError::runtime(format!(
                                "Struct does not have member '{}'",
                                member_name
                            )));
                        }
                    }
                    _ => {
                        return Err(RuminaError::runtime(format!(
                            "Cannot access member of type {}",
                            object.type_name()
                        )));
                    }
                }
            }

            OpCode::EnterScope => {
                // Push a new local scope frame
                // For now, we'll handle this with the existing locals HashMap
                // In a full implementation, we'd push a new scope onto a scope stack
            }

            OpCode::ExitScope => {
                // Pop the local scope frame
                // For now, this is a no-op as we clear locals on function return
            }

            OpCode::Return => {
                // Pop the call frame and jump back
                if let Some(frame) = self.call_stack.pop() {
                    // Decrement recursion depth
                    self.recursion_depth = self.recursion_depth.saturating_sub(1);

                    // The return value is already on the stack (or Null was pushed)
                    // Restore the instruction pointer to return address
                    self.ip = frame.return_address;

                    // Restore the previous local variables
                    self.locals = frame.locals;
                } else {
                    // Top-level return - halt execution
                    self.halted = true;
                }
            }

            OpCode::Break => {
                if let Some((_, break_target)) = self.loop_stack.last() {
                    self.ip = *break_target;
                } else {
                    return Err(RuminaError::runtime("Break outside of loop".to_string()));
                }
            }

            OpCode::Continue => {
                if let Some((continue_target, _)) = self.loop_stack.last() {
                    self.ip = *continue_target;
                } else {
                    return Err(RuminaError::runtime("Continue outside of loop".to_string()));
                }
            }

            OpCode::LoopBegin => {
                // Mark loop begin - will be set by compiler
            }

            OpCode::LoopEnd => {
                // Mark loop end - will be set by compiler
            }

            OpCode::DefineFunc {
                name,
                params,
                body_start,
                body_end,
                decorators,
            } => {
                // Store function metadata in function table
                self.functions.insert(
                    name.clone(),
                    FunctionInfo {
                        name: name.clone(),
                        params: params.clone(),
                        body_start,
                        body_end,
                    },
                );

                // Store function in globals as well (for compatibility)
                let func_value = Value::Function {
                    name: name.clone(),
                    params: params.clone(),
                    body: Box::new(crate::ast::Stmt::Block(vec![])), // Placeholder
                    decorators: decorators.clone(),
                };
                self.globals.borrow_mut().insert(name, func_value);
            }

            OpCode::CallVar(func_name, arg_count) => {
                // Get the function
                let func = self.get_variable(&func_name)?;

                // Pop arguments from stack
                let mut args = Vec::new();
                for _ in 0..arg_count {
                    let arg = self
                        .stack
                        .pop()
                        .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;
                    args.push(arg);
                }
                args.reverse(); // Restore original order

                // Call the function
                match func {
                    Value::NativeFunction {
                        func: native_fn, ..
                    } => {
                        // Call native function
                        let result = native_fn(&args).map_err(|e| RuminaError::runtime(e))?;
                        self.stack.push(result);
                    }
                    Value::Function { .. } => {
                        // Check if we have the function in our function table
                        if let Some(func_info) = self.functions.get(&func_name).cloned() {
                            // Check recursion depth
                            if self.recursion_depth >= self.max_recursion_depth {
                                return Err(RuminaError::runtime(format!(
                                    "Maximum recursion depth ({}) exceeded",
                                    self.max_recursion_depth
                                )));
                            }

                            // Check parameter count
                            if args.len() != func_info.params.len() {
                                return Err(RuminaError::runtime(format!(
                                    "Function '{}' expects {} arguments, got {}",
                                    func_name,
                                    func_info.params.len(),
                                    args.len()
                                )));
                            }

                            // Create new call frame
                            let frame = CallFrame {
                                return_address: self.ip,
                                base_pointer: self.stack.len(),
                                function_name: func_name.clone(),
                                locals: std::mem::take(&mut self.locals), // Save current locals
                            };

                            // Push call frame
                            self.call_stack.push(frame);
                            self.recursion_depth += 1;

                            // Set up parameters as local variables
                            self.locals.clear();
                            for (param_name, arg_value) in
                                func_info.params.iter().zip(args.into_iter())
                            {
                                self.locals.insert(param_name.clone(), arg_value);
                            }

                            // Jump to function body
                            self.ip = func_info.body_start;
                        } else {
                            return Err(RuminaError::runtime(format!(
                                "Function '{}' not found in function table",
                                func_name
                            )));
                        }
                    }
                    _ => {
                        return Err(RuminaError::runtime(format!(
                            "Cannot call type {}",
                            func.type_name()
                        )));
                    }
                }
            }

            OpCode::MakeStruct(field_count) => {
                let mut fields = HashMap::new();
                for _ in 0..field_count {
                    // Pop value
                    let value = self
                        .stack
                        .pop()
                        .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;
                    // Pop key (should be a string)
                    let key = self
                        .stack
                        .pop()
                        .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;

                    if let Value::String(key_str) = key {
                        fields.insert(key_str, value);
                    } else {
                        return Err(RuminaError::runtime(
                            "Struct key must be a string".to_string(),
                        ));
                    }
                }
                self.stack
                    .push(Value::Struct(Rc::new(RefCell::new(fields))));
            }

            OpCode::MemberAssign(_)
            | OpCode::IndexAssign
            | OpCode::MakeLambda { .. }
            | OpCode::Call(_) => {
                return Err(RuminaError::runtime(
                    "Opcode not yet implemented".to_string(),
                ));
            }

            OpCode::Halt => {
                self.halted = true;
            }

            OpCode::Nop => {
                // Do nothing
            }
        }

        Ok(())
    }

    /// Helper for binary operations
    fn binary_op<F>(&mut self, f: F) -> Result<(), RuminaError>
    where
        F: FnOnce(&Value, &Value) -> Result<Value, String>,
    {
        let right = self
            .stack
            .pop()
            .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;
        let left = self
            .stack
            .pop()
            .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;

        let result = f(&left, &right).map_err(|e| RuminaError::runtime(e))?;

        self.stack.push(result);
        Ok(())
    }

    /// Get variable from locals or globals
    fn get_variable(&self, name: &str) -> Result<Value, RuminaError> {
        // Check locals first
        if let Some(value) = self.locals.get(name) {
            return Ok(value.clone());
        }

        // Check globals
        if let Some(value) = self.globals.borrow().get(name) {
            return Ok(value.clone());
        }

        Err(RuminaError::runtime(format!(
            "Undefined variable: {}",
            name
        )))
    }

    /// Set variable (in globals if at top level, otherwise in locals)
    fn set_variable(&mut self, name: String, value: Value) {
        // If we're at the top level (no active call frames), store in globals
        // This allows REPL state to persist across executions
        if self.call_stack.is_empty() {
            self.globals.borrow_mut().insert(name, value);
        } else {
            // Inside a function, use locals
            self.locals.insert(name, value);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vm_push_pop() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();
        bytecode.emit(OpCode::PushConst(Value::Int(42)), None);
        bytecode.emit(OpCode::PushConst(Value::Int(10)), None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        assert_eq!(result, Some(Value::Int(10)));
    }

    #[test]
    fn test_bytecode_emit() {
        let mut bytecode = ByteCode::new();
        bytecode.emit(OpCode::PushConst(Value::Int(1)), Some(1));
        bytecode.emit(OpCode::Add, Some(1));

        assert_eq!(bytecode.instructions.len(), 2);
        assert_eq!(bytecode.line_numbers.len(), 2);
    }

    #[test]
    fn test_vm_arithmetic() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();
        bytecode.emit(OpCode::PushConst(Value::Int(10)), None);
        bytecode.emit(OpCode::PushConst(Value::Int(5)), None);
        bytecode.emit(OpCode::Add, None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        match result {
            Some(Value::Int(n)) => assert_eq!(n, 15),
            _ => panic!("Expected Int(15)"),
        }
    }

    #[test]
    fn test_vm_variables() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();
        // x = 42
        bytecode.emit(OpCode::PushConst(Value::Int(42)), None);
        bytecode.emit(OpCode::PopVar("x".to_string()), None);
        // push x
        bytecode.emit(OpCode::PushVar("x".to_string()), None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        match result {
            Some(Value::Int(n)) => assert_eq!(n, 42),
            _ => panic!("Expected Int(42)"),
        }
    }

    #[test]
    fn test_vm_comparison() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();
        bytecode.emit(OpCode::PushConst(Value::Int(10)), None);
        bytecode.emit(OpCode::PushConst(Value::Int(5)), None);
        bytecode.emit(OpCode::Gt, None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        match result {
            Some(Value::Bool(b)) => assert_eq!(b, true),
            _ => panic!("Expected Bool(true)"),
        }
    }

    #[test]
    fn test_vm_array() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();
        bytecode.emit(OpCode::PushConst(Value::Int(1)), None);
        bytecode.emit(OpCode::PushConst(Value::Int(2)), None);
        bytecode.emit(OpCode::PushConst(Value::Int(3)), None);
        bytecode.emit(OpCode::MakeArray(3), None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        match result {
            Some(Value::Array(arr)) => {
                let arr_ref = arr.borrow();
                assert_eq!(arr_ref.len(), 3);
            }
            _ => panic!("Expected Array"),
        }
    }

    #[test]
    fn test_vm_native_function_call() {
        // Create a simple native function
        fn test_add(args: &[Value]) -> Result<Value, String> {
            if args.len() != 2 {
                return Err("Expected 2 arguments".to_string());
            }

            match (&args[0], &args[1]) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                _ => Err("Expected integers".to_string()),
            }
        }

        let mut globals_map = HashMap::new();
        globals_map.insert(
            "test_add".to_string(),
            Value::NativeFunction {
                name: "test_add".to_string(),
                func: test_add,
            },
        );

        let globals = Rc::new(RefCell::new(globals_map));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();
        // Push arguments in order: 10, 20
        bytecode.emit(OpCode::PushConst(Value::Int(10)), None);
        bytecode.emit(OpCode::PushConst(Value::Int(20)), None);
        // Call test_add with 2 arguments
        bytecode.emit(OpCode::CallVar("test_add".to_string(), 2), None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        match result {
            Some(Value::Int(n)) => assert_eq!(n, 30),
            other => panic!("Expected Int(30), got {:?}", other),
        }
    }

    #[test]
    fn test_vm_user_defined_function() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();

        // func add(a, b) { return a + b; }
        // Skip function definition
        let skip_jump_addr = bytecode.current_address();
        bytecode.emit(OpCode::Jump(0), None);

        // Function body starts here
        let body_start = bytecode.current_address();
        bytecode.emit(OpCode::PushVar("a".to_string()), None);
        bytecode.emit(OpCode::PushVar("b".to_string()), None);
        bytecode.emit(OpCode::Add, None);
        bytecode.emit(OpCode::Return, None);
        let body_end = bytecode.current_address();

        // Patch skip jump to here
        bytecode.patch_jump(skip_jump_addr, body_end);

        // Define the function
        bytecode.emit(
            OpCode::DefineFunc {
                name: "add".to_string(),
                params: vec!["a".to_string(), "b".to_string()],
                body_start,
                body_end,
                decorators: vec![],
            },
            None,
        );

        // Call the function: add(5, 7)
        bytecode.emit(OpCode::PushConst(Value::Int(5)), None);
        bytecode.emit(OpCode::PushConst(Value::Int(7)), None);
        bytecode.emit(OpCode::CallVar("add".to_string(), 2), None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        match result {
            Some(Value::Int(n)) => assert_eq!(n, 12),
            other => panic!("Expected Int(12), got {:?}", other),
        }
    }

    #[test]
    fn test_vm_recursive_function() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();

        // func fib(n) {
        //     if (n <= 1) { return n; }
        //     return fib(n - 1) + fib(n - 2);
        // }

        // Skip function definition
        let skip_jump_addr = bytecode.current_address();
        bytecode.emit(OpCode::Jump(0), None);

        // Function body starts here
        let body_start = bytecode.current_address();

        // if (n <= 1)
        bytecode.emit(OpCode::PushVar("n".to_string()), None);
        bytecode.emit(OpCode::PushConst(Value::Int(1)), None);
        bytecode.emit(OpCode::Lte, None);
        let else_jump = bytecode.current_address();
        bytecode.emit(OpCode::JumpIfFalse(0), None);

        // then: return n
        bytecode.emit(OpCode::PushVar("n".to_string()), None);
        bytecode.emit(OpCode::Return, None);

        // Patch else jump to here
        bytecode.patch_jump(else_jump, bytecode.current_address());

        // else: return fib(n - 1) + fib(n - 2)
        // fib(n - 1)
        bytecode.emit(OpCode::PushVar("n".to_string()), None);
        bytecode.emit(OpCode::PushConst(Value::Int(1)), None);
        bytecode.emit(OpCode::Sub, None);
        bytecode.emit(OpCode::CallVar("fib".to_string(), 1), None);

        // fib(n - 2)
        bytecode.emit(OpCode::PushVar("n".to_string()), None);
        bytecode.emit(OpCode::PushConst(Value::Int(2)), None);
        bytecode.emit(OpCode::Sub, None);
        bytecode.emit(OpCode::CallVar("fib".to_string(), 1), None);

        // Add results
        bytecode.emit(OpCode::Add, None);
        bytecode.emit(OpCode::Return, None);

        let body_end = bytecode.current_address();

        // Patch skip jump to here
        bytecode.patch_jump(skip_jump_addr, body_end);

        // Define the function
        bytecode.emit(
            OpCode::DefineFunc {
                name: "fib".to_string(),
                params: vec!["n".to_string()],
                body_start,
                body_end,
                decorators: vec![],
            },
            None,
        );

        // Call the function: fib(10)
        bytecode.emit(OpCode::PushConst(Value::Int(10)), None);
        bytecode.emit(OpCode::CallVar("fib".to_string(), 1), None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        match result {
            Some(Value::Int(n)) => assert_eq!(n, 55), // fib(10) = 55
            other => panic!("Expected Int(55), got {:?}", other),
        }
    }
}
