/// Virtual Machine implementation for Rumina - x86_64-style Register-Based
use crate::ast::DeclaredType;
use crate::error::RuminaError;
use crate::value::Value;
use crate::vm_ops::VMOperations;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// Const error messages to avoid allocations
const ERR_STACK_UNDERFLOW: &str = "Stack underflow";
const _ERR_INVALID_CONST_INDEX: &str = "Invalid constant pool index";
const ERR_ARRAY_INDEX_MUST_BE_INT: &str = "Array index must be an integer";
const ERR_STRING_INDEX_MUST_BE_INT: &str = "String index must be an integer";
const _ERR_CANNOT_INDEX_TYPE: &str = "Cannot index type";
const _ERR_CANNOT_CALL_TYPE: &str = "Cannot call type";
const ERR_BREAK_OUTSIDE_LOOP: &str = "Break outside of loop";
const ERR_CONTINUE_OUTSIDE_LOOP: &str = "Continue outside of loop";
const ERR_LAMBDA_ID_NOT_FOUND: &str = "Lambda ID not found";

/// x86_64-style register indices (16 general-purpose registers)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Register {
    RAX = 0,  // Accumulator, return value
    RBX = 1,  // Base register
    RCX = 2,  // Counter register
    RDX = 3,  // Data register
    RSI = 4,  // Source index
    RDI = 5,  // Destination index
    RBP = 6,  // Base pointer (frame pointer)
    RSP = 7,  // Stack pointer
    R8 = 8,   // General purpose
    R9 = 9,   // General purpose
    R10 = 10, // General purpose
    R11 = 11, // General purpose
    R12 = 12, // General purpose
    R13 = 13, // General purpose
    R14 = 14, // General purpose
    R15 = 15, // General purpose
}

impl Register {
    pub fn from_u8(val: u8) -> Option<Self> {
        if val <= 15 {
            Some(unsafe { std::mem::transmute(val) })
        } else {
            None
        }
    }
    
    pub fn as_u8(self) -> u8 {
        self as u8
    }

    pub fn from_index(idx: usize) -> Self {
        match idx {
            0 => Register::RAX,
            1 => Register::RBX,
            2 => Register::RCX,
            3 => Register::RDX,
            4 => Register::RSI,
            5 => Register::RDI,
            6 => Register::RBP,
            7 => Register::RSP,
            8 => Register::R8,
            9 => Register::R9,
            10 => Register::R10,
            11 => Register::R11,
            12 => Register::R12,
            13 => Register::R13,
            14 => Register::R14,
            15 => Register::R15,
            _ => Register::RAX, // Default fallback
        }
    }
}

/// Number of general-purpose registers
const NUM_REGISTERS: usize = 16;

/// Function definition information (boxed in OpCode to reduce size)
#[derive(Debug, Clone, PartialEq)]
pub struct FuncDefInfo {
    pub name: String,
    pub params: Vec<String>,
    pub body_start: usize,
    pub body_end: usize,
    pub decorators: Vec<String>,
}

/// Lambda information (boxed in OpCode to reduce size)
#[derive(Debug, Clone, PartialEq)]
pub struct LambdaInfo {
    pub params: Vec<String>,
    pub body_start: usize,
    pub body_end: usize,
}

/// VM Instruction Set - x86_64-style Register-Based
#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    // ===== Data Movement Instructions (MOV family) =====
    /// MOV reg, imm - Move immediate value to register
    MovImm(Register, Value),

    /// MOV reg, const[idx] - Move constant from pool to register
    MovConst(Register, usize),

    /// MOV reg1, reg2 - Move register to register
    MovReg(Register, Register),

    /// MOV reg, var - Load variable to register
    MovVar(Register, String),

    /// MOV var, reg - Store register to variable
    StoreVar(String, Register),

    /// PUSH reg - Push register value onto stack
    Push(Register),

    /// POP reg - Pop stack value into register
    PopReg(Register),

    // ===== Arithmetic Instructions (x86_64-style) =====
    /// ADD reg1, reg2 - reg1 = reg1 + reg2
    Add(Register, Register),

    /// SUB reg1, reg2 - reg1 = reg1 - reg2
    Sub(Register, Register),

    /// MUL reg1, reg2 - reg1 = reg1 * reg2 (IMUL in x86)
    Mul(Register, Register),

    /// DIV reg1, reg2 - reg1 = reg1 / reg2 (IDIV in x86)
    Div(Register, Register),

    /// MOD reg1, reg2 - reg1 = reg1 % reg2
    ModOp(Register, Register),

    /// POW reg1, reg2 - reg1 = reg1 ^ reg2
    Pow(Register, Register),

    /// NEG reg - reg = -reg
    Neg(Register),

    /// FACT reg - reg = reg! (factorial)
    Factorial(Register),

    // ===== Logical Instructions =====
    /// NOT reg - reg = !reg (logical NOT)
    Not(Register),

    /// AND reg1, reg2 - reg1 = reg1 && reg2
    And(Register, Register),

    /// OR reg1, reg2 - reg1 = reg1 || reg2
    Or(Register, Register),

    // ===== Comparison Instructions (CMP family - sets result in first register) =====
    /// CMP_EQ reg1, reg2 - reg1 = (reg1 == reg2)
    CmpEq(Register, Register),

    /// CMP_NE reg1, reg2 - reg1 = (reg1 != reg2)
    CmpNe(Register, Register),

    /// CMP_GT reg1, reg2 - reg1 = (reg1 > reg2)
    CmpGt(Register, Register),

    /// CMP_GE reg1, reg2 - reg1 = (reg1 >= reg2)
    CmpGe(Register, Register),

    /// CMP_LT reg1, reg2 - reg1 = (reg1 < reg2)
    CmpLt(Register, Register),

    /// CMP_LE reg1, reg2 - reg1 = (reg1 <= reg2)
    CmpLe(Register, Register),

    // ===== Control Flow Instructions (x86_64 jump style) =====
    /// JMP addr - Unconditional jump
    Jmp(usize),

    /// JZ reg, addr - Jump if register is false/zero
    Jz(Register, usize),

    /// JNZ reg, addr - Jump if register is true/non-zero
    Jnz(Register, usize),

    /// CALL var, nargs - Call function by name (args in R8-R15/stack)
    CallVar(String, usize),

    /// CALL reg, nargs - Call function from register (dynamic call)
    CallReg(Register, usize),

    /// CALL_METHOD nargs - Call method (object in RDI, method in RSI)
    CallMethod(usize),

    /// RET - Return from function (return value in RAX)
    Ret,

    // ===== Array/Structure Instructions =====
    /// MAKE_ARRAY reg, count - Create array from stack values, store in reg
    MakeArray(Register, usize),

    /// MAKE_STRUCT reg, count - Create struct from key-value pairs on stack, store in reg
    MakeStruct(Register, usize),

    /// INDEX dst, array_reg, index_reg - dst = array_reg[index_reg]
    Index(Register, Register, Register),

    /// MEMBER dst, obj_reg, member - dst = obj_reg.member
    Member(Register, Register, String),

    /// INDEX_ASSIGN array_reg, index_reg, value_reg - array_reg[index_reg] = value_reg
    IndexAssign(Register, Register, Register),

    /// MEMBER_ASSIGN obj_reg, member, value_reg - obj_reg.member = value_reg
    MemberAssign(Register, String, Register),

    /// MEMBER_ASSIGN_VAR var, member, value_reg - var.member = value_reg (auto-vivify)
    MemberAssignVar(String, String, Register),

    // ===== Function Definition Instructions =====
    /// Define a function (boxed to reduce OpCode size)
    DefineFunc(Box<FuncDefInfo>),

    /// Create lambda/closure (boxed to reduce OpCode size)
    MakeLambda(Box<LambdaInfo>),

    // ===== Control Structures =====
    /// Break from loop
    Break,

    /// Continue loop
    Continue,

    // ===== Special Instructions =====
    /// Halt execution
    Halt,

    // ===== Type Conversion Instructions =====
    /// CONVERT reg, type - Convert register value to specified type
    ConvertType(Register, DeclaredType),
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

    /// Cache for frequently used constants to speed up lookup
    /// Maps common values to their indices
    common_constants_cache: FxHashMap<CommonConstant, usize>,
}

/// Common constant types that can be efficiently cached
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
enum CommonConstant {
    Int(i64),
    Bool(bool),
    Null,
    String(String),
}

impl ByteCode {
    pub fn new() -> Self {
        ByteCode {
            instructions: Vec::new(),
            line_numbers: Vec::new(),
            constants: Vec::new(),
            common_constants_cache: FxHashMap::default(),
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
            OpCode::Jmp(addr) => {
                *addr = target;
            }
            OpCode::Jz(_, addr) => {
                *addr = target;
            }
            OpCode::Jnz(_, addr) => {
                *addr = target;
            }
            _ => panic!("Attempted to patch non-jump instruction at {}", address),
        }
    }

    /// Add a constant to the pool or return existing index
    /// This deduplicates constants to reduce memory usage
    pub fn add_constant(&mut self, value: Value) -> usize {
        // Fast path: Check cache for common constant types (O(1) lookup)
        let cache_key = match &value {
            Value::Int(i) => Some(CommonConstant::Int(*i)),
            Value::Bool(b) => Some(CommonConstant::Bool(*b)),
            Value::Null => Some(CommonConstant::Null),
            Value::String(s) => Some(CommonConstant::String(s.clone())),
            _ => None,
        };

        if let Some(key) = cache_key {
            if let Some(&index) = self.common_constants_cache.get(&key) {
                return index;
            }
        }

        // Slow path: Linear search for complex types or cache miss
        for (i, existing) in self.constants.iter().enumerate() {
            if Self::values_equal(existing, &value) {
                // Update cache if this is a common type
                if let Some(key) = match &value {
                    Value::Int(i) => Some(CommonConstant::Int(*i)),
                    Value::Bool(b) => Some(CommonConstant::Bool(*b)),
                    Value::Null => Some(CommonConstant::Null),
                    Value::String(s) => Some(CommonConstant::String(s.clone())),
                    _ => None,
                } {
                    self.common_constants_cache.insert(key, i);
                }
                return i;
            }
        }

        // Add new constant
        let index = self.constants.len();
        self.constants.push(value.clone());

        // Cache common constants for fast future lookups
        if let Some(key) = match &value {
            Value::Int(i) => Some(CommonConstant::Int(*i)),
            Value::Bool(b) => Some(CommonConstant::Bool(*b)),
            Value::Null => Some(CommonConstant::Null),
            Value::String(s) => Some(CommonConstant::String(s.clone())),
            _ => None,
        } {
            self.common_constants_cache.insert(key, index);
        }

        index
    }

    /// Helper to check if two values are equal for pooling purposes
    fn values_equal(a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => {
                // For floats, use exact bit comparison to avoid floating point issues
                a.to_bits() == b.to_bits()
            }
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Null, Value::Null) => true,
            // For complex types, don't pool them (conservative approach)
            _ => false,
        }
    }

    /// Serialize bytecode to plain text format (.rmc)
    pub fn serialize(&self) -> String {
        let mut output = String::new();

        // Header
        output.push_str("RUMINA-BYTECODE-V1\n");
        output.push_str(&format!("CONSTANTS: {}\n", self.constants.len()));

        // Constants section
        for (i, constant) in self.constants.iter().enumerate() {
            output.push_str(&format!(
                "CONST[{}]: {}\n",
                i,
                Self::serialize_value(constant)
            ));
        }

        output.push_str("\nINSTRUCTIONS:\n");

        // Instructions section
        for (i, (op, line)) in self
            .instructions
            .iter()
            .zip(self.line_numbers.iter())
            .enumerate()
        {
            let line_str = line.map_or("?".to_string(), |l| l.to_string());
            output.push_str(&format!(
                "{:04} [L{}] {}\n",
                i,
                line_str,
                Self::serialize_opcode(op)
            ));
        }

        output
    }

    /// Deserialize bytecode from plain text format (.rmc)
    pub fn deserialize(input: &str) -> Result<Self, String> {
        let mut bytecode = ByteCode::new();
        let lines: Vec<&str> = input.lines().collect();
        let mut i = 0;

        // Check header
        if i >= lines.len() || lines[i] != "RUMINA-BYTECODE-V1" {
            return Err("Invalid bytecode header".to_string());
        }
        i += 1;

        // Parse constants count
        if i >= lines.len() || !lines[i].starts_with("CONSTANTS: ") {
            return Err("Missing constants section".to_string());
        }
        let const_count: usize = lines[i][11..]
            .parse()
            .map_err(|_| "Invalid constants count")?;
        i += 1;

        // Parse constants
        for _ in 0..const_count {
            if i >= lines.len() {
                return Err("Unexpected end of constants section".to_string());
            }
            if let Some(value_str) = lines[i]
                .strip_prefix("CONST[")
                .and_then(|s| s.split_once("]: "))
                .map(|(_, v)| v)
            {
                bytecode.constants.push(Self::deserialize_value(value_str)?);
            } else {
                return Err("Invalid constant format".to_string());
            }
            i += 1;
        }

        // Skip empty line and instructions header
        while i < lines.len() && (lines[i].is_empty() || lines[i] == "INSTRUCTIONS:") {
            i += 1;
        }

        // Parse instructions
        while i < lines.len() {
            let line = lines[i].trim();
            if line.is_empty() {
                i += 1;
                continue;
            }

            // Parse: "0000 [L1] OpCode ..."
            let parts: Vec<&str> = line.splitn(3, ' ').collect();
            if parts.len() < 3 {
                return Err(format!("Invalid instruction format: {}", line));
            }

            let line_num = if parts[1].len() > 3 {
                let num_str = &parts[1][2..parts[1].len() - 1]; // Extract number from [L...]
                if num_str == "?" {
                    None
                } else {
                    Some(num_str.parse().map_err(|_| "Invalid line number")?)
                }
            } else {
                None
            };

            let opcode = Self::deserialize_opcode(parts[2])?;
            bytecode.instructions.push(opcode);
            bytecode.line_numbers.push(line_num);

            i += 1;
        }

        Ok(bytecode)
    }

    fn serialize_value(value: &Value) -> String {
        match value {
            Value::Int(n) => format!("Int({})", n),
            Value::Float(f) => format!("Float({})", f),
            Value::Bool(b) => format!("Bool({})", b),
            Value::String(s) => {
                // Properly escape special characters
                let escaped = s
                    .replace('\\', "\\\\")
                    .replace('"', "\\\"")
                    .replace('\n', "\\n")
                    .replace('\r', "\\r")
                    .replace('\t', "\\t");
                format!("String(\"{}\")", escaped)
            }
            Value::Null => "Null".to_string(),
            Value::Array(arr) => {
                let items: Vec<String> = arr
                    .borrow()
                    .iter()
                    .map(|v| Self::serialize_value(v))
                    .collect();
                format!("Array[{}]", items.join(", "))
            }
            Value::Struct(s) => {
                let items: Vec<String> = s
                    .borrow()
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, Self::serialize_value(v)))
                    .collect();
                format!("Struct{{{}}}", items.join(", "))
            }
            Value::NativeFunction { name, .. } => format!("NativeFunction({})", name),
            Value::Function { name, .. } => format!("Function({})", name),
            Value::Lambda { .. } => "Lambda".to_string(),
            _ => format!("{:?}", value),
        }
    }

    fn deserialize_value(s: &str) -> Result<Value, String> {
        if let Some(num) = s.strip_prefix("Int(").and_then(|s| s.strip_suffix(")")) {
            return Ok(Value::Int(num.parse().map_err(|_| "Invalid int")?));
        }
        if let Some(num) = s.strip_prefix("Float(").and_then(|s| s.strip_suffix(")")) {
            return Ok(Value::Float(num.parse().map_err(|_| "Invalid float")?));
        }
        if let Some(b) = s.strip_prefix("Bool(").and_then(|s| s.strip_suffix(")")) {
            return Ok(Value::Bool(b.parse().map_err(|_| "Invalid bool")?));
        }
        if let Some(str_val) = s
            .strip_prefix("String(\"")
            .and_then(|s| s.strip_suffix("\")"))
        {
            // Properly unescape special characters
            let mut unescaped = String::new();
            let mut chars = str_val.chars();
            while let Some(ch) = chars.next() {
                if ch == '\\' {
                    if let Some(next_ch) = chars.next() {
                        match next_ch {
                            'n' => unescaped.push('\n'),
                            'r' => unescaped.push('\r'),
                            't' => unescaped.push('\t'),
                            '\\' => unescaped.push('\\'),
                            '"' => unescaped.push('"'),
                            _ => {
                                // Unknown escape sequence, keep as is
                                unescaped.push('\\');
                                unescaped.push(next_ch);
                            }
                        }
                    } else {
                        unescaped.push('\\');
                    }
                } else {
                    unescaped.push(ch);
                }
            }
            return Ok(Value::String(unescaped));
        }
        if s == "Null" {
            return Ok(Value::Null);
        }
        Err(format!("Unsupported value type: {}", s))
    }

    fn serialize_opcode(op: &OpCode) -> String {
        // TODO: Update for register-based bytecode
        format!("{:?}", op)
    }

    fn deserialize_opcode(_s: &str) -> Result<OpCode, String> {
        // TODO: Update for register-based bytecode
        Err("Deserialization not yet implemented for register-based bytecode".to_string())
    }
}

/// Function metadata for user-defined functions
#[derive(Debug, Clone)]
struct FunctionInfo {
    /// Function name
    #[allow(dead_code)]
    name: String,
    /// Parameter names
    params: Vec<String>,
    /// Start address in bytecode
    body_start: usize,
    /// End address in bytecode
    #[allow(dead_code)]
    body_end: usize,
}

/// Call frame for function calls
#[derive(Debug, Clone)]
struct CallFrame {
    /// Return address (instruction pointer to return to)
    return_address: usize,

    /// Base pointer for local variables
    #[allow(dead_code)]
    base_pointer: usize,

    /// Function name (for error reporting)
    #[allow(dead_code)]
    function_name: String,

    /// Local variables in this frame (FxHashMap for faster access)
    locals: FxHashMap<String, Value>,
}

/// Inline cache entry for member access
#[derive(Debug, Clone)]
struct InlineCache {
    /// Member name being accessed
    #[allow(dead_code)]
    member: String,
    /// Cached result for fast path (if object structure matches)
    /// Currently unused but reserved for future optimization
    #[allow(dead_code)]
    cached_value: Option<Value>,
    /// Cache hits counter
    hits: usize,
    /// Cache misses counter
    misses: usize,
}

impl InlineCache {
    fn new(member: String) -> Self {
        InlineCache {
            member,
            cached_value: None,
            hits: 0,
            misses: 0,
        }
    }
}

/// Virtual Machine state - x86_64-style Register-Based
pub struct VM {
    /// Bytecode being executed
    bytecode: ByteCode,

    /// Instruction pointer (Program Counter)
    ip: usize,

    /// General-purpose registers (16 registers like x86_64)
    registers: [Value; NUM_REGISTERS],

    /// Data stack (still needed for function calls and complex operations)
    stack: Vec<Value>,

    /// Call stack (for function calls)
    call_stack: Vec<CallFrame>,

    /// Global variables
    globals: Rc<RefCell<HashMap<String, Value>>>,

    /// Current local variables (FxHashMap for faster hashing)
    locals: FxHashMap<String, Value>,

    /// Loop break/continue targets
    loop_stack: Vec<(usize, usize)>, // (continue_target, break_target)

    /// Function table: maps function names to their bytecode locations (FxHashMap for speed)
    functions: FxHashMap<String, FunctionInfo>,

    /// Inline cache for member access (maps instruction address to cache)
    member_cache: FxHashMap<usize, InlineCache>,

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
            // Initialize all registers to Null
            registers: [
                Value::Null, Value::Null, Value::Null, Value::Null,
                Value::Null, Value::Null, Value::Null, Value::Null,
                Value::Null, Value::Null, Value::Null, Value::Null,
                Value::Null, Value::Null, Value::Null, Value::Null,
            ],
            // Keep stack for function calls and complex operations
            stack: Vec::with_capacity(256),
            call_stack: Vec::with_capacity(64),
            globals,
            // Use FxHashMap for faster string hashing
            locals: FxHashMap::default(),
            loop_stack: Vec::with_capacity(8), // Pre-allocate for nested loops
            functions: FxHashMap::default(),
            member_cache: FxHashMap::default(),
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
        // Reset registers
        for reg in &mut self.registers {
            *reg = Value::Null;
        }
    }

    /// Get value from register
    #[inline]
    fn get_reg(&self, reg: Register) -> &Value {
        &self.registers[reg.as_u8() as usize]
    }

    /// Set value in register
    #[inline]
    fn set_reg(&mut self, reg: Register, value: Value) {
        self.registers[reg.as_u8() as usize] = value;
    }

    /// Clone value from register (for operations that need ownership)
    #[inline]
    fn clone_reg(&self, reg: Register) -> Value {
        self.registers[reg.as_u8() as usize].clone()
    }

    /// Execute loaded bytecode
    pub fn run(&mut self) -> Result<Option<Value>, RuminaError> {
        while !self.halted && self.ip < self.bytecode.instructions.len() {
            // Get current instruction index
            let current_ip = self.ip;
            self.ip += 1;

            // Execute by matching on the instruction at current index
            self.execute_instruction_at(current_ip)?;
        }

        // Return value in RAX (accumulator/return register), or None if Null
        let return_value = self.clone_reg(Register::RAX);
        Ok(if return_value == Value::Null {
            None
        } else {
            Some(return_value)
        })
    }

    /// Execute a single instruction at the given index (safe, no cloning)
    fn execute_instruction_at(&mut self, ip: usize) -> Result<(), RuminaError> {
        // Pattern match directly on the instruction reference
        // The borrow checker allows this because we only need immutable access for matching
        match &self.bytecode.instructions[ip] {
            // ===== Data Movement Instructions =====
            OpCode::MovImm(reg, value) => {
                self.set_reg(*reg, value.clone());
            }

            OpCode::MovConst(reg, index) => {
                let value = self
                    .bytecode
                    .constants
                    .get(*index)
                    .ok_or_else(|| {
                        RuminaError::runtime(format!("Invalid constant pool index: {}", index))
                    })?
                    .clone();
                self.set_reg(*reg, value);
            }

            OpCode::MovReg(dst, src) => {
                let value = self.clone_reg(*src);
                self.set_reg(*dst, value);
            }

            OpCode::MovVar(reg, name) => {
                let value = self.get_variable(name)?;
                self.set_reg(*reg, value);
            }

            OpCode::StoreVar(name, reg) => {
                let value = self.clone_reg(*reg);
                self.set_variable(name.clone(), value);
            }

            OpCode::Push(reg) => {
                let value = self.clone_reg(*reg);
                self.stack.push(value);
            }

            OpCode::PopReg(reg) => {
                let value = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime(ERR_STACK_UNDERFLOW))?;
                self.set_reg(*reg, value);
            }

            // ===== Arithmetic Instructions =====
            OpCode::Add(dst, src) => {
                let a = self.clone_reg(*dst);
                let b = self.clone_reg(*src);
                let result = a.vm_add(&b).map_err(|e| RuminaError::runtime(e))?;
                self.set_reg(*dst, result);
            }

            OpCode::Sub(dst, src) => {
                let a = self.clone_reg(*dst);
                let b = self.clone_reg(*src);
                let result = a.vm_sub(&b).map_err(|e| RuminaError::runtime(e))?;
                self.set_reg(*dst, result);
            }

            OpCode::Mul(dst, src) => {
                let a = self.clone_reg(*dst);
                let b = self.clone_reg(*src);
                let result = a.vm_mul(&b).map_err(|e| RuminaError::runtime(e))?;
                self.set_reg(*dst, result);
            }

            OpCode::Div(dst, src) => {
                let a = self.clone_reg(*dst);
                let b = self.clone_reg(*src);
                let result = a.vm_div(&b).map_err(|e| RuminaError::runtime(e))?;
                self.set_reg(*dst, result);
            }

            OpCode::ModOp(dst, src) => {
                let a = self.clone_reg(*dst);
                let b = self.clone_reg(*src);
                let result = a.vm_mod(&b).map_err(|e| RuminaError::runtime(e))?;
                self.set_reg(*dst, result);
            }

            OpCode::Pow(dst, src) => {
                let a = self.clone_reg(*dst);
                let b = self.clone_reg(*src);
                let result = a.vm_pow(&b).map_err(|e| RuminaError::runtime(e))?;
                self.set_reg(*dst, result);
            }

            OpCode::Neg(reg) => {
                let value = self.clone_reg(*reg);
                let result = value.vm_neg().map_err(|e| RuminaError::runtime(e))?;
                self.set_reg(*reg, result);
            }

            OpCode::Factorial(reg) => {
                let value = self.clone_reg(*reg);
                let result = value.vm_factorial().map_err(|e| RuminaError::runtime(e))?;
                self.set_reg(*reg, result);
            }

            // ===== Logical Instructions =====
            OpCode::Not(reg) => {
                let value = self.clone_reg(*reg);
                let result = value.vm_not().map_err(|e| RuminaError::runtime(e))?;
                self.set_reg(*reg, result);
            }

            OpCode::And(dst, src) => {
                let a = self.clone_reg(*dst);
                let b = self.clone_reg(*src);
                let result = a.vm_and(&b).map_err(|e| RuminaError::runtime(e))?;
                self.set_reg(*dst, result);
            }

            OpCode::Or(dst, src) => {
                let a = self.clone_reg(*dst);
                let b = self.clone_reg(*src);
                let result = a.vm_or(&b).map_err(|e| RuminaError::runtime(e))?;
                self.set_reg(*dst, result);
            }

            // ===== Comparison Instructions =====
            OpCode::CmpEq(dst, src) => {
                let a = self.clone_reg(*dst);
                let b = self.clone_reg(*src);
                let result = a.vm_eq(&b).map_err(|e| RuminaError::runtime(e))?;
                self.set_reg(*dst, result);
            }

            OpCode::CmpNe(dst, src) => {
                let a = self.clone_reg(*dst);
                let b = self.clone_reg(*src);
                let result = a.vm_neq(&b).map_err(|e| RuminaError::runtime(e))?;
                self.set_reg(*dst, result);
            }

            OpCode::CmpGt(dst, src) => {
                let a = self.clone_reg(*dst);
                let b = self.clone_reg(*src);
                let result = a.vm_gt(&b).map_err(|e| RuminaError::runtime(e))?;
                self.set_reg(*dst, result);
            }

            OpCode::CmpGe(dst, src) => {
                let a = self.clone_reg(*dst);
                let b = self.clone_reg(*src);
                let result = a.vm_gte(&b).map_err(|e| RuminaError::runtime(e))?;
                self.set_reg(*dst, result);
            }

            OpCode::CmpLt(dst, src) => {
                let a = self.clone_reg(*dst);
                let b = self.clone_reg(*src);
                let result = a.vm_lt(&b).map_err(|e| RuminaError::runtime(e))?;
                self.set_reg(*dst, result);
            }

            OpCode::CmpLe(dst, src) => {
                let a = self.clone_reg(*dst);
                let b = self.clone_reg(*src);
                let result = a.vm_lte(&b).map_err(|e| RuminaError::runtime(e))?;
                self.set_reg(*dst, result);
            }

            // ===== Control Flow Instructions =====
            OpCode::Jmp(addr) => {
                self.ip = *addr;
            }

            OpCode::Jz(reg, addr) => {
                let condition = self.get_reg(*reg);
                if !condition.is_truthy() {
                    self.ip = *addr;
                }
            }

            OpCode::Jnz(reg, addr) => {
                let condition = self.get_reg(*reg);
                if condition.is_truthy() {
                    self.ip = *addr;
                }
            }

            // ===== Function Call Instructions =====
            OpCode::CallVar(func_name, arg_count) => {
                let func = self.get_variable(func_name)?;

                // Collect arguments from registers R8-R15, then stack overflow
                let mut args = Vec::with_capacity(*arg_count);
                let reg_args = (*arg_count).min(8);
                for i in 0..reg_args {
                    args.push(self.clone_reg(Register::from_index(8 + i)));
                }
                // If more than 8 args, pop the rest from stack
                for _ in reg_args..*arg_count {
                    let arg = self
                        .stack
                        .pop()
                        .ok_or_else(|| RuminaError::runtime(ERR_STACK_UNDERFLOW))?;
                    args.push(arg);
                }

                match func {
                    Value::NativeFunction {
                        func: native_fn, ..
                    } => {
                        let result = native_fn(&args).map_err(|e| RuminaError::runtime(e))?;
                        self.set_reg(Register::RAX, result);
                    }
                    Value::Function { name, .. } => {
                        if let Some(func_info) = self.functions.get(name.as_str()) {
                            if self.recursion_depth >= self.max_recursion_depth {
                                return Err(RuminaError::runtime(format!(
                                    "Maximum recursion depth ({}) exceeded",
                                    self.max_recursion_depth
                                )));
                            }

                            if args.len() != func_info.params.len() {
                                return Err(RuminaError::runtime(format!(
                                    "Function '{}' expects {} arguments, got {}",
                                    func_name,
                                    func_info.params.len(),
                                    args.len()
                                )));
                            }

                            let body_start = func_info.body_start;
                            let params = func_info.params.clone();

                            let frame = CallFrame {
                                return_address: self.ip,
                                base_pointer: self.stack.len(),
                                function_name: func_name.clone(),
                                locals: std::mem::take(&mut self.locals),
                            };

                            self.call_stack.push(frame);
                            self.recursion_depth += 1;

                            let mut new_locals = FxHashMap::with_capacity_and_hasher(
                                params.len(),
                                Default::default(),
                            );
                            for (param_name, arg_value) in params.iter().zip(args.into_iter()) {
                                new_locals.insert(param_name.clone(), arg_value);
                            }
                            self.locals = new_locals;

                            self.ip = body_start;
                        } else {
                            return Err(RuminaError::runtime(format!(
                                "Function '{}' not found in function table",
                                func_name
                            )));
                        }
                    }
                    Value::Lambda {
                        params,
                        body,
                        closure,
                        ..
                    } => {
                        if self.recursion_depth >= self.max_recursion_depth {
                            return Err(RuminaError::runtime(format!(
                                "Maximum recursion depth ({}) exceeded",
                                self.max_recursion_depth
                            )));
                        }

                        if args.len() != params.len() {
                            return Err(RuminaError::runtime(format!(
                                "Lambda expects {} arguments, got {}",
                                params.len(),
                                args.len()
                            )));
                        }

                        let lambda_id = match body.as_ref() {
                            crate::ast::Stmt::Include(id) => id.clone(),
                            _ => {
                                let mut found_id = None;
                                for (name, _) in &self.functions {
                                    if name.starts_with("__lambda_") {
                                        found_id = Some(name.clone());
                                        break;
                                    }
                                }
                                found_id
                                    .ok_or_else(|| RuminaError::runtime(ERR_LAMBDA_ID_NOT_FOUND))?
                            }
                        };

                        let func_info = self
                            .functions
                            .get(&lambda_id)
                            .ok_or_else(|| {
                                RuminaError::runtime(format!("Lambda '{}' not found", lambda_id))
                            })?
                            .clone();

                        let frame = CallFrame {
                            return_address: self.ip,
                            base_pointer: self.stack.len(),
                            function_name: lambda_id.clone(),
                            locals: std::mem::take(&mut self.locals),
                        };

                        self.call_stack.push(frame);
                        self.recursion_depth += 1;

                        let closure_ref = closure.borrow();
                        let total_capacity = closure_ref.len() + params.len();
                        let mut new_locals =
                            FxHashMap::with_capacity_and_hasher(total_capacity, Default::default());
                        for (k, v) in closure_ref.iter() {
                            new_locals.insert(k.clone(), v.clone());
                        }
                        drop(closure_ref);
                        for (param_name, arg_value) in params.iter().zip(args.into_iter()) {
                            new_locals.insert(param_name.clone(), arg_value);
                        }
                        self.locals = new_locals;

                        self.ip = func_info.body_start;
                    }
                    _ => {
                        return Err(RuminaError::runtime(format!(
                            "Cannot call type {}",
                            func.type_name()
                        )));
                    }
                }
            }

            OpCode::CallReg(reg, arg_count) => {
                let func = self.clone_reg(*reg);

                // Collect arguments from registers R8-R15, then stack overflow
                let mut args = Vec::with_capacity(*arg_count);
                let reg_args = (*arg_count).min(8);
                for i in 0..reg_args {
                    args.push(self.clone_reg(Register::from_index(8 + i)));
                }
                for _ in reg_args..*arg_count {
                    let arg = self
                        .stack
                        .pop()
                        .ok_or_else(|| RuminaError::runtime(ERR_STACK_UNDERFLOW))?;
                    args.push(arg);
                }

                match func {
                    Value::NativeFunction {
                        func: native_fn, ..
                    } => {
                        let result = native_fn(&args).map_err(|e| RuminaError::runtime(e))?;
                        self.set_reg(Register::RAX, result);
                    }
                    Value::Function { name, .. } => {
                        if let Some(func_info) = self.functions.get(&name) {
                            if self.recursion_depth >= self.max_recursion_depth {
                                return Err(RuminaError::runtime(format!(
                                    "Maximum recursion depth ({}) exceeded",
                                    self.max_recursion_depth
                                )));
                            }

                            if args.len() != func_info.params.len() {
                                return Err(RuminaError::runtime(format!(
                                    "Function '{}' expects {} arguments, got {}",
                                    name,
                                    func_info.params.len(),
                                    args.len()
                                )));
                            }

                            let body_start = func_info.body_start;
                            let params = func_info.params.clone();

                            let frame = CallFrame {
                                return_address: self.ip,
                                base_pointer: self.stack.len(),
                                function_name: name.clone(),
                                locals: std::mem::take(&mut self.locals),
                            };

                            self.call_stack.push(frame);
                            self.recursion_depth += 1;

                            let mut new_locals = FxHashMap::with_capacity_and_hasher(
                                params.len(),
                                Default::default(),
                            );
                            for (param_name, arg_value) in params.iter().zip(args.into_iter()) {
                                new_locals.insert(param_name.clone(), arg_value);
                            }
                            self.locals = new_locals;

                            self.ip = body_start;
                        } else {
                            return Err(RuminaError::runtime(format!(
                                "Function '{}' not found in function table",
                                name
                            )));
                        }
                    }
                    Value::Lambda {
                        params,
                        body,
                        closure,
                        ..
                    } => {
                        if self.recursion_depth >= self.max_recursion_depth {
                            return Err(RuminaError::runtime(format!(
                                "Maximum recursion depth ({}) exceeded",
                                self.max_recursion_depth
                            )));
                        }

                        if args.len() != params.len() {
                            return Err(RuminaError::runtime(format!(
                                "Lambda expects {} arguments, got {}",
                                params.len(),
                                args.len()
                            )));
                        }

                        let lambda_id = match body.as_ref() {
                            crate::ast::Stmt::Include(id) => id.clone(),
                            _ => {
                                let mut found_id = None;
                                for (name, _) in &self.functions {
                                    if name.starts_with("__lambda_") {
                                        found_id = Some(name.clone());
                                        break;
                                    }
                                }
                                found_id
                                    .ok_or_else(|| RuminaError::runtime(ERR_LAMBDA_ID_NOT_FOUND))?
                            }
                        };

                        let func_info = self
                            .functions
                            .get(&lambda_id)
                            .ok_or_else(|| {
                                RuminaError::runtime(format!("Lambda '{}' not found", lambda_id))
                            })?
                            .clone();

                        let frame = CallFrame {
                            return_address: self.ip,
                            base_pointer: self.stack.len(),
                            function_name: lambda_id.clone(),
                            locals: std::mem::take(&mut self.locals),
                        };

                        self.call_stack.push(frame);
                        self.recursion_depth += 1;

                        let closure_ref = closure.borrow();
                        let total_capacity = closure_ref.len() + params.len();
                        let mut new_locals =
                            FxHashMap::with_capacity_and_hasher(total_capacity, Default::default());
                        for (k, v) in closure_ref.iter() {
                            new_locals.insert(k.clone(), v.clone());
                        }
                        drop(closure_ref);
                        for (param_name, arg_value) in params.iter().zip(args.into_iter()) {
                            new_locals.insert(param_name.clone(), arg_value);
                        }
                        self.locals = new_locals;

                        self.ip = func_info.body_start;
                    }
                    _ => {
                        return Err(RuminaError::runtime(format!(
                            "Cannot call type {}",
                            func.type_name()
                        )));
                    }
                }
            }

            OpCode::CallMethod(arg_count) => {
                let object = self.clone_reg(Register::RDI);
                let method = self.clone_reg(Register::RSI);

                // Collect arguments from registers R8-R15, then stack overflow
                let mut args = Vec::with_capacity(*arg_count);
                let reg_args = (*arg_count).min(8);
                for i in 0..reg_args {
                    args.push(self.clone_reg(Register::from_index(8 + i)));
                }
                for _ in reg_args..*arg_count {
                    let arg = self
                        .stack
                        .pop()
                        .ok_or_else(|| RuminaError::runtime(ERR_STACK_UNDERFLOW))?;
                    args.push(arg);
                }

                match method {
                    Value::Lambda {
                        params,
                        body,
                        closure,
                        ..
                    } => {
                        if self.recursion_depth >= self.max_recursion_depth {
                            return Err(RuminaError::runtime(format!(
                                "Maximum recursion depth ({}) exceeded",
                                self.max_recursion_depth
                            )));
                        }

                        if args.len() != params.len() {
                            return Err(RuminaError::runtime(format!(
                                "Method expects {} arguments, got {}",
                                params.len(),
                                args.len()
                            )));
                        }

                        let lambda_id = match body.as_ref() {
                            crate::ast::Stmt::Include(id) => id.clone(),
                            _ => {
                                let mut found_id = None;
                                for (name, _) in &self.functions {
                                    if name.starts_with("__lambda_") {
                                        found_id = Some(name.clone());
                                        break;
                                    }
                                }
                                found_id
                                    .ok_or_else(|| RuminaError::runtime(ERR_LAMBDA_ID_NOT_FOUND))?
                            }
                        };

                        let func_info = self
                            .functions
                            .get(&lambda_id)
                            .ok_or_else(|| {
                                RuminaError::runtime(format!("Lambda '{}' not found", lambda_id))
                            })?
                            .clone();

                        let frame = CallFrame {
                            return_address: self.ip,
                            base_pointer: self.stack.len(),
                            function_name: lambda_id.clone(),
                            locals: std::mem::take(&mut self.locals),
                        };

                        self.call_stack.push(frame);
                        self.recursion_depth += 1;

                        self.locals = closure
                            .borrow()
                            .iter()
                            .map(|(k, v)| (k.clone(), v.clone()))
                            .collect();
                        self.locals.insert("self".to_string(), object);
                        for (param_name, arg_value) in params.iter().zip(args.into_iter()) {
                            self.locals.insert(param_name.clone(), arg_value);
                        }

                        self.ip = func_info.body_start;
                    }
                    Value::Function { name, .. } => {
                        if let Some(func_info) = self.functions.get(&name) {
                            if self.recursion_depth >= self.max_recursion_depth {
                                return Err(RuminaError::runtime(format!(
                                    "Maximum recursion depth ({}) exceeded",
                                    self.max_recursion_depth
                                )));
                            }

                            if args.len() != func_info.params.len() {
                                return Err(RuminaError::runtime(format!(
                                    "Function '{}' expects {} arguments, got {}",
                                    name,
                                    func_info.params.len(),
                                    args.len()
                                )));
                            }

                            let body_start = func_info.body_start;
                            let params = func_info.params.clone();

                            let frame = CallFrame {
                                return_address: self.ip,
                                base_pointer: self.stack.len(),
                                function_name: name.clone(),
                                locals: std::mem::take(&mut self.locals),
                            };

                            self.call_stack.push(frame);
                            self.recursion_depth += 1;

                            self.locals.clear();
                            self.locals.reserve(params.len() + 1);
                            self.locals.insert("self".to_string(), object);
                            for (param_name, arg_value) in params.iter().zip(args.into_iter()) {
                                self.locals.insert(param_name.clone(), arg_value);
                            }

                            self.ip = body_start;
                        } else {
                            return Err(RuminaError::runtime(format!(
                                "Function '{}' not found in function table",
                                name
                            )));
                        }
                    }
                    _ => {
                        return Err(RuminaError::runtime(format!(
                            "Cannot call method of type {}",
                            method.type_name()
                        )));
                    }
                }
            }

            OpCode::Ret => {
                if let Some(frame) = self.call_stack.pop() {
                    self.recursion_depth = self.recursion_depth.saturating_sub(1);
                    self.ip = frame.return_address;
                    self.locals = frame.locals;
                } else {
                    self.halted = true;
                }
            }

            // ===== Array/Structure Instructions =====
            OpCode::MakeArray(reg, count) => {
                let mut elements = Vec::new();
                for _ in 0..*count {
                    let elem = self
                        .stack
                        .pop()
                        .ok_or_else(|| RuminaError::runtime(ERR_STACK_UNDERFLOW))?;
                    elements.push(elem);
                }
                elements.reverse();
                self.set_reg(*reg, Value::Array(Rc::new(RefCell::new(elements))));
            }

            OpCode::MakeStruct(reg, count) => {
                let mut fields = HashMap::new();
                for _ in 0..*count {
                    let value = self
                        .stack
                        .pop()
                        .ok_or_else(|| RuminaError::runtime(ERR_STACK_UNDERFLOW))?;
                    let key = self
                        .stack
                        .pop()
                        .ok_or_else(|| RuminaError::runtime(ERR_STACK_UNDERFLOW))?;

                    if let Value::String(key_str) = key {
                        fields.insert(key_str, value);
                    } else {
                        return Err(RuminaError::runtime(
                            "Struct key must be a string".to_string(),
                        ));
                    }
                }
                self.set_reg(*reg, Value::Struct(Rc::new(RefCell::new(fields))));
            }

            OpCode::Index(dst, array_reg, index_reg) => {
                let array = self.clone_reg(*array_reg);
                let index = self.clone_reg(*index_reg);

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
                                self.set_reg(*dst, arr_ref[idx].clone());
                            } else {
                                return Err(RuminaError::runtime(format!(
                                    "Array index out of bounds: {} (length: {})",
                                    idx,
                                    arr_ref.len()
                                )));
                            }
                        } else {
                            return Err(RuminaError::runtime(ERR_ARRAY_INDEX_MUST_BE_INT));
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
                                self.set_reg(*dst, Value::String(ch.to_string()));
                            } else {
                                return Err(RuminaError::runtime(format!(
                                    "String index out of bounds: {} (length: {})",
                                    idx,
                                    s.len()
                                )));
                            }
                        } else {
                            return Err(RuminaError::runtime(ERR_STRING_INDEX_MUST_BE_INT));
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

            OpCode::Member(dst, obj_reg, member_name) => {
                let cache_addr = self.ip - 1;
                let object = self.clone_reg(*obj_reg);

                match &object {
                    Value::Struct(s) | Value::Module(s) => {
                        let s_ref = s.borrow();
                        if let Some(value) = s_ref.get(member_name) {
                            if let Some(cache) = self.member_cache.get_mut(&cache_addr) {
                                cache.hits += 1;
                            } else {
                                self.member_cache
                                    .insert(cache_addr, InlineCache::new(member_name.clone()));
                            }

                            self.set_reg(*dst, value.clone());
                        } else {
                            if let Some(cache) = self.member_cache.get_mut(&cache_addr) {
                                cache.misses += 1;
                            } else {
                                let mut cache = InlineCache::new(member_name.clone());
                                cache.misses = 1;
                                self.member_cache.insert(cache_addr, cache);
                            }

                            return Err(RuminaError::runtime(format!(
                                "{} does not have member '{}'",
                                object.type_name(),
                                member_name
                            )));
                        }
                    }
                    _ => {
                        if let Some(cache) = self.member_cache.get_mut(&cache_addr) {
                            cache.misses += 1;
                        } else {
                            let mut cache = InlineCache::new(member_name.clone());
                            cache.misses = 1;
                            self.member_cache.insert(cache_addr, cache);
                        }

                        return Err(RuminaError::runtime(format!(
                            "Cannot access member of type {}",
                            object.type_name()
                        )));
                    }
                }
            }

            OpCode::IndexAssign(array_reg, index_reg, value_reg) => {
                let array = self.clone_reg(*array_reg);
                let index = self.clone_reg(*index_reg);
                let value = self.clone_reg(*value_reg);

                match array {
                    Value::Array(arr) => {
                        if let Value::Int(idx) = index {
                            let mut arr_ref = arr.borrow_mut();
                            let idx = if idx < 0 {
                                (arr_ref.len() as i64 + idx) as usize
                            } else {
                                idx as usize
                            };

                            if idx < arr_ref.len() {
                                arr_ref[idx] = value;
                            } else {
                                return Err(RuminaError::runtime(format!(
                                    "Array index out of bounds: {} (length: {})",
                                    idx,
                                    arr_ref.len()
                                )));
                            }
                        } else {
                            return Err(RuminaError::runtime(ERR_ARRAY_INDEX_MUST_BE_INT));
                        }
                    }
                    _ => {
                        return Err(RuminaError::runtime(format!(
                            "Cannot index assign to type {}",
                            array.type_name()
                        )));
                    }
                }
            }

            OpCode::MemberAssign(obj_reg, member_name, value_reg) => {
                let object = self.clone_reg(*obj_reg);
                let value = self.clone_reg(*value_reg);

                match object {
                    Value::Struct(s) | Value::Module(s) => {
                        s.borrow_mut().insert(member_name.clone(), value);
                    }
                    _ => {
                        return Err(RuminaError::runtime(format!(
                            "Cannot assign member to {}",
                            object.type_name()
                        )));
                    }
                }
            }

            OpCode::MemberAssignVar(var_name, member_name, value_reg) => {
                let value = self.clone_reg(*value_reg);
                let object = self.get_variable(var_name)?;

                match object {
                    Value::Struct(s) | Value::Module(s) => {
                        s.borrow_mut().insert(member_name.clone(), value);
                    }
                    Value::Null => {
                        let new_struct = Rc::new(RefCell::new(HashMap::default()));
                        new_struct.borrow_mut().insert(member_name.clone(), value);
                        self.set_variable(var_name.clone(), Value::Struct(new_struct));
                    }
                    _ => {
                        return Err(RuminaError::runtime(format!(
                            "Cannot assign member to {}",
                            object.type_name()
                        )));
                    }
                }
            }

            // ===== Function Definition Instructions =====
            OpCode::DefineFunc(info) => {
                self.functions.insert(
                    info.name.clone(),
                    FunctionInfo {
                        name: info.name.clone(),
                        params: info.params.clone(),
                        body_start: info.body_start,
                        body_end: info.body_end,
                    },
                );

                let func_value = Value::Function {
                    name: info.name.clone(),
                    params: info.params.clone(),
                    body: Box::new(crate::ast::Stmt::Block(vec![])),
                    decorators: info.decorators.clone(),
                };
                self.globals
                    .borrow_mut()
                    .insert(info.name.clone(), func_value);
            }

            OpCode::MakeLambda(info) => {
                let lambda_id_value = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime(ERR_STACK_UNDERFLOW))?;

                let lambda_id = match lambda_id_value {
                    Value::String(id) => id,
                    _ => {
                        return Err(RuminaError::runtime(
                            "Expected lambda ID as string".to_string(),
                        ));
                    }
                };

                let closure = if !self.locals.is_empty() {
                    let locals_hashmap: HashMap<String, Value> = self
                        .locals
                        .iter()
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .collect();
                    Rc::new(RefCell::new(locals_hashmap))
                } else {
                    Rc::clone(&self.globals)
                };

                let marker_body = Box::new(crate::ast::Stmt::Include(lambda_id.clone()));

                let lambda_value = Value::Lambda {
                    params: info.params.clone(),
                    body: marker_body,
                    closure,
                };

                // Store lambda in RAX (register-based convention)
                self.set_reg(Register::RAX, lambda_value);
            }

            // ===== Control Structures =====
            OpCode::Break => {
                if let Some((_, break_target)) = self.loop_stack.last() {
                    self.ip = *break_target;
                } else {
                    return Err(RuminaError::runtime(ERR_BREAK_OUTSIDE_LOOP));
                }
            }

            OpCode::Continue => {
                if let Some((continue_target, _)) = self.loop_stack.last() {
                    self.ip = *continue_target;
                } else {
                    return Err(RuminaError::runtime(ERR_CONTINUE_OUTSIDE_LOOP));
                }
            }

            // ===== Special Instructions =====
            OpCode::Halt => {
                self.halted = true;
            }

            // ===== Type Conversion Instructions =====
            OpCode::ConvertType(reg, dtype) => {
                let val = self.clone_reg(*reg);
                let converted = self.convert_to_type(val, &dtype)?;
                self.set_reg(*reg, converted);
            }
        }

        Ok(())
    }


    /// Convert value to specified type
    fn convert_to_type(&self, val: Value, dtype: &DeclaredType) -> Result<Value, RuminaError> {
        use crate::interpreter::convert;
        convert::convert_to_declared_type(val, dtype).map_err(|e| RuminaError::runtime(e))
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

    /// Get inline cache statistics for debugging/profiling
    #[allow(dead_code)]
    pub fn get_cache_stats(&self) -> (usize, usize) {
        let total_hits: usize = self.member_cache.values().map(|c| c.hits).sum();
        let total_misses: usize = self.member_cache.values().map(|c| c.misses).sum();
        (total_hits, total_misses)
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
        bytecode.emit(OpCode::MovImm(Register::R8, Value::Int(42)), None);
        bytecode.emit(OpCode::MovImm(Register::RAX, Value::Int(10)), None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        assert_eq!(result, Some(Value::Int(10)));
    }

    #[test]
    fn test_bytecode_emit() {
        let mut bytecode = ByteCode::new();
        bytecode.emit(OpCode::MovImm(Register::R8, Value::Int(1)), Some(1));
        bytecode.emit(OpCode::Add(Register::R8, Register::R9), Some(1));

        assert_eq!(bytecode.instructions.len(), 2);
        assert_eq!(bytecode.line_numbers.len(), 2);
    }

    #[test]
    fn test_vm_arithmetic() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();
        bytecode.emit(OpCode::MovImm(Register::R8, Value::Int(10)), None);
        bytecode.emit(OpCode::MovImm(Register::R9, Value::Int(5)), None);
        bytecode.emit(OpCode::Add(Register::R8, Register::R9), None);
        bytecode.emit(OpCode::MovReg(Register::RAX, Register::R8), None);
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
        bytecode.emit(OpCode::MovImm(Register::R8, Value::Int(42)), None);
        bytecode.emit(OpCode::StoreVar("x".to_string(), Register::R8), None);
        // load x to RAX
        bytecode.emit(OpCode::MovVar(Register::RAX, "x".to_string()), None);
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
        bytecode.emit(OpCode::MovImm(Register::R8, Value::Int(10)), None);
        bytecode.emit(OpCode::MovImm(Register::R9, Value::Int(5)), None);
        bytecode.emit(OpCode::CmpGt(Register::R8, Register::R9), None);
        bytecode.emit(OpCode::MovReg(Register::RAX, Register::R8), None);
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
        bytecode.emit(OpCode::MovImm(Register::R8, Value::Int(1)), None);
        bytecode.emit(OpCode::Push(Register::R8), None);
        bytecode.emit(OpCode::MovImm(Register::R8, Value::Int(2)), None);
        bytecode.emit(OpCode::Push(Register::R8), None);
        bytecode.emit(OpCode::MovImm(Register::R8, Value::Int(3)), None);
        bytecode.emit(OpCode::Push(Register::R8), None);
        bytecode.emit(OpCode::MakeArray(Register::RAX, 3), None);
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
        // Place arguments in R8, R9 (first 2 registers)
        bytecode.emit(OpCode::MovImm(Register::R8, Value::Int(10)), None);
        bytecode.emit(OpCode::MovImm(Register::R9, Value::Int(20)), None);
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
        bytecode.emit(OpCode::Jmp(0), None);

        // Function body starts here
        let body_start = bytecode.current_address();
        bytecode.emit(OpCode::MovVar(Register::R8, "a".to_string()), None);
        bytecode.emit(OpCode::MovVar(Register::R9, "b".to_string()), None);
        bytecode.emit(OpCode::Add(Register::R8, Register::R9), None);
        bytecode.emit(OpCode::MovReg(Register::RAX, Register::R8), None);
        bytecode.emit(OpCode::Ret, None);
        let body_end = bytecode.current_address();

        // Patch skip jump to here
        bytecode.patch_jump(skip_jump_addr, body_end);

        // Define the function
        bytecode.emit(
            OpCode::DefineFunc(Box::new(FuncDefInfo {
                name: "add".to_string(),
                params: vec!["a".to_string(), "b".to_string()],
                body_start,
                body_end,
                decorators: vec![],
            })),
            None,
        );

        // Call the function: add(5, 7)
        bytecode.emit(OpCode::MovImm(Register::R8, Value::Int(5)), None);
        bytecode.emit(OpCode::MovImm(Register::R9, Value::Int(7)), None);
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
        bytecode.emit(OpCode::Jmp(0), None);

        // Function body starts here
        let body_start = bytecode.current_address();

        // if (n <= 1)
        bytecode.emit(OpCode::MovVar(Register::R8, "n".to_string()), None);
        bytecode.emit(OpCode::MovImm(Register::R9, Value::Int(1)), None);
        bytecode.emit(OpCode::CmpLe(Register::R8, Register::R9), None);
        let else_jump = bytecode.current_address();
        bytecode.emit(OpCode::Jz(Register::R8, 0), None);

        // then: return n
        bytecode.emit(OpCode::MovVar(Register::RAX, "n".to_string()), None);
        bytecode.emit(OpCode::Ret, None);

        // Patch else jump to here
        bytecode.patch_jump(else_jump, bytecode.current_address());

        // else: return fib(n - 1) + fib(n - 2)
        // fib(n - 1)
        bytecode.emit(OpCode::MovVar(Register::R8, "n".to_string()), None);
        bytecode.emit(OpCode::MovImm(Register::R9, Value::Int(1)), None);
        bytecode.emit(OpCode::Sub(Register::R8, Register::R9), None);
        bytecode.emit(OpCode::CallVar("fib".to_string(), 1), None);

        // Save first result to stack
        bytecode.emit(OpCode::Push(Register::RAX), None);

        // fib(n - 2)
        bytecode.emit(OpCode::MovVar(Register::R8, "n".to_string()), None);
        bytecode.emit(OpCode::MovImm(Register::R9, Value::Int(2)), None);
        bytecode.emit(OpCode::Sub(Register::R8, Register::R9), None);
        bytecode.emit(OpCode::CallVar("fib".to_string(), 1), None);

        // Pop first result and add: first + RAX
        bytecode.emit(OpCode::PopReg(Register::R10), None);
        bytecode.emit(OpCode::Add(Register::R10, Register::RAX), None);
        bytecode.emit(OpCode::MovReg(Register::RAX, Register::R10), None);
        bytecode.emit(OpCode::Ret, None);

        let body_end = bytecode.current_address();

        // Patch skip jump to here
        bytecode.patch_jump(skip_jump_addr, body_end);

        // Define the function
        bytecode.emit(
            OpCode::DefineFunc(Box::new(FuncDefInfo {
                name: "fib".to_string(),
                params: vec!["n".to_string()],
                body_start,
                body_end,
                decorators: vec![],
            })),
            None,
        );

        // Call the function: fib(10)
        bytecode.emit(OpCode::MovImm(Register::R8, Value::Int(10)), None);
        bytecode.emit(OpCode::CallVar("fib".to_string(), 1), None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        match result {
            Some(Value::Int(n)) => assert_eq!(n, 55), // fib(10) = 55
            other => panic!("Expected Int(55), got {:?}", other),
        }
    }

    #[test]
    fn test_polymorphic_operations_mixed_types() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();

        // Test: 10 (int) + 3.14 (float) = 13.14 (float)
        let idx_int = bytecode.add_constant(Value::Int(10));
        let idx_float = bytecode.add_constant(Value::Float(3.14));

        bytecode.emit(OpCode::MovConst(Register::R8, idx_int), None);
        bytecode.emit(OpCode::MovConst(Register::R9, idx_float), None);
        bytecode.emit(OpCode::Add(Register::R8, Register::R9), None);
        bytecode.emit(OpCode::MovReg(Register::RAX, Register::R8), None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        match result {
            Some(Value::Float(f)) => assert!((f - 13.14).abs() < 0.01),
            other => panic!("Expected Float(13.14), got {:?}", other),
        }
    }

    #[test]
    fn test_polymorphic_comparison_same_types() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();

        // Test: 5 (int) < 10 (int) = true
        let idx1 = bytecode.add_constant(Value::Int(5));
        let idx2 = bytecode.add_constant(Value::Int(10));

        bytecode.emit(OpCode::MovConst(Register::R8, idx1), None);
        bytecode.emit(OpCode::MovConst(Register::R9, idx2), None);
        bytecode.emit(OpCode::CmpLt(Register::R8, Register::R9), None);
        bytecode.emit(OpCode::MovReg(Register::RAX, Register::R8), None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        match result {
            Some(Value::Bool(b)) => assert_eq!(b, true),
            other => panic!("Expected Bool(true), got {:?}", other),
        }
    }

    #[test]
    fn test_constant_pooling() {
        let mut bytecode = ByteCode::new();

        // Add the same constant multiple times
        let idx1 = bytecode.add_constant(Value::Int(42));
        let idx2 = bytecode.add_constant(Value::Int(42));
        let idx3 = bytecode.add_constant(Value::Int(100));
        let idx4 = bytecode.add_constant(Value::Int(42));

        // First two should be the same index (deduplicated)
        assert_eq!(idx1, idx2);
        assert_eq!(idx1, idx4);
        // Third should be different
        assert_ne!(idx1, idx3);

        // Pool should only have 2 constants
        assert_eq!(bytecode.constants.len(), 2);
    }

    #[test]
    fn test_constant_pooling_strings() {
        let mut bytecode = ByteCode::new();

        // Add the same string multiple times
        let idx1 = bytecode.add_constant(Value::String("hello".to_string()));
        let idx2 = bytecode.add_constant(Value::String("hello".to_string()));
        let idx3 = bytecode.add_constant(Value::String("world".to_string()));

        // First two should be the same index
        assert_eq!(idx1, idx2);
        // Third should be different
        assert_ne!(idx1, idx3);

        // Pool should only have 2 constants
        assert_eq!(bytecode.constants.len(), 2);
    }

    #[test]
    fn test_push_const_pooled() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();

        // Add constants to pool
        let idx1 = bytecode.add_constant(Value::Int(10));
        let idx2 = bytecode.add_constant(Value::Int(20));

        // Use pooled constants
        bytecode.emit(OpCode::MovConst(Register::R8, idx1), None);
        bytecode.emit(OpCode::MovConst(Register::R9, idx2), None);
        bytecode.emit(OpCode::Add(Register::R8, Register::R9), None);
        bytecode.emit(OpCode::MovReg(Register::RAX, Register::R8), None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        match result {
            Some(Value::Int(n)) => assert_eq!(n, 30),
            _ => panic!("Expected Int(30)"),
        }
    }

    #[test]
    fn test_constant_pooling_floats() {
        let mut bytecode = ByteCode::new();

        // Add the same float multiple times
        let idx1 = bytecode.add_constant(Value::Float(3.14));
        let idx2 = bytecode.add_constant(Value::Float(3.14));
        let idx3 = bytecode.add_constant(Value::Float(2.71));

        // First two should be the same index
        assert_eq!(idx1, idx2);
        // Third should be different
        assert_ne!(idx1, idx3);

        // Pool should only have 2 constants
        assert_eq!(bytecode.constants.len(), 2);
    }

    #[test]
    fn test_inline_cache_member_access() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();

        // Create a struct with a member: { x: 42 }
        let idx_key = bytecode.add_constant(Value::String("x".to_string()));
        let idx_val = bytecode.add_constant(Value::Int(42));

        bytecode.emit(OpCode::MovConst(Register::R8, idx_key), None);
        bytecode.emit(OpCode::Push(Register::R8), None);
        bytecode.emit(OpCode::MovConst(Register::R8, idx_val), None);
        bytecode.emit(OpCode::Push(Register::R8), None);
        bytecode.emit(OpCode::MakeStruct(Register::RAX, 1), None);

        // Store struct in a variable
        bytecode.emit(OpCode::StoreVar("obj".to_string(), Register::RAX), None);

        // Access member - this will create a cache entry at this instruction address
        bytecode.emit(OpCode::MovVar(Register::R8, "obj".to_string()), None);
        bytecode.emit(OpCode::Member(Register::RAX, Register::R8, "x".to_string()), None);

        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        // Verify result
        match result {
            Some(Value::Int(n)) => assert_eq!(n, 42),
            _ => panic!("Expected Int(42)"),
        }

        // Verify cache entry was created (even though this is first access, no "hits" yet)
        let cache_entries = vm.member_cache.len();
        assert_eq!(cache_entries, 1, "Should have created one cache entry");
    }

    #[test]
    fn test_inline_cache_multiple_members() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();

        // Create a struct with multiple members: { x: 10, y: 20 }
        let idx_key_x = bytecode.add_constant(Value::String("x".to_string()));
        let idx_val_x = bytecode.add_constant(Value::Int(10));
        let idx_key_y = bytecode.add_constant(Value::String("y".to_string()));
        let idx_val_y = bytecode.add_constant(Value::Int(20));

        bytecode.emit(OpCode::MovConst(Register::R8, idx_key_x), None);
        bytecode.emit(OpCode::Push(Register::R8), None);
        bytecode.emit(OpCode::MovConst(Register::R8, idx_val_x), None);
        bytecode.emit(OpCode::Push(Register::R8), None);
        bytecode.emit(OpCode::MovConst(Register::R8, idx_key_y), None);
        bytecode.emit(OpCode::Push(Register::R8), None);
        bytecode.emit(OpCode::MovConst(Register::R8, idx_val_y), None);
        bytecode.emit(OpCode::Push(Register::R8), None);
        bytecode.emit(OpCode::MakeStruct(Register::RAX, 2), None);

        // Store struct in a variable
        bytecode.emit(OpCode::StoreVar("obj".to_string(), Register::RAX), None);

        // Access first member
        bytecode.emit(OpCode::MovVar(Register::R8, "obj".to_string()), None);
        bytecode.emit(OpCode::Member(Register::R9, Register::R8, "x".to_string()), None);

        // Access second member
        bytecode.emit(OpCode::MovVar(Register::R8, "obj".to_string()), None);
        bytecode.emit(OpCode::Member(Register::RAX, Register::R8, "y".to_string()), None);

        // Add the results: R9 + RAX
        bytecode.emit(OpCode::Add(Register::R9, Register::RAX), None);
        bytecode.emit(OpCode::MovReg(Register::RAX, Register::R9), None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        // Verify result: x + y = 30
        match result {
            Some(Value::Int(n)) => assert_eq!(n, 30),
            _ => panic!("Expected Int(30)"),
        }
    }

    #[test]
    fn test_inline_cache_miss_tracking() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();

        // Create a struct with one member: { x: 42 }
        let idx_key = bytecode.add_constant(Value::String("x".to_string()));
        let idx_val = bytecode.add_constant(Value::Int(42));

        bytecode.emit(OpCode::MovConst(Register::R8, idx_key), None);
        bytecode.emit(OpCode::Push(Register::R8), None);
        bytecode.emit(OpCode::MovConst(Register::R8, idx_val), None);
        bytecode.emit(OpCode::Push(Register::R8), None);
        bytecode.emit(OpCode::MakeStruct(Register::R8, 1), None);

        // Try to access non-existent member (will fail)
        bytecode.emit(OpCode::Member(Register::RAX, Register::R8, "nonexistent".to_string()), None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run();

        // Should fail with error
        assert!(result.is_err(), "Should error on nonexistent member");

        // Verify cache miss was tracked
        let (_hits, misses) = vm.get_cache_stats();
        assert_eq!(misses, 1, "Cache should have recorded exactly one miss");
    }

    #[test]
    fn test_bytecode_serialization() {
        let mut bytecode = ByteCode::new();

        // Add some constants
        let idx1 = bytecode.add_constant(Value::Int(10));
        let idx2 = bytecode.add_constant(Value::Int(20));

        // Add instructions
        bytecode.emit(OpCode::MovConst(Register::R8, idx1), Some(1));
        bytecode.emit(OpCode::MovConst(Register::R9, idx2), Some(2));
        bytecode.emit(OpCode::Add(Register::R8, Register::R9), Some(3));
        bytecode.emit(OpCode::Halt, None);

        // Serialize
        let serialized = bytecode.serialize();

        // Check header
        assert!(serialized.contains("RUMINA-BYTECODE-V1"));
        assert!(serialized.contains("CONSTANTS: 2"));
        assert!(serialized.contains("MovConst"));
    }

    #[test]
    #[ignore = "Deserialization not yet implemented for register-based bytecode"]
    fn test_bytecode_deserialization() {
        let bytecode_text = r#"RUMINA-BYTECODE-V1
CONSTANTS: 2
CONST[0]: Int(10)
CONST[1]: Int(20)

INSTRUCTIONS:
0000 [L1] MovConst(R8, 0)
0001 [L2] MovConst(R9, 1)
0002 [L3] Add(R8, R9)
0003 [L?] Halt
"#;

        let bytecode = ByteCode::deserialize(bytecode_text).unwrap();

        assert_eq!(bytecode.constants.len(), 2);
        assert_eq!(bytecode.instructions.len(), 4);

        // Check constants
        match &bytecode.constants[0] {
            Value::Int(n) => assert_eq!(*n, 10),
            _ => panic!("Expected Int(10)"),
        }

        // Check instructions
        assert!(matches!(
            bytecode.instructions[0],
            OpCode::MovConst(Register::R8, 0)
        ));
        assert!(matches!(
            bytecode.instructions[1],
            OpCode::MovConst(Register::R9, 1)
        ));
        assert!(matches!(bytecode.instructions[2], OpCode::Add(Register::R8, Register::R9)));
        assert!(matches!(bytecode.instructions[3], OpCode::Halt));
    }

    #[test]
    #[ignore = "Deserialization not yet implemented for register-based bytecode"]
    fn test_bytecode_roundtrip() {
        // Create original bytecode
        let mut original = ByteCode::new();

        let idx1 = original.add_constant(Value::Int(42));
        let idx2 = original.add_constant(Value::String("test".to_string()));

        original.emit(OpCode::MovConst(Register::R8, idx1), Some(1));
        original.emit(OpCode::StoreVar("x".to_string(), Register::R8), Some(2));
        original.emit(OpCode::MovVar(Register::R8, "x".to_string()), Some(3));
        original.emit(OpCode::MovConst(Register::RAX, idx2), Some(4));
        original.emit(OpCode::Halt, None);

        // Serialize and deserialize
        let serialized = original.serialize();
        let deserialized = ByteCode::deserialize(&serialized).unwrap();

        // Compare
        assert_eq!(deserialized.constants.len(), original.constants.len());
        assert_eq!(deserialized.instructions.len(), original.instructions.len());

        // Execute both and compare results
        let globals = Rc::new(RefCell::new(HashMap::new()));

        let mut vm1 = VM::new(globals.clone());
        vm1.load(original);
        let result1 = vm1.run().unwrap();

        let mut vm2 = VM::new(globals.clone());
        vm2.load(deserialized);
        let result2 = vm2.run().unwrap();

        match (result1, result2) {
            (Some(Value::String(s1)), Some(Value::String(s2))) => assert_eq!(s1, s2),
            _ => panic!("Expected matching String results"),
        }
    }

    #[test]
    #[ignore = "Deserialization not yet implemented for register-based bytecode"]
    fn test_bytecode_convert_type_roundtrip() {
        // Test serialization/deserialization of ConvertType opcode
        let mut original = ByteCode::new();

        // Test all DeclaredType variants with RAX register
        original.emit(OpCode::ConvertType(Register::RAX, DeclaredType::Int), Some(1));
        original.emit(OpCode::ConvertType(Register::RAX, DeclaredType::Float), Some(2));
        original.emit(OpCode::ConvertType(Register::RAX, DeclaredType::Bool), Some(3));
        original.emit(OpCode::ConvertType(Register::RAX, DeclaredType::String), Some(4));
        original.emit(OpCode::ConvertType(Register::RAX, DeclaredType::Rational), Some(5));
        original.emit(OpCode::ConvertType(Register::RAX, DeclaredType::Irrational), Some(6));
        original.emit(OpCode::ConvertType(Register::RAX, DeclaredType::Complex), Some(7));
        original.emit(OpCode::ConvertType(Register::RAX, DeclaredType::Array), Some(8));
        original.emit(OpCode::ConvertType(Register::RAX, DeclaredType::BigInt), Some(9));
        original.emit(OpCode::Halt, None);

        // Serialize and deserialize
        let serialized = original.serialize();
        let deserialized = ByteCode::deserialize(&serialized).unwrap();

        // Verify all opcodes match
        assert_eq!(deserialized.instructions.len(), original.instructions.len());
        for i in 0..original.instructions.len() {
            assert_eq!(
                deserialized.instructions[i], original.instructions[i],
                "Opcode at index {} should match",
                i
            );
        }
    }
}
