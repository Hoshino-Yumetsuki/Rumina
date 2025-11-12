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

    /// Push a constant from the constant pool onto the stack
    /// Similar to: PUSH [constant_pool + index]
    PushConstPooled(usize),

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

    /// Specialized add for known integer types (hot path optimization)
    /// Similar to: ADD rax, rbx (with type check)
    AddInt,

    /// Subtract top two stack values (TOS-1 - TOS)
    /// Similar to: SUB rax, rbx
    Sub,

    /// Specialized subtract for known integer types (hot path optimization)
    SubInt,

    /// Multiply top two stack values
    /// Similar to: MUL rbx
    Mul,

    /// Specialized multiply for known integer types (hot path optimization)
    MulInt,

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

    // ===== Type Conversion Instructions =====
    /// Convert top stack value to specified type
    ConvertType(DeclaredType),
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

    /// Add a constant to the pool or return existing index
    /// This deduplicates constants to reduce memory usage
    pub fn add_constant(&mut self, value: Value) -> usize {
        // Check if constant already exists in the pool
        for (i, existing) in self.constants.iter().enumerate() {
            if Self::values_equal(existing, &value) {
                return i;
            }
        }

        // Add new constant
        let index = self.constants.len();
        self.constants.push(value);
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
            Value::String(s) => format!("String(\"{}\")", s.replace('"', "\\\"")),
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
            return Ok(Value::String(str_val.replace("\\\"", "\"")));
        }
        if s == "Null" {
            return Ok(Value::Null);
        }
        Err(format!("Unsupported value type: {}", s))
    }

    fn serialize_opcode(op: &OpCode) -> String {
        match op {
            OpCode::PushConst(v) => format!("PushConst({})", Self::serialize_value(v)),
            OpCode::PushConstPooled(i) => format!("PushConstPooled({})", i),
            OpCode::PushVar(name) => format!("PushVar({})", name),
            OpCode::PopVar(name) => format!("PopVar({})", name),
            OpCode::Dup => "Dup".to_string(),
            OpCode::Pop => "Pop".to_string(),
            OpCode::Add => "Add".to_string(),
            OpCode::AddInt => "AddInt".to_string(),
            OpCode::Sub => "Sub".to_string(),
            OpCode::SubInt => "SubInt".to_string(),
            OpCode::Mul => "Mul".to_string(),
            OpCode::MulInt => "MulInt".to_string(),
            OpCode::Div => "Div".to_string(),
            OpCode::Mod => "Mod".to_string(),
            OpCode::Pow => "Pow".to_string(),
            OpCode::Neg => "Neg".to_string(),
            OpCode::Factorial => "Factorial".to_string(),
            OpCode::Not => "Not".to_string(),
            OpCode::And => "And".to_string(),
            OpCode::Or => "Or".to_string(),
            OpCode::Eq => "Eq".to_string(),
            OpCode::Neq => "Neq".to_string(),
            OpCode::Gt => "Gt".to_string(),
            OpCode::Gte => "Gte".to_string(),
            OpCode::Lt => "Lt".to_string(),
            OpCode::Lte => "Lte".to_string(),
            OpCode::Jump(addr) => format!("Jump({})", addr),
            OpCode::JumpIfFalse(addr) => format!("JumpIfFalse({})", addr),
            OpCode::JumpIfTrue(addr) => format!("JumpIfTrue({})", addr),
            OpCode::Call(addr) => format!("Call({})", addr),
            OpCode::CallVar(name, argc) => format!("CallVar({}, {})", name, argc),
            OpCode::Return => "Return".to_string(),
            OpCode::MakeArray(size) => format!("MakeArray({})", size),
            OpCode::MakeStruct(size) => format!("MakeStruct({})", size),
            OpCode::Index => "Index".to_string(),
            OpCode::Member(name) => format!("Member({})", name),
            OpCode::IndexAssign => "IndexAssign".to_string(),
            OpCode::MemberAssign(name) => format!("MemberAssign({})", name),
            OpCode::EnterScope => "EnterScope".to_string(),
            OpCode::ExitScope => "ExitScope".to_string(),
            OpCode::LoopBegin => "LoopBegin".to_string(),
            OpCode::LoopEnd => "LoopEnd".to_string(),
            OpCode::Break => "Break".to_string(),
            OpCode::Continue => "Continue".to_string(),
            OpCode::Nop => "Nop".to_string(),
            OpCode::Halt => "Halt".to_string(),
            OpCode::DefineFunc {
                name,
                params,
                body_start,
                body_end,
                decorators,
            } => {
                format!(
                    "DefineFunc({}, [{}], {}, {}, [{}])",
                    name,
                    params.join(","),
                    body_start,
                    body_end,
                    decorators.join(",")
                )
            }
            OpCode::MakeLambda {
                params,
                body_start,
                body_end,
            } => {
                format!(
                    "MakeLambda([{}], {}, {})",
                    params.join(","),
                    body_start,
                    body_end
                )
            }
        }
    }

    fn deserialize_opcode(s: &str) -> Result<OpCode, String> {
        if s == "Dup" {
            return Ok(OpCode::Dup);
        }
        if s == "Pop" {
            return Ok(OpCode::Pop);
        }
        if s == "Add" {
            return Ok(OpCode::Add);
        }
        if s == "AddInt" {
            return Ok(OpCode::AddInt);
        }
        if s == "Sub" {
            return Ok(OpCode::Sub);
        }
        if s == "SubInt" {
            return Ok(OpCode::SubInt);
        }
        if s == "Mul" {
            return Ok(OpCode::Mul);
        }
        if s == "MulInt" {
            return Ok(OpCode::MulInt);
        }
        if s == "Div" {
            return Ok(OpCode::Div);
        }
        if s == "Mod" {
            return Ok(OpCode::Mod);
        }
        if s == "Pow" {
            return Ok(OpCode::Pow);
        }
        if s == "Neg" {
            return Ok(OpCode::Neg);
        }
        if s == "Factorial" {
            return Ok(OpCode::Factorial);
        }
        if s == "Not" {
            return Ok(OpCode::Not);
        }
        if s == "And" {
            return Ok(OpCode::And);
        }
        if s == "Or" {
            return Ok(OpCode::Or);
        }
        if s == "Eq" {
            return Ok(OpCode::Eq);
        }
        if s == "Neq" {
            return Ok(OpCode::Neq);
        }
        if s == "Gt" {
            return Ok(OpCode::Gt);
        }
        if s == "Gte" {
            return Ok(OpCode::Gte);
        }
        if s == "Lt" {
            return Ok(OpCode::Lt);
        }
        if s == "Lte" {
            return Ok(OpCode::Lte);
        }
        if s == "Return" {
            return Ok(OpCode::Return);
        }
        if s == "Index" {
            return Ok(OpCode::Index);
        }
        if s == "IndexAssign" {
            return Ok(OpCode::IndexAssign);
        }
        if s == "EnterScope" {
            return Ok(OpCode::EnterScope);
        }
        if s == "ExitScope" {
            return Ok(OpCode::ExitScope);
        }
        if s == "LoopBegin" {
            return Ok(OpCode::LoopBegin);
        }
        if s == "LoopEnd" {
            return Ok(OpCode::LoopEnd);
        }
        if s == "Break" {
            return Ok(OpCode::Break);
        }
        if s == "Continue" {
            return Ok(OpCode::Continue);
        }
        if s == "Nop" {
            return Ok(OpCode::Nop);
        }
        if s == "Halt" {
            return Ok(OpCode::Halt);
        }

        if let Some(val) = s
            .strip_prefix("PushConst(")
            .and_then(|s| s.strip_suffix(")"))
        {
            return Ok(OpCode::PushConst(Self::deserialize_value(val)?));
        }
        if let Some(idx) = s
            .strip_prefix("PushConstPooled(")
            .and_then(|s| s.strip_suffix(")"))
        {
            return Ok(OpCode::PushConstPooled(
                idx.parse().map_err(|_| "Invalid index")?,
            ));
        }
        if let Some(name) = s.strip_prefix("PushVar(").and_then(|s| s.strip_suffix(")")) {
            return Ok(OpCode::PushVar(name.to_string()));
        }
        if let Some(name) = s.strip_prefix("PopVar(").and_then(|s| s.strip_suffix(")")) {
            return Ok(OpCode::PopVar(name.to_string()));
        }
        if let Some(addr) = s.strip_prefix("Jump(").and_then(|s| s.strip_suffix(")")) {
            return Ok(OpCode::Jump(addr.parse().map_err(|_| "Invalid address")?));
        }
        if let Some(addr) = s
            .strip_prefix("JumpIfFalse(")
            .and_then(|s| s.strip_suffix(")"))
        {
            return Ok(OpCode::JumpIfFalse(
                addr.parse().map_err(|_| "Invalid address")?,
            ));
        }
        if let Some(addr) = s
            .strip_prefix("JumpIfTrue(")
            .and_then(|s| s.strip_suffix(")"))
        {
            return Ok(OpCode::JumpIfTrue(
                addr.parse().map_err(|_| "Invalid address")?,
            ));
        }
        if let Some(addr) = s.strip_prefix("Call(").and_then(|s| s.strip_suffix(")")) {
            return Ok(OpCode::Call(addr.parse().map_err(|_| "Invalid address")?));
        }
        if let Some(args) = s.strip_prefix("CallVar(").and_then(|s| s.strip_suffix(")")) {
            let parts: Vec<&str> = args.splitn(2, ", ").collect();
            if parts.len() == 2 {
                let name = parts[0].to_string();
                let argc = parts[1].parse().map_err(|_| "Invalid arg count")?;
                return Ok(OpCode::CallVar(name, argc));
            }
        }
        if let Some(size) = s
            .strip_prefix("MakeArray(")
            .and_then(|s| s.strip_suffix(")"))
        {
            return Ok(OpCode::MakeArray(size.parse().map_err(|_| "Invalid size")?));
        }
        if let Some(size) = s
            .strip_prefix("MakeStruct(")
            .and_then(|s| s.strip_suffix(")"))
        {
            return Ok(OpCode::MakeStruct(
                size.parse().map_err(|_| "Invalid size")?,
            ));
        }
        if let Some(name) = s.strip_prefix("Member(").and_then(|s| s.strip_suffix(")")) {
            return Ok(OpCode::Member(name.to_string()));
        }
        if let Some(name) = s
            .strip_prefix("MemberAssign(")
            .and_then(|s| s.strip_suffix(")"))
        {
            return Ok(OpCode::MemberAssign(name.to_string()));
        }
        if let Some(args) = s
            .strip_prefix("DefineFunc(")
            .and_then(|s| s.strip_suffix(")"))
        {
            // Parse: name, [params], body_start, body_end, [decorators]
            let parts: Vec<&str> = args.split(", ").collect();
            if parts.len() >= 4 {
                let name = parts[0].to_string();
                let params_str = parts[1].trim_matches(|c| c == '[' || c == ']');
                let params: Vec<String> = if params_str.is_empty() {
                    vec![]
                } else {
                    params_str.split(',').map(|s| s.to_string()).collect()
                };
                let body_start = parts[2].parse().map_err(|_| "Invalid body_start")?;
                let body_end = parts[3].parse().map_err(|_| "Invalid body_end")?;
                let decorators = if parts.len() > 4 {
                    let dec_str = parts[4].trim_matches(|c| c == '[' || c == ']');
                    if dec_str.is_empty() {
                        vec![]
                    } else {
                        dec_str.split(',').map(|s| s.to_string()).collect()
                    }
                } else {
                    vec![]
                };
                return Ok(OpCode::DefineFunc {
                    name,
                    params,
                    body_start,
                    body_end,
                    decorators,
                });
            }
        }
        if let Some(args) = s
            .strip_prefix("MakeLambda(")
            .and_then(|s| s.strip_suffix(")"))
        {
            // Parse: [params], body_start, body_end
            let parts: Vec<&str> = args.split(", ").collect();
            if parts.len() >= 3 {
                let params_str = parts[0].trim_matches(|c| c == '[' || c == ']');
                let params: Vec<String> = if params_str.is_empty() {
                    vec![]
                } else {
                    params_str.split(',').map(|s| s.to_string()).collect()
                };
                let body_start = parts[1].parse().map_err(|_| "Invalid body_start")?;
                let body_end = parts[2].parse().map_err(|_| "Invalid body_end")?;
                return Ok(OpCode::MakeLambda {
                    params,
                    body_start,
                    body_end,
                });
            }
        }

        Err(format!("Unknown opcode: {}", s))
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

    /// Local variables in this frame
    locals: HashMap<String, Value>,
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

    /// Inline cache for member access (maps instruction address to cache)
    member_cache: HashMap<usize, InlineCache>,

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
            member_cache: HashMap::new(),
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

            OpCode::PushConstPooled(index) => {
                let value = self
                    .bytecode
                    .constants
                    .get(index)
                    .ok_or_else(|| {
                        RuminaError::runtime(format!("Invalid constant pool index: {}", index))
                    })?
                    .clone();
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
            OpCode::AddInt => {
                // Fast path for integer addition
                let right = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;
                let left = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;

                match (&left, &right) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.stack.push(Value::Int(a + b));
                    }
                    _ => {
                        // Fallback to generic add
                        let result = left.vm_add(&right).map_err(|e| RuminaError::runtime(e))?;
                        self.stack.push(result);
                    }
                }
            }
            OpCode::Sub => self.binary_op(|a, b| a.vm_sub(b))?,
            OpCode::SubInt => {
                // Fast path for integer subtraction
                let right = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;
                let left = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;

                match (&left, &right) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.stack.push(Value::Int(a - b));
                    }
                    _ => {
                        // Fallback to generic sub
                        let result = left.vm_sub(&right).map_err(|e| RuminaError::runtime(e))?;
                        self.stack.push(result);
                    }
                }
            }
            OpCode::Mul => self.binary_op(|a, b| a.vm_mul(b))?,
            OpCode::MulInt => {
                // Fast path for integer multiplication
                let right = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;
                let left = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;

                match (&left, &right) {
                    (Value::Int(a), Value::Int(b)) => {
                        self.stack.push(Value::Int(a * b));
                    }
                    _ => {
                        // Fallback to generic mul
                        let result = left.vm_mul(&right).map_err(|e| RuminaError::runtime(e))?;
                        self.stack.push(result);
                    }
                }
            }
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
                let cache_addr = self.ip - 1; // Address of this Member instruction

                let object = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;

                match &object {
                    Value::Struct(s) => {
                        let s_ref = s.borrow();
                        if let Some(value) = s_ref.get(&member_name) {
                            // Check if we have a cache entry for this instruction
                            if let Some(cache) = self.member_cache.get_mut(&cache_addr) {
                                // Cache exists - increment hits
                                cache.hits += 1;
                            } else {
                                // First access - initialize cache entry
                                self.member_cache
                                    .insert(cache_addr, InlineCache::new(member_name.clone()));
                            }

                            self.stack.push(value.clone());
                        } else {
                            // Member not found - track miss
                            if let Some(cache) = self.member_cache.get_mut(&cache_addr) {
                                cache.misses += 1;
                            } else {
                                // Initialize cache with miss
                                let mut cache = InlineCache::new(member_name.clone());
                                cache.misses = 1;
                                self.member_cache.insert(cache_addr, cache);
                            }

                            return Err(RuminaError::runtime(format!(
                                "Struct does not have member '{}'",
                                member_name
                            )));
                        }
                    }
                    _ => {
                        // Non-struct type - track miss
                        if let Some(cache) = self.member_cache.get_mut(&cache_addr) {
                            cache.misses += 1;
                        } else {
                            // Initialize cache with miss
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
                    Value::Lambda {
                        params,
                        body,
                        closure,
                        ..
                    } => {
                        // Check recursion depth
                        if self.recursion_depth >= self.max_recursion_depth {
                            return Err(RuminaError::runtime(format!(
                                "Maximum recursion depth ({}) exceeded",
                                self.max_recursion_depth
                            )));
                        }

                        // Check parameter count
                        if args.len() != params.len() {
                            return Err(RuminaError::runtime(format!(
                                "Lambda expects {} arguments, got {}",
                                params.len(),
                                args.len()
                            )));
                        }

                        // Extract lambda_id from the body marker
                        let lambda_id = match body.as_ref() {
                            crate::ast::Stmt::Include(id) => id.clone(),
                            _ => {
                                // Fallback: try to find by params (less reliable)
                                let mut found_id = None;
                                for (name, _) in &self.functions {
                                    if name.starts_with("__lambda_") {
                                        found_id = Some(name.clone());
                                        break;
                                    }
                                }
                                found_id.ok_or_else(|| {
                                    RuminaError::runtime("Lambda ID not found".to_string())
                                })?
                            }
                        };

                        // Look up the lambda function info
                        let func_info = self
                            .functions
                            .get(&lambda_id)
                            .ok_or_else(|| {
                                RuminaError::runtime(format!("Lambda '{}' not found", lambda_id))
                            })?
                            .clone();

                        // Create new call frame
                        let frame = CallFrame {
                            return_address: self.ip,
                            base_pointer: self.stack.len(),
                            function_name: lambda_id.clone(),
                            locals: std::mem::take(&mut self.locals), // Save current locals
                        };

                        // Push call frame
                        self.call_stack.push(frame);
                        self.recursion_depth += 1;

                        // Set up parameters as local variables
                        // Start with the closure environment
                        self.locals = closure.borrow().clone();
                        for (param_name, arg_value) in params.iter().zip(args.into_iter()) {
                            self.locals.insert(param_name.clone(), arg_value);
                        }

                        // Jump to lambda body
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

            OpCode::MakeLambda {
                params,
                body_start: _,
                body_end: _,
            } => {
                // Pop the lambda_id from the stack
                let lambda_id_value = self
                    .stack
                    .pop()
                    .ok_or_else(|| RuminaError::runtime("Stack underflow".to_string()))?;

                let lambda_id = match lambda_id_value {
                    Value::String(id) => id,
                    _ => {
                        return Err(RuminaError::runtime(
                            "Expected lambda ID as string".to_string(),
                        ));
                    }
                };

                // Create a lambda value with current locals as closure
                let closure = if !self.locals.is_empty() {
                    // Clone current locals for the closure
                    Rc::new(RefCell::new(self.locals.clone()))
                } else {
                    // Use globals as closure
                    Rc::clone(&self.globals)
                };

                // Store the lambda_id in the body as an Include statement (marker)
                // This allows us to identify which lambda this is when calling
                let marker_body = Box::new(crate::ast::Stmt::Include(lambda_id.clone()));

                let lambda_value = Value::Lambda {
                    params: params.clone(),
                    body: marker_body,
                    closure,
                };

                // The lambda was already registered by DefineFunc, no need to store again
                // Just push the lambda value onto stack
                self.stack.push(lambda_value);
            }

            OpCode::MemberAssign(_) | OpCode::IndexAssign | OpCode::Call(_) => {
                return Err(RuminaError::runtime(
                    "Opcode not yet implemented".to_string(),
                ));
            }

            OpCode::ConvertType(dtype) => {
                let val = self.pop()?;
                let converted = self.convert_to_type(val, dtype)?;
                self.push(converted);
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

    /// Convert value to specified type
    fn convert_to_type(&self, val: Value, dtype: &DeclaredType) -> Result<Value, RuminaError> {
        use crate::interpreter::convert;
        convert::convert_to_declared_type(val, dtype)
            .map_err(|e| RuminaError::runtime(e))
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
        bytecode.emit(OpCode::PushConstPooled(idx1), None);
        bytecode.emit(OpCode::PushConstPooled(idx2), None);
        bytecode.emit(OpCode::Add, None);
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

        bytecode.emit(OpCode::PushConstPooled(idx_key), None);
        bytecode.emit(OpCode::PushConstPooled(idx_val), None);
        bytecode.emit(OpCode::MakeStruct(1), None);

        // Store struct in a variable
        bytecode.emit(OpCode::PopVar("obj".to_string()), None);

        // Access member - this will create a cache entry at this instruction address
        bytecode.emit(OpCode::PushVar("obj".to_string()), None);
        bytecode.emit(OpCode::Member("x".to_string()), None);

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

        bytecode.emit(OpCode::PushConstPooled(idx_key_x), None);
        bytecode.emit(OpCode::PushConstPooled(idx_val_x), None);
        bytecode.emit(OpCode::PushConstPooled(idx_key_y), None);
        bytecode.emit(OpCode::PushConstPooled(idx_val_y), None);
        bytecode.emit(OpCode::MakeStruct(2), None);

        // Store struct in a variable
        bytecode.emit(OpCode::PopVar("obj".to_string()), None);

        // Access first member
        bytecode.emit(OpCode::PushVar("obj".to_string()), None);
        bytecode.emit(OpCode::Member("x".to_string()), None);

        // Access second member
        bytecode.emit(OpCode::PushVar("obj".to_string()), None);
        bytecode.emit(OpCode::Member("y".to_string()), None);

        // Add the results
        bytecode.emit(OpCode::Add, None);
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

        bytecode.emit(OpCode::PushConstPooled(idx_key), None);
        bytecode.emit(OpCode::PushConstPooled(idx_val), None);
        bytecode.emit(OpCode::MakeStruct(1), None);

        // Try to access non-existent member (will fail)
        bytecode.emit(OpCode::Member("nonexistent".to_string()), None);
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
    fn test_specialized_int_add() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();

        // Add constants to pool
        let idx1 = bytecode.add_constant(Value::Int(15));
        let idx2 = bytecode.add_constant(Value::Int(27));

        // Use specialized integer add
        bytecode.emit(OpCode::PushConstPooled(idx1), None);
        bytecode.emit(OpCode::PushConstPooled(idx2), None);
        bytecode.emit(OpCode::AddInt, None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        match result {
            Some(Value::Int(n)) => assert_eq!(n, 42),
            _ => panic!("Expected Int(42)"),
        }
    }

    #[test]
    fn test_specialized_int_sub() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();

        let idx1 = bytecode.add_constant(Value::Int(100));
        let idx2 = bytecode.add_constant(Value::Int(42));

        bytecode.emit(OpCode::PushConstPooled(idx1), None);
        bytecode.emit(OpCode::PushConstPooled(idx2), None);
        bytecode.emit(OpCode::SubInt, None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        match result {
            Some(Value::Int(n)) => assert_eq!(n, 58),
            _ => panic!("Expected Int(58)"),
        }
    }

    #[test]
    fn test_specialized_int_mul() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();

        let idx1 = bytecode.add_constant(Value::Int(6));
        let idx2 = bytecode.add_constant(Value::Int(7));

        bytecode.emit(OpCode::PushConstPooled(idx1), None);
        bytecode.emit(OpCode::PushConstPooled(idx2), None);
        bytecode.emit(OpCode::MulInt, None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        match result {
            Some(Value::Int(n)) => assert_eq!(n, 42),
            _ => panic!("Expected Int(42)"),
        }
    }

    #[test]
    fn test_specialized_int_fallback_to_generic() {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);

        let mut bytecode = ByteCode::new();

        // Mix int and float - should fallback to generic add
        let idx1 = bytecode.add_constant(Value::Int(10));
        let idx2 = bytecode.add_constant(Value::Float(3.14));

        bytecode.emit(OpCode::PushConstPooled(idx1), None);
        bytecode.emit(OpCode::PushConstPooled(idx2), None);
        bytecode.emit(OpCode::AddInt, None);
        bytecode.emit(OpCode::Halt, None);

        vm.load(bytecode);
        let result = vm.run().unwrap();

        // Result should be a float
        match result {
            Some(Value::Float(f)) => assert!((f - 13.14).abs() < 0.01),
            _ => panic!("Expected Float(13.14)"),
        }
    }

    #[test]
    fn test_bytecode_serialization() {
        let mut bytecode = ByteCode::new();

        // Add some constants
        let idx1 = bytecode.add_constant(Value::Int(10));
        let idx2 = bytecode.add_constant(Value::Int(20));

        // Add instructions
        bytecode.emit(OpCode::PushConstPooled(idx1), Some(1));
        bytecode.emit(OpCode::PushConstPooled(idx2), Some(2));
        bytecode.emit(OpCode::Add, Some(3));
        bytecode.emit(OpCode::Halt, None);

        // Serialize
        let serialized = bytecode.serialize();

        // Check header
        assert!(serialized.contains("RUMINA-BYTECODE-V1"));
        assert!(serialized.contains("CONSTANTS: 2"));
        assert!(serialized.contains("PushConstPooled(0)"));
        assert!(serialized.contains("PushConstPooled(1)"));
    }

    #[test]
    fn test_bytecode_deserialization() {
        let bytecode_text = r#"RUMINA-BYTECODE-V1
CONSTANTS: 2
CONST[0]: Int(10)
CONST[1]: Int(20)

INSTRUCTIONS:
0000 [L1] PushConstPooled(0)
0001 [L2] PushConstPooled(1)
0002 [L3] Add
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
            OpCode::PushConstPooled(0)
        ));
        assert!(matches!(
            bytecode.instructions[1],
            OpCode::PushConstPooled(1)
        ));
        assert!(matches!(bytecode.instructions[2], OpCode::Add));
        assert!(matches!(bytecode.instructions[3], OpCode::Halt));
    }

    #[test]
    fn test_bytecode_roundtrip() {
        // Create original bytecode
        let mut original = ByteCode::new();

        let idx1 = original.add_constant(Value::Int(42));
        let idx2 = original.add_constant(Value::String("test".to_string()));

        original.emit(OpCode::PushConstPooled(idx1), Some(1));
        original.emit(OpCode::PopVar("x".to_string()), Some(2));
        original.emit(OpCode::PushVar("x".to_string()), Some(3));
        original.emit(OpCode::PushConstPooled(idx2), Some(4));
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
}
