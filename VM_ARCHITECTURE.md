# Rumina VM Architecture

## Overview

Rumina uses a bytecode Virtual Machine (VM) with an instruction set inspired by x86_64 CISC architecture. **The VM is now the default execution engine for all Lamina code.** This document describes the VM's design, instruction set, and implementation details.

## Architecture

### Components

1. **Compiler (`src/compiler.rs`)**: Translates AST to bytecode instructions
2. **VM (`src/vm.rs`)**: Executes bytecode with stack-based operations
3. **VM Operations Bridge (`src/vm_ops.rs`)**: Connects VM to existing interpreter operations
4. **Bytecode**: Sequence of opcodes with optional debug information

### Execution Model

- **Stack-based**: All operations work with a value stack
- **Instruction Pointer (IP)**: Points to the next instruction to execute
- **Global Variables**: Shared across all scopes, includes built-in functions
- **Local Variables**: Scoped to current execution context
- **Call Stack**: Manages function calls and returns with proper call frames
- **Decoupled Frontend**: Lexer and Parser produce AST, Compiler produces bytecode, VM executes

### API

All public APIs now use the VM by default:

- `run(source: &str) -> Result<(), RuminaError>` - Main entry point, uses VM
- `run_vm(source: &str) -> Result<Option<Value>, RuminaError>` - VM execution with return value
- `run_interpreter(source: &str) -> Result<Option<Value>, RuminaError>` - Alias for `run_vm()` (backward compatible)
- WASM `rumina(code: &str) -> String` - Uses VM internally

## Instruction Set

The instruction set is inspired by x86_64, using CISC-style operations with high-level abstractions suitable for a dynamic language.

### Data Movement Instructions (MOV family)

| OpCode | x86_64 Equivalent | Description |
|--------|-------------------|-------------|
| `PushConst(Value)` | `PUSH imm` | Push a constant value onto the stack |
| `PushVar(String)` | `MOV reg, [addr]` | Push a variable value onto the stack |
| `PopVar(String)` | `MOV [addr], reg` | Pop value from stack and store in variable |
| `Dup` | `PUSH [rsp]` | Duplicate top stack value |
| `Pop` | `ADD rsp, 8` | Pop and discard top stack value |

### Arithmetic Instructions (ADD, SUB, MUL, DIV family)

| OpCode | x86_64 Equivalent | Description |
|--------|-------------------|-------------|
| `Add` | `ADD rax, rbx` | Add top two stack values |
| `Sub` | `SUB rax, rbx` | Subtract (TOS-1 - TOS) |
| `Mul` | `MUL rbx` | Multiply top two stack values |
| `Div` | `DIV rbx` | Divide (TOS-1 / TOS) |
| `Mod` | `IDIV (rdx)` | Modulo operation |
| `Pow` | - | Power operation (TOS-1 ^ TOS) |
| `Neg` | `NEG rax` | Negate top stack value |
| `Factorial` | - | Factorial operation (postfix !) |

### Logical Instructions (CMP, TEST, AND, OR family)

| OpCode | x86_64 Equivalent | Description |
|--------|-------------------|-------------|
| `Not` | `NOT rax` | Logical NOT |
| `And` | `AND rax, rbx` | Logical AND |
| `Or` | `OR rax, rbx` | Logical OR |
| `Eq` | `CMP + SETE` | Compare equal |
| `Neq` | `CMP + SETNE` | Compare not equal |
| `Gt` | `CMP + SETG` | Compare greater than |
| `Gte` | `CMP + SETGE` | Compare greater or equal |
| `Lt` | `CMP + SETL` | Compare less than |
| `Lte` | `CMP + SETLE` | Compare less or equal |

### Control Flow Instructions (JMP, CALL, RET family)

| OpCode | x86_64 Equivalent | Description |
|--------|-------------------|-------------|
| `Jump(usize)` | `JMP addr` | Unconditional jump to address |
| `JumpIfFalse(usize)` | `TEST + JZ` | Jump if false (pop condition) |
| `JumpIfTrue(usize)` | `TEST + JNZ` | Jump if true (pop condition) |
| `Call(usize)` | `CALL addr` | Call function (address) |
| `CallVar(String, usize)` | `CALL [addr]` | Call function by name with N args |
| `Return` | `RET` | Return from function |

### Array/Structure Instructions (LEA, MOV family)

| OpCode | x86_64 Equivalent | Description |
|--------|-------------------|-------------|
| `MakeArray(usize)` | `MOV [addr], ...` | Create array from N stack values |
| `MakeStruct(usize)` | - | Create struct from N key-value pairs |
| `Index` | `MOV rax, [rbx + rcx*8]` | Array index access |
| `Member(String)` | `MOV rax, [rbx + offset]` | Member access |
| `IndexAssign` | - | Assign to array element |
| `MemberAssign(String)` | - | Assign to struct member |

### Function Definition Instructions

| OpCode | Description |
|--------|-------------|
| `DefineFunc {...}` | Define a function with metadata |
| `MakeLambda {...}` | Create lambda/closure |

### Scope Management

| OpCode | x86_64 Equivalent | Description |
|--------|-------------------|-------------|
| `EnterScope` | `PUSH rbp; MOV rbp, rsp` | Enter new local scope |
| `ExitScope` | `MOV rsp, rbp; POP rbp` | Exit local scope |

### Control Structures

| OpCode | Description |
|--------|-------------|
| `LoopBegin` | Mark loop begin (for continue) |
| `LoopEnd` | Mark loop end (for break) |
| `Break` | Break from loop |
| `Continue` | Continue loop |

### Special Instructions

| OpCode | x86_64 Equivalent | Description |
|--------|-------------------|-------------|
| `Nop` | `NOP` | No operation |
| `Halt` | `HLT` | Halt execution |

## Compilation Process

### 1. Lexing and Parsing
Input source code → Tokens → AST (unchanged from original interpreter)

### 2. Compilation
AST → Bytecode instructions

The compiler:
- Maintains a symbol table for variable resolution
- Manages jump targets for control flow
- Tracks loop contexts for break/continue
- Emits opcodes with optional line numbers for debugging

Example compilation:
```lamina
let x = 10;
let y = 20;
x + y;
```

Compiles to:
```
PushConst(Int(10))
PopVar("x")
PushConst(Int(20))
PopVar("y")
PushVar("x")
PushVar("y")
Add
Halt
```

### 3. Execution
The VM:
1. Loads bytecode
2. Initializes instruction pointer (IP) to 0
3. Executes instructions sequentially
4. Modifies IP for jumps/branches
5. Returns top of stack as result

## Integration with Existing Code

### Operation Bridge

The VM reuses the existing interpreter's operation implementations through `vm_ops.rs`:

- `vm_add()`, `vm_sub()`, etc. delegate to `interpreter.eval_binary_op()`
- Maintains exact same semantics as AST interpreter
- Ensures backward compatibility
- Leverages battle-tested operation logic

### Built-in Functions

Built-in functions are integrated through the interpreter's globals:
1. Interpreter initializes with built-in functions
2. VM receives reference to interpreter's globals
3. `CallVar` opcode can call native functions directly
4. No code duplication or re-registration needed

## Implementation Status

### Completed ✅
- All OpCodes defined and documented
- Compiler for all implemented AST constructs:
  - Variable declarations and assignments
  - Arithmetic, logical, and comparison operations
  - Control flow (if/else, while loops, break/continue)
  - User-defined functions with parameters
  - Recursive function calls with proper call frames
  - Array and struct operations
  - Native function calls
  - Member and index access
- VM execution engine with stack-based operations
- Variable storage (locals and globals with proper scoping)
- Built-in function integration
- Comprehensive test suite (48 unit tests + 2 performance tests)

### Not Yet Implemented ⚠️
- Lambda/closure compilation (interpreter supports this, VM compiler does not yet)
- For loop compilation (interpreter supports this, VM compiler does not yet)

**Note**: These features work in the interpreter but need compiler implementation for VM support. The VM will continue to work for all other language features.

### Future Enhancements 🔮
- Constant folding optimization
- Dead code elimination
- Register allocation simulation
- JIT compilation possibilities
- Performance benchmarking

## Examples

### Simple Arithmetic
```lamina
10 + 20 * 2;
```

Bytecode:
```
PushConst(Int(10))
PushConst(Int(20))
PushConst(Int(2))
Mul
Add
Halt
```

### Variables and Conditionals
```lamina
let x = 10;
if (x > 5) {
    x + 10;
} else {
    x - 5;
}
```

Bytecode:
```
PushConst(Int(10))
PopVar("x")
PushVar("x")
PushConst(Int(5))
Gt
JumpIfFalse(8)
PushVar("x")
PushConst(Int(10))
Add
Jump(11)
PushVar("x")
PushConst(Int(5))
Sub
Halt
```

### Built-in Function Calls
```lamina
abs(-42);
```

Bytecode:
```
PushConst(Int(-42))
CallVar("abs", 1)
Halt
```

## Testing

The VM includes comprehensive tests:

### Unit Tests
- `test_vm_push_pop`: Basic stack operations
- `test_vm_arithmetic`: Arithmetic operations
- `test_vm_variables`: Variable storage and retrieval
- `test_vm_comparison`: Comparison operations
- `test_vm_array`: Array creation
- `test_vm_native_function_call`: Native function calls

### Integration Tests
- `test_compile_and_run_simple`: End-to-end simple expressions
- `test_compile_and_run_variables`: Variables and operations
- `test_compile_and_run_with_builtins`: Built-in function integration

## Performance Considerations

The VM is designed for:
- **Performance**: Optimized operation implementations without overhead
- **Simplicity**: Easy to understand and maintain
- **Compatibility**: Falls back to existing interpreter logic for complex types
- **Future optimization**: Foundation for JIT or AOT compilation

### Performance Optimizations Implemented

**Optimized VM Operations** (`src/value_ops.rs`):
- Direct operation implementations for common types (Int, BigInt, Bool)
- No interpreter instantiation overhead for basic operations
- Fast paths for arithmetic, comparison, and logical operations
- Falls back to full interpreter only for complex types (Irrational, Complex, etc.)

**Performance Results** (fib(30) benchmark):
- VM (release): **1.93 seconds**
- AST Interpreter (release): **3.45 seconds**
- **Speedup: 1.79x** (VM is nearly 2x faster than interpreter)

Debug mode (fib(20)):
- VM: ~90ms
- AST Interpreter: ~185ms
- **Speedup: 2.06x**

### Architecture Trade-offs

Current design choices:
- Stack-based (vs. register-based) design - simpler to implement and maintain
- No constant pooling yet - opportunity for future optimization
- Function calls use call frames with proper local variable management

## Future Directions

1. **Further Performance Optimizations**: 
   - Constant pooling to reduce memory allocations
   - Inline caching for property access
   - Type specialization for hot paths
2. **Optimization Pass**: Add compiler optimization phase (constant folding, dead code elimination)
3. **JIT Compilation**: Explore runtime compilation for hot loops
4. **WASM Integration**: Ensure VM works efficiently in WASM environment
5. **Profiling Tools**: Add built-in profiling support for identifying bottlenecks

## References

- x86_64 Instruction Set Reference: https://www.felixcloutier.com/x86/
- Virtual Machine Design Patterns
- Stack Machine Architecture
