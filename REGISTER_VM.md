# x86_64-Style Register-Based Virtual Machine

## Overview

Rumina VM has been enhanced with x86_64-style register-based instructions while maintaining full backward compatibility with the original stack-based architecture. This hybrid approach provides:

- **Register-based operations** for modern, efficient bytecode execution
- **x86_64-inspired design** with familiar register names and calling conventions
- **Full backward compatibility** with existing stack-based bytecode

## Register Architecture

The VM provides 14 general-purpose registers, inspired by x86_64 architecture:

| Register | Purpose | x86_64 Convention |
|----------|---------|-------------------|
| RAX | Accumulator, Return Value | Return value register |
| RBX | Base | General purpose |
| RCX | Counter | General purpose |
| RDX | Data | General purpose |
| RSI | Source Index | General purpose |
| RDI | Destination Index | General purpose |
| R8-R15 | General Purpose | General purpose |

### Return Value Convention

Following x86_64 calling conventions, the VM returns values from the **RAX** register. When a program completes:
1. The value in RAX is returned as the result
2. If RAX is null, the VM falls back to the stack (for backward compatibility)

## Instruction Set

### Data Movement Instructions (MOV family)

```
MovConst(dst, value)           ; MOV dst, immediate_value
MovConstPooled(dst, index)     ; MOV dst, [const_pool + index]
MovVar(dst, name)              ; MOV dst, variable
MovToVar(name, src)            ; MOV variable, src
MovReg(dst, src)               ; MOV dst, src
```

**Example:**
```rust
// MOV RAX, 42
bytecode.emit(OpCode::MovConst(Register::RAX, Value::Int(42)), None);

// MOV RBX, RAX
bytecode.emit(OpCode::MovReg(Register::RBX, Register::RAX), None);
```

### Arithmetic Instructions (Three-Operand Format)

All arithmetic operations use three-operand format: `OP dst, src1, src2`

```
AddReg(dst, src1, src2)        ; dst = src1 + src2
SubReg(dst, src1, src2)        ; dst = src1 - src2
MulReg(dst, src1, src2)        ; dst = src1 * src2
DivReg(dst, src1, src2)        ; dst = src1 / src2
ModReg(dst, src1, src2)        ; dst = src1 % src2
PowReg(dst, src1, src2)        ; dst = src1 ^ src2
```

**Example:**
```rust
// ADD RCX, RAX, RBX  ; RCX = RAX + RBX
bytecode.emit(OpCode::AddReg(Register::RCX, Register::RAX, Register::RBX), None);

// MUL RDX, RCX, RAX  ; RDX = RCX * RAX
bytecode.emit(OpCode::MulReg(Register::RDX, Register::RCX, Register::RAX), None);
```

### Unary Operations (Two-Operand Format)

```
NegReg(dst, src)               ; dst = -src
FactorialReg(dst, src)         ; dst = src!
NotReg(dst, src)               ; dst = !src
```

**Example:**
```rust
// NEG RBX, RAX  ; RBX = -RAX
bytecode.emit(OpCode::NegReg(Register::RBX, Register::RAX), None);
```

### Logical Instructions

```
AndReg(dst, src1, src2)        ; dst = src1 && src2
OrReg(dst, src1, src2)         ; dst = src1 || src2
NotReg(dst, src)               ; dst = !src
```

**Example:**
```rust
// AND RCX, RAX, RBX  ; RCX = RAX && RBX
bytecode.emit(OpCode::AndReg(Register::RCX, Register::RAX, Register::RBX), None);
```

### Comparison Instructions

```
EqReg(dst, src1, src2)         ; dst = (src1 == src2)
NeqReg(dst, src1, src2)        ; dst = (src1 != src2)
GtReg(dst, src1, src2)         ; dst = (src1 > src2)
GteReg(dst, src1, src2)        ; dst = (src1 >= src2)
LtReg(dst, src1, src2)         ; dst = (src1 < src2)
LteReg(dst, src1, src2)        ; dst = (src1 <= src2)
```

**Example:**
```rust
// GT RDX, RAX, RBX  ; RDX = (RAX > RBX)
bytecode.emit(OpCode::GtReg(Register::RDX, Register::RAX, Register::RBX), None);
```

## Example Programs

### Simple Arithmetic

Computing `(10 + 5) * 2`:

```rust
let mut bytecode = ByteCode::new();

// MOV RAX, 10
bytecode.emit(OpCode::MovConst(Register::RAX, Value::Int(10)), None);
// MOV RBX, 5
bytecode.emit(OpCode::MovConst(Register::RBX, Value::Int(5)), None);
// ADD RCX, RAX, RBX  ; RCX = 15
bytecode.emit(OpCode::AddReg(Register::RCX, Register::RAX, Register::RBX), None);
// MOV RDX, 2
bytecode.emit(OpCode::MovConst(Register::RDX, Value::Int(2)), None);
// MUL RAX, RCX, RDX  ; RAX = 30 (return value)
bytecode.emit(OpCode::MulReg(Register::RAX, Register::RCX, Register::RDX), None);
bytecode.emit(OpCode::Halt, None);

// Result: 30
```

### Using Variables

```rust
let mut bytecode = ByteCode::new();

// MOV RAX, 100
bytecode.emit(OpCode::MovConst(Register::RAX, Value::Int(100)), None);
// MOV x, RAX  ; Store to variable
bytecode.emit(OpCode::MovToVar("x".to_string(), Register::RAX), None);
// MOV RBX, x  ; Load from variable
bytecode.emit(OpCode::MovVar(Register::RBX, "x".to_string()), None);
// MOV RCX, 50
bytecode.emit(OpCode::MovConst(Register::RCX, Value::Int(50)), None);
// ADD RAX, RBX, RCX  ; RAX = 150
bytecode.emit(OpCode::AddReg(Register::RAX, Register::RBX, Register::RCX), None);
bytecode.emit(OpCode::Halt, None);

// Result: 150
```

### Comparison and Logic

```rust
let mut bytecode = ByteCode::new();

// MOV RAX, 10
bytecode.emit(OpCode::MovConst(Register::RAX, Value::Int(10)), None);
// MOV RBX, 5
bytecode.emit(OpCode::MovConst(Register::RBX, Value::Int(5)), None);
// GT RCX, RAX, RBX  ; RCX = true (10 > 5)
bytecode.emit(OpCode::GtReg(Register::RCX, Register::RAX, Register::RBX), None);
// MOV RAX, RCX  ; Return comparison result
bytecode.emit(OpCode::MovReg(Register::RAX, Register::RCX), None);
bytecode.emit(OpCode::Halt, None);

// Result: true
```

## Bytecode Serialization Format

Register-based bytecode serializes to a readable text format:

```
RUMINA-BYTECODE-V1
CONSTANTS: 0

INSTRUCTIONS:
0000 [L1] MovConst(RAX, Int(10))
0001 [L2] MovConst(RBX, Int(5))
0002 [L3] AddReg(RCX, RAX, RBX)
0003 [L4] MovConst(RDX, Int(2))
0004 [L5] MulReg(RAX, RCX, RDX)
0005 [L6] Halt
```

The format includes:
- Register names (RAX, RBX, etc.)
- Instruction mnemonics
- Line numbers for debugging
- Constant pool support

## Backward Compatibility

All existing stack-based instructions continue to work:
- `PushConst`, `PushVar`, `PopVar`
- `Add`, `Sub`, `Mul`, `Div`
- `Jump`, `JumpIfFalse`, `JumpIfTrue`
- etc.

Programs can mix stack-based and register-based instructions. The VM maintains both:
- A **register file** for register operations
- A **stack** for stack operations and function calls

## Performance Benefits

Register-based instructions provide several advantages:

1. **Fewer Memory Accesses**: Values stay in registers, reducing stack operations
2. **Explicit Data Flow**: Clear source and destination operands
3. **Better Optimization**: Register allocation enables more compiler optimizations
4. **Familiar Model**: Matches real CPU architectures (x86_64, ARM64)

## Design Principles

The register-based architecture follows these principles:

1. **x86_64 Inspiration**: Register names and conventions match x86_64
2. **Three-Operand Format**: Modern RISC-like instruction format (dst, src1, src2)
3. **Backward Compatible**: Existing bytecode continues to work
4. **Clear Semantics**: Each instruction has well-defined behavior
5. **Debuggable**: Readable bytecode format for inspection

## Future Enhancements

Potential future improvements include:

- Compiler optimizations to emit register-based bytecode
- Register allocation algorithms
- Peephole optimization for register operations
- Advanced calling conventions
- SSA (Static Single Assignment) form support

## See Also

- `examples/register_vm_demo.rs` - Working example of register-based VM
- `tests/register_vm_test.rs` - Comprehensive tests for register operations
- `src/vm.rs` - VM implementation with register support
