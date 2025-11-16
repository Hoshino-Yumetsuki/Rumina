# Rumina VM Architecture

## Overview

The Rumina Virtual Machine is a stack-based bytecode interpreter with an instruction set inspired by x86_64 assembly. It implements a clean CISC (Complex Instruction Set Computing) design that balances simplicity with expressiveness.

## Design Philosophy

### 1. Stack-Based Execution Model
- All operations work on values pushed onto an evaluation stack
- Clean separation between data stack and call stack
- No explicit registers (variables act as named storage locations)

### 2. Polymorphic Operations
- All arithmetic and comparison operations are type-agnostic
- Type checking and conversions happen at runtime
- Single instruction can handle multiple types (Int, Float, Rational, Complex, etc.)

### 3. Orthogonal Instruction Set
- Each instruction does one thing well
- No redundant specialized variants
- Minimal instruction count with maximum expressiveness

### 4. x86_64-Inspired Design
- Instruction naming follows x86_64 conventions where applicable
- MOV-style operations for data movement
- CMP-style operations for comparisons
- JMP/CALL/RET for control flow

## Instruction Categories

### Data Movement (MOV family)
```
PushConst(value)         - Push immediate constant
PushConstPooled(idx)     - Push from constant pool (optimized)
PushVar(name)            - Load variable
PopVar(name)             - Store variable
Dup                      - Duplicate top of stack
Pop                      - Discard top of stack
```

### Arithmetic Operations (ADD/SUB/MUL/DIV family)
```
Add                      - Addition (polymorphic)
Sub                      - Subtraction (polymorphic)
Mul                      - Multiplication (polymorphic)
Div                      - Division (polymorphic)
Mod                      - Modulo (polymorphic)
Pow                      - Power (polymorphic)
Neg                      - Negation (unary)
Factorial                - Factorial (unary)
```

### Logical Operations (CMP/TEST family)
```
Not                      - Logical NOT
And                      - Logical AND
Or                       - Logical OR
Eq                       - Equal comparison
Neq                      - Not equal comparison
Gt                       - Greater than
Gte                      - Greater than or equal
Lt                       - Less than
Lte                      - Less than or equal
```

### Control Flow (JMP/CALL/RET family)
```
Jump(addr)               - Unconditional jump
JumpIfFalse(addr)        - Conditional jump (if TOS is false)
JumpIfTrue(addr)         - Conditional jump (if TOS is true)
CallVar(name, argc)      - Call function by name
Call(argc)               - Call function from stack
CallMethod(argc)         - Call method with self injection
Return                   - Return from function
Break                    - Break from loop
Continue                 - Continue loop
Halt                     - Stop execution
```

### Data Structures
```
MakeArray(size)          - Create array from stack values
MakeStruct(size)         - Create struct from key-value pairs
Index                    - Array/string indexing
Member(name)             - Struct member access
IndexAssign              - Assign to array element
MemberAssign(name)       - Assign to struct member
```

### Function Management
```
DefineFunc(info)         - Define a function
MakeLambda(info)         - Create a lambda/closure
```

### Type Conversion
```
ConvertType(type)        - Explicit type conversion
```

## Why No Specialized Instructions?

Earlier versions of the VM included specialized instructions like `AddInt`, `SubInt`, `MulInt`, `LtInt`, etc. for integer operations. These were removed because:

### 1. Code Duplication
- Each specialized instruction duplicated the logic of its generic counterpart
- Led to ~500 lines of redundant code
- Made maintenance harder

### 2. False Optimization
- Compiler couldn't accurately predict types at compile time
- Specialized instructions still needed fallback to generic operations
- Minimal performance benefit in practice

### 3. Wrong Level of Optimization
- Type-specific optimizations belong in:
  - JIT compilation layer (not yet implemented)
  - Ahead-of-time compilation
  - Profile-guided optimization
- Bytecode interpreter should focus on correctness and simplicity

### 4. Premature Optimization
- "Premature optimization is the root of all evil" - Donald Knuth
- The generic operations are already quite fast
- Real bottlenecks are elsewhere (e.g., memory allocation, complex number operations)

### 5. Complexity Without Benefit
- Compiler had complex type inference heuristics
- Bytecode optimizer had to handle both variants
- Tests had to cover both code paths
- All this complexity provided minimal real-world benefit

## Performance Considerations

### Current Optimizations
1. **Constant Pooling** - Deduplicates constants to reduce memory
2. **Inline Caching** - Caches member access results
3. **Pre-allocated Stacks** - Reduces allocation overhead
4. **FxHashMap** - Faster string hashing for variables

### Future Optimization Opportunities
1. **JIT Compilation** - Compile hot paths to native code
2. **Type Specialization** - JIT can generate type-specific code based on profiling
3. **Register Allocation** - Convert stack operations to register operations in JIT
4. **Inlining** - Inline small functions at JIT level

## Memory Model

### Stack Layout
```
[value_n]    <- Top of Stack (TOS)
[value_n-1]
[value_n-2]
...
[value_0]    <- Bottom
```

### Call Frame Structure
```rust
struct CallFrame {
    return_address: usize,     // Where to jump back after return
    base_pointer: usize,        // Stack position when function started
    function_name: String,      // For error reporting
    locals: FxHashMap<...>,    // Local variables
}
```

### Variable Resolution
1. Check local variables (current function scope)
2. Check global variables (module level)
3. Throw error if not found

## Bytecode Format

The bytecode is serialized to a human-readable text format (.rmc):

```
RUMINA-BYTECODE-V1
CONSTANTS: 2
CONST[0]: Int(10)
CONST[1]: Int(20)

INSTRUCTIONS:
0000 [L1] PushConstPooled(0)
0001 [L2] PushConstPooled(1)
0002 [L3] Add
0003 [L4] PopVar(result)
0004 [L5] Halt
```

### Format Features
- Human-readable for debugging
- Line number information preserved
- Constant pool for optimization
- Easily parseable for tools

## Error Handling

### Runtime Errors
- Stack underflow
- Type mismatches (e.g., can't add string to number without conversion)
- Undefined variables
- Division by zero
- Array index out of bounds
- Maximum recursion depth exceeded

### Error Context
- Current instruction address
- Line number (when available)
- Function call stack
- Variable state

## Comparison with Other VMs

### vs. Python VM
- **Similar**: Stack-based, dynamic typing, bytecode format
- **Different**: Simpler instruction set, no peephole optimizer, cleaner constant pooling

### vs. JVM
- **Similar**: Constant pool, structured bytecode format
- **Different**: Stack-based only (no registers), dynamic typing, simpler verification

### vs. V8 (JavaScript)
- **Similar**: Inline caching for member access, constant pooling
- **Different**: No JIT (yet), no hidden classes, simpler type system

### vs. Lua VM
- **Similar**: Small instruction set, register-like local variables, closure support
- **Different**: Stack-based (Lua VM is register-based), different type system

## Design Trade-offs

### Stack-Based vs Register-Based
**Choice**: Stack-based
- **Pros**: Simpler instruction encoding, easier to compile to
- **Cons**: More instructions per operation, harder to optimize
- **Rationale**: Simplicity and maintainability over raw performance

### Dynamic Typing vs Static Typing
**Choice**: Dynamic typing
- **Pros**: Flexibility, easier language design, simpler compiler
- **Cons**: Runtime overhead, harder to optimize
- **Rationale**: Matches Lamina language semantics

### CISC vs RISC
**Choice**: CISC (with orthogonal operations)
- **Pros**: Fewer instructions overall, expressive, matches x86_64 style
- **Cons**: Complex instructions can be harder to implement
- **Rationale**: Better match for high-level language constructs

## Future Enhancements

### Planned
1. **Bytecode Verification** - Verify bytecode before execution
2. **Profiling Support** - Track hot paths and types
3. **Better Debugging** - Breakpoints, single-stepping, variable inspection

### Under Consideration
1. **JIT Compilation** - Compile hot code to native
2. **Parallel Execution** - Multi-threaded execution for pure functions
3. **Incremental GC** - Better memory management for long-running programs
4. **Module System** - Better support for large codebases

## Conclusion

The Rumina VM prioritizes:
1. **Correctness** - Operations work correctly for all types
2. **Simplicity** - Easy to understand and maintain
3. **Expressiveness** - High-level operations for complex types
4. **Extensibility** - Easy to add new operations and types

Performance optimizations are applied judiciously, only where they provide clear benefits without sacrificing the above principles.
