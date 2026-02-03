# JIT Optimization and VM Performance Improvements

This document describes the Just-In-Time (JIT) compiler and VM optimizations added to Rumina.

## Overview

The JIT compiler automatically detects "hot paths" (frequently executed code) in your programs and optimizes them for better performance. This provides significant speedups for loops and arithmetic-heavy code without requiring any changes to your programs.

## Key Features

### 1. Hot Path Detection
- Automatically tracks execution frequency of each instruction
- When an instruction is executed 100+ times, it's marked as a "hot spot"
- Hot spots are compiled into optimized traces

### 2. Superinstructions
The JIT combines multiple VM instructions into single optimized operations:

- **AddVarVar**: Combines `PushVar + PushVar + Add` → Direct addition of two variables
- **AddVarConst**: Combines `PushVar + PushConst + Add` → Direct addition of variable and constant
- **MulVarConst**: Combines `PushVar + PushConst + Mul` → Direct multiplication
- **CopyVar**: Combines `PushVar + PopVar` → Direct variable copy

### 3. Fast Path for Integer Operations
Special optimized execution paths for common integer operations:
- Addition, subtraction, multiplication, modulo
- Bypasses generic value handling when both operands are integers
- Inline integer arithmetic without function call overhead

## Performance Improvements

Expected speedups (varies by workload):
- **Simple loops**: 1.2-1.5x faster
- **Arithmetic-heavy code**: 1.3-2x faster
- **Nested loops**: 1.5-2.5x faster
- **Integer operations**: 1.5-3x faster

## Usage

JIT is enabled by default. Just run your code normally:

```bash
rumina-cli program.lm
```

See documentation for programmatic control via Rust API.
