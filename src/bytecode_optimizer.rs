/// Bytecode-level peephole optimizer
///
/// This module implements peephole optimizations on bytecode to:
/// - Eliminate redundant operations
/// - Merge adjacent operations
/// - Remove dead code
/// - Optimize jump patterns

use crate::vm::{ByteCode, OpCode};

/// Bytecode optimizer that performs peephole optimizations
pub struct BytecodeOptimizer {
    /// Track if any optimizations were applied
    modified: bool,
}

impl BytecodeOptimizer {
    pub fn new() -> Self {
        BytecodeOptimizer { modified: false }
    }

    /// Optimize bytecode by applying peephole optimizations
    /// Returns true if any optimizations were applied
    pub fn optimize(&mut self, bytecode: &mut ByteCode) -> bool {
        self.modified = false;

        // Apply multiple passes until no more optimizations can be made
        loop {
            let before_modified = self.modified;
            
            self.eliminate_dead_pushpop(bytecode);
            self.eliminate_redundant_dup(bytecode);
            self.merge_constant_operations(bytecode);
            self.optimize_jump_chains(bytecode);
            self.eliminate_nop_patterns(bytecode);
            
            // If no changes were made in this iteration, we're done
            if self.modified == before_modified {
                break;
            }
        }

        self.modified
    }

    /// Eliminate push followed by immediate pop patterns
    /// Pattern: PushConst(x) -> Pop  ==>  (removed)
    fn eliminate_dead_pushpop(&mut self, bytecode: &mut ByteCode) {
        let mut i = 0;
        let mut removals = Vec::new();

        while i + 1 < bytecode.instructions.len() {
            let current = &bytecode.instructions[i];
            let next = &bytecode.instructions[i + 1];

            match (current, next) {
                // Push followed by Pop - dead code
                (OpCode::PushConst(_) | OpCode::PushConstPooled(_), OpCode::Pop) => {
                    removals.push(i);
                    removals.push(i + 1);
                    self.modified = true;
                    i += 2;
                }
                _ => {
                    i += 1;
                }
            }
        }

        // Remove marked instructions (in reverse to maintain indices)
        for &idx in removals.iter().rev() {
            bytecode.instructions.remove(idx);
            bytecode.line_numbers.remove(idx);
        }
    }

    /// Eliminate redundant duplicate operations
    /// Pattern: PushVar(x) -> Dup -> PopVar(x)  ==>  PushVar(x)
    fn eliminate_redundant_dup(&mut self, bytecode: &mut ByteCode) {
        let mut i = 0;
        let mut removals = Vec::new();

        while i + 2 < bytecode.instructions.len() {
            let first = &bytecode.instructions[i];
            let second = &bytecode.instructions[i + 1];
            let third = &bytecode.instructions[i + 2];

            match (first, second, third) {
                // PushVar(x) -> Dup -> PopVar(x) leaves just PushVar(x) on stack
                (OpCode::PushVar(name1), OpCode::Dup, OpCode::PopVar(name2)) if name1 == name2 => {
                    removals.push(i + 1); // Remove Dup
                    removals.push(i + 2); // Remove PopVar
                    self.modified = true;
                    i += 3;
                }
                _ => {
                    i += 1;
                }
            }
        }

        for &idx in removals.iter().rev() {
            bytecode.instructions.remove(idx);
            bytecode.line_numbers.remove(idx);
        }
    }

    /// Merge constant operations at bytecode level
    /// Pattern: PushConst(a) -> PushConst(b) -> Add  ==>  PushConst(a+b)
    fn merge_constant_operations(&mut self, bytecode: &mut ByteCode) {
        let mut i = 0;
        let mut changes = Vec::new(); // (index, new_opcode, remove_count)

        while i + 2 < bytecode.instructions.len() {
            let first = &bytecode.instructions[i];
            let second = &bytecode.instructions[i + 1];
            let third = &bytecode.instructions[i + 2];

            // Try to constant-fold integer operations
            match (first, second, third) {
                (
                    OpCode::PushConstPooled(idx1),
                    OpCode::PushConstPooled(idx2),
                    OpCode::Add | OpCode::AddInt | OpCode::Sub | OpCode::SubInt | 
                    OpCode::Mul | OpCode::MulInt,
                ) => {
                    // Check if both constants are integers
                    if let (Some(val1), Some(val2)) = (
                        bytecode.constants.get(*idx1),
                        bytecode.constants.get(*idx2),
                    ) {
                        use crate::value::Value;
                        
                        if let (Value::Int(a), Value::Int(b)) = (val1, val2) {
                            let result = match third {
                                OpCode::Add | OpCode::AddInt => a + b,
                                OpCode::Sub | OpCode::SubInt => a - b,
                                OpCode::Mul | OpCode::MulInt => a * b,
                                _ => {
                                    i += 1;
                                    continue;
                                }
                            };

                            changes.push((i, OpCode::PushConst(Value::Int(result)), 3));
                            self.modified = true;
                            i += 3;
                            continue;
                        }
                    }
                }
                _ => {}
            }

            i += 1;
        }

        // Apply changes (in reverse to maintain indices)
        for (idx, new_op, remove_count) in changes.into_iter().rev() {
            // Replace first instruction with merged result
            bytecode.instructions[idx] = new_op;
            
            // Remove the next instructions
            for _ in 1..remove_count {
                bytecode.instructions.remove(idx + 1);
                bytecode.line_numbers.remove(idx + 1);
            }
        }
    }

    /// Optimize jump chains: Jump to Jump -> Direct Jump
    /// Pattern: Jump(A) where A contains Jump(B)  ==>  Jump(B)
    fn optimize_jump_chains(&mut self, bytecode: &mut ByteCode) {
        for i in 0..bytecode.instructions.len() {
            match &bytecode.instructions[i] {
                OpCode::Jump(target) | OpCode::JumpIfFalse(target) | OpCode::JumpIfTrue(target) => {
                    // Follow the jump chain
                    let mut final_target = *target;
                    let mut visited = std::collections::HashSet::new();
                    
                    while final_target < bytecode.instructions.len() && visited.insert(final_target) {
                        match &bytecode.instructions[final_target] {
                            OpCode::Jump(next_target) => {
                                final_target = *next_target;
                                self.modified = true;
                            }
                            _ => break,
                        }
                    }

                    // Update the instruction with the final target
                    if final_target != *target {
                        match &mut bytecode.instructions[i] {
                            OpCode::Jump(addr) => *addr = final_target,
                            OpCode::JumpIfFalse(addr) => *addr = final_target,
                            OpCode::JumpIfTrue(addr) => *addr = final_target,
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
    }

    /// Eliminate no-op patterns
    /// Pattern: PushVar(x) -> PopVar(x)  ==>  (removed if not used)
    fn eliminate_nop_patterns(&mut self, bytecode: &mut ByteCode) {
        let mut i = 0;
        let mut removals = Vec::new();

        while i + 1 < bytecode.instructions.len() {
            let current = &bytecode.instructions[i];
            let next = &bytecode.instructions[i + 1];

            match (current, next) {
                // Push then immediately pop same variable - no effect
                (OpCode::PushVar(name1), OpCode::PopVar(name2)) if name1 == name2 => {
                    removals.push(i);
                    removals.push(i + 1);
                    self.modified = true;
                    i += 2;
                }
                _ => {
                    i += 1;
                }
            }
        }

        for &idx in removals.iter().rev() {
            bytecode.instructions.remove(idx);
            bytecode.line_numbers.remove(idx);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[test]
    fn test_eliminate_dead_pushpop() {
        let mut bytecode = ByteCode::new();
        bytecode.emit(OpCode::PushConst(Value::Int(42)), None);
        bytecode.emit(OpCode::Pop, None);
        bytecode.emit(OpCode::PushConst(Value::Int(10)), None);
        bytecode.emit(OpCode::Halt, None);

        let mut optimizer = BytecodeOptimizer::new();
        optimizer.optimize(&mut bytecode);

        // The PushConst(42) -> Pop should be eliminated
        assert_eq!(bytecode.instructions.len(), 2); // PushConst(10) and Halt
    }

    #[test]
    fn test_merge_constant_add() {
        let mut bytecode = ByteCode::new();
        
        let idx1 = bytecode.add_constant(Value::Int(10));
        let idx2 = bytecode.add_constant(Value::Int(20));
        
        bytecode.emit(OpCode::PushConstPooled(idx1), None);
        bytecode.emit(OpCode::PushConstPooled(idx2), None);
        bytecode.emit(OpCode::AddInt, None);
        bytecode.emit(OpCode::Halt, None);

        let mut optimizer = BytecodeOptimizer::new();
        optimizer.optimize(&mut bytecode);

        // Should merge into PushConst(30) and Halt
        assert_eq!(bytecode.instructions.len(), 2);
        match &bytecode.instructions[0] {
            OpCode::PushConst(Value::Int(n)) => assert_eq!(*n, 30),
            _ => panic!("Expected PushConst(30)"),
        }
    }

    #[test]
    fn test_optimize_jump_chains() {
        let mut bytecode = ByteCode::new();
        
        // Create jump chain: 0->2, 2->4
        bytecode.emit(OpCode::Jump(2), None);     // 0
        bytecode.emit(OpCode::Halt, None);        // 1
        bytecode.emit(OpCode::Jump(4), None);     // 2
        bytecode.emit(OpCode::Halt, None);        // 3
        bytecode.emit(OpCode::PushConst(Value::Int(42)), None); // 4
        bytecode.emit(OpCode::Halt, None);        // 5

        let mut optimizer = BytecodeOptimizer::new();
        optimizer.optimize(&mut bytecode);

        // Jump at 0 should now point directly to 4
        match &bytecode.instructions[0] {
            OpCode::Jump(addr) => assert_eq!(*addr, 4),
            _ => panic!("Expected Jump(4)"),
        }
    }

    #[test]
    fn test_eliminate_nop_pushvar_popvar() {
        let mut bytecode = ByteCode::new();
        
        bytecode.emit(OpCode::PushVar("x".to_string()), None);
        bytecode.emit(OpCode::PopVar("x".to_string()), None);
        bytecode.emit(OpCode::PushConst(Value::Int(42)), None);
        bytecode.emit(OpCode::Halt, None);

        let mut optimizer = BytecodeOptimizer::new();
        optimizer.optimize(&mut bytecode);

        // The PushVar(x) -> PopVar(x) should be eliminated
        assert_eq!(bytecode.instructions.len(), 2); // PushConst and Halt
    }

    #[test]
    fn test_multiple_pass_optimization() {
        let mut bytecode = ByteCode::new();
        
        // Create a pattern that requires multiple passes
        bytecode.emit(OpCode::PushConst(Value::Int(1)), None);
        bytecode.emit(OpCode::Pop, None);
        bytecode.emit(OpCode::PushVar("x".to_string()), None);
        bytecode.emit(OpCode::PopVar("x".to_string()), None);
        bytecode.emit(OpCode::Halt, None);

        let mut optimizer = BytecodeOptimizer::new();
        optimizer.optimize(&mut bytecode);

        // Both patterns should be eliminated
        assert_eq!(bytecode.instructions.len(), 1); // Only Halt remains
    }
}
