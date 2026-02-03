/// JIT (Just-In-Time) Compiler for Rumina VM
///
/// This module implements a JIT compiler that detects hot paths in bytecode
/// and compiles them to optimized native execution paths.
use crate::error::RuminaError;
use crate::vm::{ByteCode, OpCode};
use rustc_hash::FxHashMap;
use std::sync::Arc;

/// Threshold for considering a bytecode region as "hot"
const HOT_THRESHOLD: u32 = 100;

/// Maximum size of a hot trace (in instructions)
const MAX_TRACE_SIZE: usize = 1000;

/// JIT compiler state
pub struct JITCompiler {
    /// Execution counts for each instruction address
    execution_counts: FxHashMap<usize, u32>,
    
    /// Compiled traces: maps start address to compiled function
    compiled_traces: FxHashMap<usize, Arc<CompiledTrace>>,
    
    /// Enable/disable JIT compilation
    enabled: bool,
}

impl JITCompiler {
    /// Create a new JIT compiler
    pub fn new() -> Self {
        JITCompiler {
            execution_counts: FxHashMap::default(),
            compiled_traces: FxHashMap::default(),
            enabled: true,
        }
    }

    /// Record execution of an instruction
    #[inline]
    pub fn record_execution(&mut self, ip: usize) -> bool {
        if !self.enabled {
            return false;
        }

        let count = self.execution_counts.entry(ip).or_insert(0);
        *count += 1;
        
        // Check if this location has become hot
        *count >= HOT_THRESHOLD && !self.compiled_traces.contains_key(&ip)
    }

    /// Check if a trace exists for this address
    #[inline]
    pub fn has_trace(&self, ip: usize) -> bool {
        self.compiled_traces.contains_key(&ip)
    }

    /// Get a compiled trace
    #[inline]
    pub fn get_trace(&self, ip: usize) -> Option<&Arc<CompiledTrace>> {
        self.compiled_traces.get(&ip)
    }

    /// Compile a hot trace starting at the given address
    pub fn compile_trace(
        &mut self,
        bytecode: &ByteCode,
        start_ip: usize,
    ) -> Result<(), RuminaError> {
        if !self.enabled {
            return Ok(());
        }

        // Detect loop or linear trace
        let trace = self.detect_trace(bytecode, start_ip)?;
        
        // Compile the trace
        let compiled = self.optimize_trace(trace)?;
        
        // Store compiled trace
        self.compiled_traces.insert(start_ip, Arc::new(compiled));
        
        Ok(())
    }

    /// Detect a trace (loop or linear sequence) starting from the given IP
    fn detect_trace(&self, bytecode: &ByteCode, start_ip: usize) -> Result<Trace, RuminaError> {
        let mut instructions = Vec::new();
        let mut ip = start_ip;
        let mut visited = FxHashMap::default();

        // Collect instructions until we hit a loop back-edge or max size
        while ip < bytecode.instructions.len() && instructions.len() < MAX_TRACE_SIZE {
            // Check for loop (back-edge)
            if visited.contains_key(&ip) && ip == start_ip {
                return Ok(Trace {
                    start_ip,
                    end_ip: ip,
                    instructions,
                    is_loop: true,
                });
            }
            
            visited.insert(ip, instructions.len());
            
            let op = &bytecode.instructions[ip];
            instructions.push((ip, op.clone()));

            // Stop at certain control flow instructions
            match op {
                OpCode::Jump(target) => {
                    // If jumping back to start, it's a loop
                    if *target == start_ip {
                        return Ok(Trace {
                            start_ip,
                            end_ip: ip + 1,
                            instructions,
                            is_loop: true,
                        });
                    }
                    // Stop at forward jumps for now
                    break;
                }
                OpCode::JumpIfFalse(_) | OpCode::JumpIfTrue(_) => {
                    // Stop at conditional branches - don't JIT compile control flow yet
                    // Remove the conditional jump from the trace
                    instructions.pop();
                    break;
                }
                OpCode::Return | OpCode::Halt => {
                    // Stop at return/halt
                    break;
                }
                OpCode::Call(_) | OpCode::CallVar(_, _) | OpCode::CallMethod(_) => {
                    // Stop at function calls for now
                    instructions.pop();
                    break;
                }
                _ => {
                    ip += 1;
                }
            }
        }

        Ok(Trace {
            start_ip,
            end_ip: ip,
            instructions,
            is_loop: false,
        })
    }

    /// Optimize a trace using various techniques
    fn optimize_trace(&self, trace: Trace) -> Result<CompiledTrace, RuminaError> {
        let mut optimized_ops = Vec::new();

        let mut i = 0;
        while i < trace.instructions.len() {
            let (_ip, op) = &trace.instructions[i];

            // Pattern matching for superinstructions
            if i + 2 < trace.instructions.len() {
                // Pattern: PushVar + PushVar + Add -> AddVarVar
                if matches!(op, OpCode::PushVar(_)) {
                    if let (OpCode::PushVar(v1), OpCode::PushVar(v2), OpCode::Add) = (
                        &trace.instructions[i].1,
                        &trace.instructions[i + 1].1,
                        &trace.instructions[i + 2].1,
                    ) {
                        optimized_ops.push(OptimizedOp::AddVarVar(v1.clone(), v2.clone()));
                        i += 3;
                        continue;
                    }
                }

                // Pattern: PushVar + PushConstPooled + Add -> AddVarConst
                if let (OpCode::PushVar(var), OpCode::PushConstPooled(idx), OpCode::Add) = (
                    &trace.instructions[i].1,
                    &trace.instructions[i + 1].1,
                    &trace.instructions[i + 2].1,
                ) {
                    optimized_ops.push(OptimizedOp::AddVarConst(var.clone(), *idx));
                    i += 3;
                    continue;
                }

                // Pattern: PushVar + PushConstPooled + Mul -> MulVarConst
                if let (OpCode::PushVar(var), OpCode::PushConstPooled(idx), OpCode::Mul) = (
                    &trace.instructions[i].1,
                    &trace.instructions[i + 1].1,
                    &trace.instructions[i + 2].1,
                ) {
                    optimized_ops.push(OptimizedOp::MulVarConst(var.clone(), *idx));
                    i += 3;
                    continue;
                }
            }

            if i + 1 < trace.instructions.len() {
                // Pattern: PushVar + PopVar -> CopyVar
                if let (OpCode::PushVar(from), OpCode::PopVar(to)) = (
                    &trace.instructions[i].1,
                    &trace.instructions[i + 1].1,
                ) {
                    if from != to {
                        optimized_ops.push(OptimizedOp::CopyVar(from.clone(), to.clone()));
                        i += 2;
                        continue;
                    }
                }
            }

            // No optimization, keep original
            optimized_ops.push(OptimizedOp::Original(op.clone()));
            i += 1;
        }

        Ok(CompiledTrace {
            start_ip: trace.start_ip,
            end_ip: trace.end_ip,
            is_loop: trace.is_loop,
            optimized_ops,
        })
    }

    /// Get statistics about JIT compilation
    pub fn stats(&self) -> JITStats {
        JITStats {
            hot_spots: self.execution_counts.len(),
            compiled_traces: self.compiled_traces.len(),
            total_executions: self.execution_counts.values().sum(),
        }
    }

    /// Reset JIT state
    pub fn reset(&mut self) {
        self.execution_counts.clear();
        self.compiled_traces.clear();
    }

    /// Enable or disable JIT
    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }
}

/// A detected trace in the bytecode
#[derive(Debug)]
struct Trace {
    start_ip: usize,
    end_ip: usize,
    instructions: Vec<(usize, OpCode)>,
    is_loop: bool,
}

/// A compiled and optimized trace
#[derive(Debug, Clone)]
pub struct CompiledTrace {
    pub start_ip: usize,
    pub end_ip: usize,
    pub is_loop: bool,
    pub optimized_ops: Vec<OptimizedOp>,
}

/// Optimized operations (superinstructions)
#[derive(Debug, Clone)]
pub enum OptimizedOp {
    /// Original unoptimized operation
    Original(OpCode),
    
    /// Add two variables directly (PushVar + PushVar + Add)
    AddVarVar(String, String),
    
    /// Add variable and constant (PushVar + PushConst + Add)
    AddVarConst(String, usize),
    
    /// Multiply variable and constant (PushVar + PushConst + Mul)
    MulVarConst(String, usize),
    
    /// Copy variable (PushVar + PopVar)
    CopyVar(String, String),
    
    /// Increment variable by 1 (specialized)
    IncrementVar(String),
    
    /// Decrement variable by 1 (specialized)
    DecrementVar(String),
}

/// JIT compilation statistics
#[derive(Debug, Clone)]
pub struct JITStats {
    pub hot_spots: usize,
    pub compiled_traces: usize,
    pub total_executions: u32,
}

impl OptimizedOp {
    /// Check if this is an original (unoptimized) operation
    pub fn is_original(&self) -> bool {
        matches!(self, OptimizedOp::Original(_))
    }

    /// Get the underlying OpCode if this is an original operation
    pub fn as_original(&self) -> Option<&OpCode> {
        match self {
            OptimizedOp::Original(op) => Some(op),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_jit_hotspot_detection() {
        let mut jit = JITCompiler::new();
        
        // Execute instruction 100 times to make it hot
        for _ in 0..HOT_THRESHOLD-1 {
            assert!(!jit.record_execution(0));
        }
        
        // Execution at hot threshold should trigger hotspot detection
        assert!(jit.record_execution(0));
        
        // Mark as having a trace (simulate compilation)
        let mut bytecode = ByteCode::new();
        bytecode.emit(OpCode::Halt, None);
        jit.compile_trace(&bytecode, 0).unwrap();
        
        // Further executions don't trigger (trace exists)
        assert!(!jit.record_execution(0));
    }

    #[test]
    fn test_jit_trace_optimization() {
        let mut jit = JITCompiler::new();
        let mut bytecode = ByteCode::new();
        
        // Create a simple trace: PushVar + PushVar + Add
        bytecode.emit(OpCode::PushVar("x".to_string()), Some(1));
        bytecode.emit(OpCode::PushVar("y".to_string()), Some(2));
        bytecode.emit(OpCode::Add, Some(3));
        bytecode.emit(OpCode::Halt, Some(4));
        
        // Compile the trace
        jit.compile_trace(&bytecode, 0).unwrap();
        
        // Verify trace was compiled
        assert!(jit.has_trace(0));
        
        let trace = jit.get_trace(0).unwrap();
        assert_eq!(trace.start_ip, 0);
        
        // Check that superinstruction was created
        assert!(trace.optimized_ops.iter().any(|op| {
            matches!(op, OptimizedOp::AddVarVar(_, _))
        }));
    }
}
