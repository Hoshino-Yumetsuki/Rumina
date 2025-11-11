/// Bytecode compiler for Rumina
///
/// This module compiles AST to bytecode instructions for the VM.

use crate::ast::*;
use crate::error::RuminaError;
use crate::value::Value;
use crate::vm::{ByteCode, OpCode};
use std::collections::HashMap;

/// Symbol table for variable resolution
#[derive(Debug, Clone)]
struct SymbolTable {
    /// Scopes stack (innermost scope is last)
    scopes: Vec<HashMap<String, SymbolInfo>>,
}

#[derive(Debug, Clone)]
struct SymbolInfo {
    /// Variable name
    name: String,
    
    /// Scope depth
    depth: usize,
}

impl SymbolTable {
    fn new() -> Self {
        SymbolTable {
            scopes: vec![HashMap::new()], // Global scope
        }
    }
    
    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    
    fn exit_scope(&mut self) {
        self.scopes.pop();
    }
    
    fn define(&mut self, name: String) {
        let depth = self.scopes.len() - 1;
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.clone(), SymbolInfo { name, depth });
        }
    }
    
    fn resolve(&self, name: &str) -> Option<&SymbolInfo> {
        // Search from innermost to outermost scope
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(info);
            }
        }
        None
    }
}

/// Loop context for break/continue
#[derive(Debug, Clone)]
struct LoopContext {
    /// Address to jump to for continue
    continue_target: usize,
    
    /// Addresses to patch for break statements
    break_patches: Vec<usize>,
}

/// Bytecode compiler
pub struct Compiler {
    /// Output bytecode
    bytecode: ByteCode,
    
    /// Symbol table
    symbols: SymbolTable,
    
    /// Loop context stack
    loop_stack: Vec<LoopContext>,
    
    /// Current line number (for debugging)
    current_line: Option<usize>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            bytecode: ByteCode::new(),
            symbols: SymbolTable::new(),
            loop_stack: Vec::new(),
            current_line: None,
        }
    }
    
    /// Compile a list of statements
    pub fn compile(&mut self, statements: Vec<Stmt>) -> Result<ByteCode, RuminaError> {
        for stmt in statements {
            self.compile_stmt(&stmt)?;
        }
        
        // Add halt at the end
        self.emit(OpCode::Halt);
        
        Ok(self.bytecode.clone())
    }
    
    /// Emit an instruction
    fn emit(&mut self, op: OpCode) {
        self.bytecode.emit(op, self.current_line);
    }
    
    /// Get current instruction address
    fn current_address(&self) -> usize {
        self.bytecode.current_address()
    }
    
    /// Emit a jump placeholder and return its address for patching
    fn emit_jump(&mut self, op: OpCode) -> usize {
        let addr = self.current_address();
        self.emit(op);
        addr
    }
    
    /// Patch a jump instruction
    fn patch_jump(&mut self, address: usize) {
        let target = self.current_address();
        self.bytecode.patch_jump(address, target);
    }
    
    /// Compile a statement
    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<(), RuminaError> {
        match stmt {
            Stmt::Expr(expr) => {
                self.compile_expr(expr)?;
                // Keep expression result on stack for potential return value
            }
            
            Stmt::VarDecl { name, value, .. } => {
                // Compile the value expression
                self.compile_expr(value)?;
                
                // Store in variable
                self.emit(OpCode::PopVar(name.clone()));
                self.symbols.define(name.clone());
            }
            
            Stmt::Assign { name, value } => {
                // Compile the value expression
                self.compile_expr(value)?;
                
                // Store in variable
                self.emit(OpCode::PopVar(name.clone()));
            }
            
            Stmt::Block(statements) => {
                self.symbols.enter_scope();
                self.emit(OpCode::EnterScope);
                
                for stmt in statements {
                    self.compile_stmt(stmt)?;
                }
                
                self.emit(OpCode::ExitScope);
                self.symbols.exit_scope();
            }
            
            Stmt::If { condition, then_branch, else_branch } => {
                // Compile condition
                self.compile_expr(condition)?;
                
                // Jump to else if false
                let else_jump = self.emit_jump(OpCode::JumpIfFalse(0));
                
                // Compile then branch
                for stmt in then_branch {
                    self.compile_stmt(stmt)?;
                }
                
                if let Some(else_stmts) = else_branch {
                    // Jump over else branch
                    let end_jump = self.emit_jump(OpCode::Jump(0));
                    
                    // Patch else jump to here
                    self.patch_jump(else_jump);
                    
                    // Compile else branch
                    for stmt in else_stmts {
                        self.compile_stmt(stmt)?;
                    }
                    
                    // Patch end jump
                    self.patch_jump(end_jump);
                } else {
                    // No else branch, just patch the jump
                    self.patch_jump(else_jump);
                }
            }
            
            Stmt::While { condition, body } => {
                let loop_start = self.current_address();
                
                // Push loop context
                self.loop_stack.push(LoopContext {
                    continue_target: loop_start,
                    break_patches: Vec::new(),
                });
                
                // Compile condition
                self.compile_expr(condition)?;
                
                // Jump to end if false
                let end_jump = self.emit_jump(OpCode::JumpIfFalse(0));
                
                // Compile body
                for stmt in body {
                    self.compile_stmt(stmt)?;
                }
                
                // Jump back to start
                self.emit(OpCode::Jump(loop_start));
                
                // Patch end jump
                self.patch_jump(end_jump);
                
                // Patch all break statements
                if let Some(loop_ctx) = self.loop_stack.pop() {
                    let break_target = self.current_address();
                    for break_addr in loop_ctx.break_patches {
                        self.bytecode.patch_jump(break_addr, break_target);
                    }
                }
            }
            
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    self.compile_expr(expr)?;
                } else {
                    self.emit(OpCode::PushConst(Value::Null));
                }
                self.emit(OpCode::Return);
            }
            
            Stmt::Break => {
                let jump_addr = self.emit_jump(OpCode::Jump(0));
                if let Some(loop_ctx) = self.loop_stack.last_mut() {
                    loop_ctx.break_patches.push(jump_addr);
                } else {
                    return Err(RuminaError::runtime("Break outside of loop".to_string()));
                }
            }
            
            Stmt::Continue => {
                if let Some(loop_ctx) = self.loop_stack.last() {
                    let target = loop_ctx.continue_target;
                    self.emit(OpCode::Jump(target));
                } else {
                    return Err(RuminaError::runtime("Continue outside of loop".to_string()));
                }
            }
            
            Stmt::FuncDef { name, params, body, decorators } => {
                // Store function definition
                let skip_jump = self.emit_jump(OpCode::Jump(0));
                
                let body_start = self.current_address();
                
                // Compile function body
                self.symbols.enter_scope();
                for param in params {
                    self.symbols.define(param.clone());
                }
                
                for stmt in body {
                    self.compile_stmt(stmt)?;
                }
                
                // Implicit return null if no explicit return
                self.emit(OpCode::PushConst(Value::Null));
                self.emit(OpCode::Return);
                
                self.symbols.exit_scope();
                
                let body_end = self.current_address();
                
                // Patch skip jump
                self.patch_jump(skip_jump);
                
                // Define the function
                self.emit(OpCode::DefineFunc {
                    name: name.clone(),
                    params: params.clone(),
                    body_start,
                    body_end,
                    decorators: decorators.clone(),
                });
                
                self.symbols.define(name.clone());
            }
            
            _ => {
                return Err(RuminaError::runtime(format!(
                    "Unimplemented statement compilation: {:?}",
                    stmt
                )));
            }
        }
        
        Ok(())
    }
    
    /// Compile an expression
    fn compile_expr(&mut self, expr: &Expr) -> Result<(), RuminaError> {
        match expr {
            Expr::Int(n) => {
                self.emit(OpCode::PushConst(Value::Int(*n)));
            }
            
            Expr::Float(f) => {
                self.emit(OpCode::PushConst(Value::Float(*f)));
            }
            
            Expr::String(s) => {
                self.emit(OpCode::PushConst(Value::String(s.clone())));
            }
            
            Expr::Bool(b) => {
                self.emit(OpCode::PushConst(Value::Bool(*b)));
            }
            
            Expr::Null => {
                self.emit(OpCode::PushConst(Value::Null));
            }
            
            Expr::Ident(name) => {
                self.emit(OpCode::PushVar(name.clone()));
            }
            
            Expr::Binary { left, op, right } => {
                // Compile operands
                self.compile_expr(left)?;
                self.compile_expr(right)?;
                
                // Emit operation
                let opcode = match op {
                    BinOp::Add => OpCode::Add,
                    BinOp::Sub => OpCode::Sub,
                    BinOp::Mul => OpCode::Mul,
                    BinOp::Div => OpCode::Div,
                    BinOp::Mod => OpCode::Mod,
                    BinOp::Pow => OpCode::Pow,
                    BinOp::Equal => OpCode::Eq,
                    BinOp::NotEqual => OpCode::Neq,
                    BinOp::Greater => OpCode::Gt,
                    BinOp::GreaterEq => OpCode::Gte,
                    BinOp::Less => OpCode::Lt,
                    BinOp::LessEq => OpCode::Lte,
                    BinOp::And => OpCode::And,
                    BinOp::Or => OpCode::Or,
                };
                
                self.emit(opcode);
            }
            
            Expr::Unary { op, expr } => {
                self.compile_expr(expr)?;
                
                let opcode = match op {
                    UnaryOp::Neg => OpCode::Neg,
                    UnaryOp::Not => OpCode::Not,
                    UnaryOp::Factorial => OpCode::Factorial,
                };
                
                self.emit(opcode);
            }
            
            Expr::Array(elements) => {
                // Compile each element
                for elem in elements {
                    self.compile_expr(elem)?;
                }
                
                // Create array from N elements
                self.emit(OpCode::MakeArray(elements.len()));
            }
            
            Expr::Call { func, args } => {
                // Compile arguments
                for arg in args {
                    self.compile_expr(arg)?;
                }
                
                // Check if it's a simple function call
                if let Expr::Ident(name) = &**func {
                    self.emit(OpCode::CallVar(name.clone(), args.len()));
                } else {
                    // Complex expression as function
                    self.compile_expr(func)?;
                    // TODO: Implement dynamic call
                    return Err(RuminaError::runtime(
                        "Dynamic function calls not yet implemented".to_string()
                    ));
                }
            }
            
            Expr::Index { object, index } => {
                self.compile_expr(object)?;
                self.compile_expr(index)?;
                self.emit(OpCode::Index);
            }
            
            Expr::Member { object, member } => {
                self.compile_expr(object)?;
                self.emit(OpCode::Member(member.clone()));
            }
            
            _ => {
                return Err(RuminaError::runtime(format!(
                    "Unimplemented expression compilation: {:?}",
                    expr
                )));
            }
        }
        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_compile_simple_expr() {
        let mut compiler = Compiler::new();
        
        // Compile: 2 + 3
        let expr = Expr::Binary {
            left: Box::new(Expr::Int(2)),
            op: BinOp::Add,
            right: Box::new(Expr::Int(3)),
        };
        
        compiler.compile_expr(&expr).unwrap();
        
        assert!(compiler.bytecode.instructions.len() > 0);
    }
    
    #[test]
    fn test_compile_var_decl() {
        let mut compiler = Compiler::new();
        
        // Compile: var x = 42;
        let stmt = Stmt::VarDecl {
            name: "x".to_string(),
            is_bigint: false,
            declared_type: None,
            value: Expr::Int(42),
        };
        
        compiler.compile_stmt(&stmt).unwrap();
        
        assert!(compiler.symbols.resolve("x").is_some());
    }
    
    #[test]
    fn test_compile_and_run_simple() {
        use crate::vm::VM;
        use std::rc::Rc;
        use std::cell::RefCell;
        use std::collections::HashMap;
        
        let mut compiler = Compiler::new();
        
        // Compile: 10 + 20
        let stmts = vec![
            Stmt::Expr(Expr::Binary {
                left: Box::new(Expr::Int(10)),
                op: BinOp::Add,
                right: Box::new(Expr::Int(20)),
            })
        ];
        
        let bytecode = compiler.compile(stmts).unwrap();
        
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);
        vm.load(bytecode);
        
        let result = vm.run().unwrap();
        match result {
            Some(Value::Int(n)) => assert_eq!(n, 30),
            _ => panic!("Expected Int(30), got {:?}", result),
        }
    }
    
    #[test]
    fn test_compile_and_run_variables() {
        use crate::vm::VM;
        use std::rc::Rc;
        use std::cell::RefCell;
        use std::collections::HashMap;
        
        let mut compiler = Compiler::new();
        
        // Compile: var x = 10; var y = 20; x + y
        let stmts = vec![
            Stmt::VarDecl {
                name: "x".to_string(),
                is_bigint: false,
                declared_type: None,
                value: Expr::Int(10),
            },
            Stmt::VarDecl {
                name: "y".to_string(),
                is_bigint: false,
                declared_type: None,
                value: Expr::Int(20),
            },
            Stmt::Expr(Expr::Binary {
                left: Box::new(Expr::Ident("x".to_string())),
                op: BinOp::Add,
                right: Box::new(Expr::Ident("y".to_string())),
            }),
        ];
        
        let bytecode = compiler.compile(stmts).unwrap();
        
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);
        vm.load(bytecode);
        
        let result = vm.run().unwrap();
        match result {
            Some(Value::Int(n)) => assert_eq!(n, 30),
            _ => panic!("Expected Int(30), got {:?}", result),
        }
    }
    
    #[test]
    fn test_compile_and_run_with_builtins() {
        use crate::vm::VM;
        use crate::interpreter::Interpreter;
        
        let mut compiler = Compiler::new();
        
        // Compile: abs(-10)
        let stmts = vec![
            Stmt::Expr(Expr::Call {
                func: Box::new(Expr::Ident("abs".to_string())),
                args: vec![Expr::Int(-10)],
            })
        ];
        
        let bytecode = compiler.compile(stmts).unwrap();
        
        // Use interpreter's globals to get built-in functions
        let interpreter = Interpreter::new();
        let globals = interpreter.get_globals();
        
        let mut vm = VM::new(globals);
        vm.load(bytecode);
        
        let result = vm.run().unwrap();
        match result {
            Some(Value::Int(n)) => assert_eq!(n, 10),
            _ => panic!("Expected Int(10), got {:?}", result),
        }
    }
}
