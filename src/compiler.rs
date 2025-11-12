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
    #[allow(dead_code)]
    name: String,

    /// Scope depth
    #[allow(dead_code)]
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

    #[allow(dead_code)]
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

    /// Lambda counter for unique IDs
    lambda_counter: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            bytecode: ByteCode::new(),
            symbols: SymbolTable::new(),
            loop_stack: Vec::new(),
            current_line: None,
            lambda_counter: 0,
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

    /// Simple type inference: Check if an expression is likely to produce an integer
    /// This is an optimistic heuristic - we emit specialized integer opcodes when there's
    /// a good chance the operands will be integers. The VM has fallback logic for mixed types.
    fn is_likely_int(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Int(_) => true,
            // Optimistically assume variables might be integers
            // This is safe because the VM has fallback logic
            Expr::Ident(_) => true,
            Expr::Binary { left, op, right } => {
                // Integer arithmetic operations preserve integer type
                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Mod => {
                        self.is_likely_int(left) && self.is_likely_int(right)
                    }
                    BinOp::Div => false, // Division may produce rational/float
                    BinOp::Pow => false, // Power may produce float for negative exponents
                    _ => false,          // Comparisons return bool, logical ops work on bool
                }
            }
            Expr::Unary { op, expr } => match op {
                UnaryOp::Neg => self.is_likely_int(expr),
                _ => false,
            },
            // Function calls might return integers
            Expr::Call { .. } => true,
            _ => false,
        }
    }

    /// Compile a statement
    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<(), RuminaError> {
        match stmt {
            Stmt::Expr(expr) => {
                self.compile_expr(expr)?;
                // Keep expression result on stack for potential return value
            }

            Stmt::VarDecl {
                name,
                value,
                is_bigint,
                declared_type,
            } => {
                // Compile the value expression
                self.compile_expr(value)?;

                // Apply type conversion if declared_type is specified
                if let Some(dtype) = declared_type {
                    self.emit(OpCode::ConvertType(dtype.clone()));
                } else if *is_bigint {
                    // Backward compatibility
                    self.emit(OpCode::ConvertType(DeclaredType::BigInt));
                }

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

            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
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

            Stmt::For {
                init,
                condition,
                update,
                body,
            } => {
                // Compile initialization (if present)
                if let Some(init_stmt) = init {
                    self.compile_stmt(init_stmt)?;
                }

                // Mark loop start (for continue, we'll jump to update)
                let condition_start = self.current_address();

                // Compile condition (if present)
                let end_jump = if let Some(cond_expr) = condition {
                    self.compile_expr(cond_expr)?;
                    Some(self.emit_jump(OpCode::JumpIfFalse(0)))
                } else {
                    None
                };

                // Remember where update starts (for continue)
                let update_placeholder = self.current_address();

                // Push loop context - continue jumps to update section
                self.loop_stack.push(LoopContext {
                    continue_target: update_placeholder,
                    break_patches: Vec::new(),
                });

                // Jump over update to body
                let body_jump = self.emit_jump(OpCode::Jump(0));

                // Compile update section
                let update_start = self.current_address();
                if let Some(update_stmt) = update {
                    self.compile_stmt(update_stmt)?;
                }
                // Jump back to condition
                self.emit(OpCode::Jump(condition_start));

                // Patch body jump to here
                self.patch_jump(body_jump);

                // Update the loop context with correct continue target
                if let Some(loop_ctx) = self.loop_stack.last_mut() {
                    loop_ctx.continue_target = update_start;
                }

                // Compile body
                for stmt in body {
                    self.compile_stmt(stmt)?;
                }

                // Jump to update
                self.emit(OpCode::Jump(update_start));

                // Patch end jump (if condition exists)
                if let Some(end_addr) = end_jump {
                    self.patch_jump(end_addr);
                }

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
                    let index = self.bytecode.add_constant(Value::Null);
                    self.emit(OpCode::PushConstPooled(index));
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

            Stmt::FuncDef {
                name,
                params,
                body,
                decorators,
            } => {
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
                let index = self.bytecode.add_constant(Value::Null);
                self.emit(OpCode::PushConstPooled(index));
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
                let index = self.bytecode.add_constant(Value::Int(*n));
                self.emit(OpCode::PushConstPooled(index));
            }

            Expr::Float(f) => {
                let index = self.bytecode.add_constant(Value::Float(*f));
                self.emit(OpCode::PushConstPooled(index));
            }

            Expr::String(s) => {
                let index = self.bytecode.add_constant(Value::String(s.clone()));
                self.emit(OpCode::PushConstPooled(index));
            }

            Expr::Bool(b) => {
                let index = self.bytecode.add_constant(Value::Bool(*b));
                self.emit(OpCode::PushConstPooled(index));
            }

            Expr::Null => {
                let index = self.bytecode.add_constant(Value::Null);
                self.emit(OpCode::PushConstPooled(index));
            }

            Expr::Ident(name) => {
                self.emit(OpCode::PushVar(name.clone()));
            }

            Expr::Binary { left, op, right } => {
                // Compile operands
                self.compile_expr(left)?;
                self.compile_expr(right)?;

                // Determine if we can use specialized integer opcodes
                let use_int_ops = self.is_likely_int(left) && self.is_likely_int(right);

                // Emit operation - use specialized opcodes when possible
                let opcode = match op {
                    BinOp::Add => {
                        if use_int_ops {
                            OpCode::AddInt
                        } else {
                            OpCode::Add
                        }
                    }
                    BinOp::Sub => {
                        if use_int_ops {
                            OpCode::SubInt
                        } else {
                            OpCode::Sub
                        }
                    }
                    BinOp::Mul => {
                        if use_int_ops {
                            OpCode::MulInt
                        } else {
                            OpCode::Mul
                        }
                    }
                    BinOp::Div => OpCode::Div,
                    BinOp::Mod => OpCode::Mod,
                    BinOp::Pow => OpCode::Pow,
                    BinOp::Equal => {
                        if use_int_ops {
                            OpCode::EqInt
                        } else {
                            OpCode::Eq
                        }
                    }
                    BinOp::NotEqual => {
                        if use_int_ops {
                            OpCode::NeqInt
                        } else {
                            OpCode::Neq
                        }
                    }
                    BinOp::Greater => {
                        if use_int_ops {
                            OpCode::GtInt
                        } else {
                            OpCode::Gt
                        }
                    }
                    BinOp::GreaterEq => {
                        if use_int_ops {
                            OpCode::GteInt
                        } else {
                            OpCode::Gte
                        }
                    }
                    BinOp::Less => {
                        if use_int_ops {
                            OpCode::LtInt
                        } else {
                            OpCode::Lt
                        }
                    }
                    BinOp::LessEq => {
                        if use_int_ops {
                            OpCode::LteInt
                        } else {
                            OpCode::Lte
                        }
                    }
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
                        "Dynamic function calls not yet implemented".to_string(),
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

            Expr::Lambda { params, body, .. } => {
                // Generate unique lambda ID
                let lambda_id = format!("__lambda_{}", self.lambda_counter);
                self.lambda_counter += 1;

                // Skip over the lambda body (similar to function definition)
                let skip_jump = self.emit_jump(OpCode::Jump(0));

                let body_start = self.current_address();

                // Compile lambda body
                self.symbols.enter_scope();
                for param in params {
                    self.symbols.define(param.clone());
                }

                // Lambda body is a statement - could be a block or an expression
                self.compile_stmt(body)?;

                // For simple lambdas (expressions), the result is already on stack
                // For complex lambdas, we need to ensure proper return
                self.emit(OpCode::Return);

                self.symbols.exit_scope();

                let body_end = self.current_address();

                // Patch skip jump
                self.patch_jump(skip_jump);

                // Create the lambda value and push it on stack
                // Store lambda_id in the bytecode so VM can register it
                self.emit(OpCode::DefineFunc {
                    name: lambda_id.clone(),
                    params: params.clone(),
                    body_start,
                    body_end,
                    decorators: vec![],
                });

                // Now push a marker that tells VM this is a lambda with this ID
                let index = self.bytecode.add_constant(Value::String(lambda_id.clone()));
                self.emit(OpCode::PushConstPooled(index));
                self.emit(OpCode::MakeLambda {
                    params: params.clone(),
                    body_start,
                    body_end,
                });
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
        use std::cell::RefCell;
        use std::collections::HashMap;
        use std::rc::Rc;

        let mut compiler = Compiler::new();

        // Compile: 10 + 20
        let stmts = vec![Stmt::Expr(Expr::Binary {
            left: Box::new(Expr::Int(10)),
            op: BinOp::Add,
            right: Box::new(Expr::Int(20)),
        })];

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
        use std::cell::RefCell;
        use std::collections::HashMap;
        use std::rc::Rc;

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
        use crate::interpreter::Interpreter;
        use crate::vm::VM;

        let mut compiler = Compiler::new();

        // Compile: abs(-10)
        let stmts = vec![Stmt::Expr(Expr::Call {
            func: Box::new(Expr::Ident("abs".to_string())),
            args: vec![Expr::Int(-10)],
        })];

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

    #[test]
    fn test_compile_and_run_user_defined_function() {
        use crate::vm::VM;
        use std::cell::RefCell;
        use std::collections::HashMap;
        use std::rc::Rc;

        let mut compiler = Compiler::new();

        // Compile: func double(x) { return x * 2; } double(21)
        let stmts = vec![
            Stmt::FuncDef {
                name: "double".to_string(),
                params: vec!["x".to_string()],
                body: vec![Stmt::Return(Some(Expr::Binary {
                    left: Box::new(Expr::Ident("x".to_string())),
                    op: BinOp::Mul,
                    right: Box::new(Expr::Int(2)),
                }))],
                decorators: vec![],
            },
            Stmt::Expr(Expr::Call {
                func: Box::new(Expr::Ident("double".to_string())),
                args: vec![Expr::Int(21)],
            }),
        ];

        let bytecode = compiler.compile(stmts).unwrap();

        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);
        vm.load(bytecode);

        let result = vm.run().unwrap();
        match result {
            Some(Value::Int(n)) => assert_eq!(n, 42),
            _ => panic!("Expected Int(42), got {:?}", result),
        }
    }

    #[test]
    fn test_compile_and_run_recursive_fibonacci() {
        use crate::vm::VM;
        use std::cell::RefCell;
        use std::collections::HashMap;
        use std::rc::Rc;

        let mut compiler = Compiler::new();

        // Compile: func fib(n) { if (n <= 1) { return n; } return fib(n-1) + fib(n-2); } fib(8)
        let stmts = vec![
            Stmt::FuncDef {
                name: "fib".to_string(),
                params: vec!["n".to_string()],
                body: vec![
                    Stmt::If {
                        condition: Expr::Binary {
                            left: Box::new(Expr::Ident("n".to_string())),
                            op: BinOp::LessEq,
                            right: Box::new(Expr::Int(1)),
                        },
                        then_branch: vec![Stmt::Return(Some(Expr::Ident("n".to_string())))],
                        else_branch: None,
                    },
                    Stmt::Return(Some(Expr::Binary {
                        left: Box::new(Expr::Call {
                            func: Box::new(Expr::Ident("fib".to_string())),
                            args: vec![Expr::Binary {
                                left: Box::new(Expr::Ident("n".to_string())),
                                op: BinOp::Sub,
                                right: Box::new(Expr::Int(1)),
                            }],
                        }),
                        op: BinOp::Add,
                        right: Box::new(Expr::Call {
                            func: Box::new(Expr::Ident("fib".to_string())),
                            args: vec![Expr::Binary {
                                left: Box::new(Expr::Ident("n".to_string())),
                                op: BinOp::Sub,
                                right: Box::new(Expr::Int(2)),
                            }],
                        }),
                    })),
                ],
                decorators: vec![],
            },
            Stmt::Expr(Expr::Call {
                func: Box::new(Expr::Ident("fib".to_string())),
                args: vec![Expr::Int(8)],
            }),
        ];

        let bytecode = compiler.compile(stmts).unwrap();

        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);
        vm.load(bytecode);

        let result = vm.run().unwrap();
        match result {
            Some(Value::Int(n)) => assert_eq!(n, 21), // fib(8) = 21
            _ => panic!("Expected Int(21), got {:?}", result),
        }
    }

    #[test]
    fn test_compile_and_run_for_loop() {
        use crate::vm::VM;
        use std::cell::RefCell;
        use std::collections::HashMap;
        use std::rc::Rc;

        let mut compiler = Compiler::new();

        // Compile: let sum = 0; for (let i = 1; i <= 5; i = i + 1) { sum = sum + i; } sum
        let stmts = vec![
            Stmt::VarDecl {
                name: "sum".to_string(),
                is_bigint: false,
                declared_type: None,
                value: Expr::Int(0),
            },
            Stmt::For {
                init: Some(Box::new(Stmt::VarDecl {
                    name: "i".to_string(),
                    is_bigint: false,
                    declared_type: None,
                    value: Expr::Int(1),
                })),
                condition: Some(Expr::Binary {
                    left: Box::new(Expr::Ident("i".to_string())),
                    op: BinOp::LessEq,
                    right: Box::new(Expr::Int(5)),
                }),
                update: Some(Box::new(Stmt::Assign {
                    name: "i".to_string(),
                    value: Expr::Binary {
                        left: Box::new(Expr::Ident("i".to_string())),
                        op: BinOp::Add,
                        right: Box::new(Expr::Int(1)),
                    },
                })),
                body: vec![Stmt::Assign {
                    name: "sum".to_string(),
                    value: Expr::Binary {
                        left: Box::new(Expr::Ident("sum".to_string())),
                        op: BinOp::Add,
                        right: Box::new(Expr::Ident("i".to_string())),
                    },
                }],
            },
            Stmt::Expr(Expr::Ident("sum".to_string())),
        ];

        let bytecode = compiler.compile(stmts).unwrap();

        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);
        vm.load(bytecode);

        let result = vm.run().unwrap();
        match result {
            Some(Value::Int(n)) => assert_eq!(n, 15), // 1+2+3+4+5 = 15
            _ => panic!("Expected Int(15), got {:?}", result),
        }
    }

    #[test]
    fn test_compile_and_run_lambda() {
        use crate::vm::VM;
        use std::cell::RefCell;
        use std::collections::HashMap;
        use std::rc::Rc;

        let mut compiler = Compiler::new();

        // Compile: let add = |a, b| a + b; add(10, 20)
        let stmts = vec![
            Stmt::VarDecl {
                name: "add".to_string(),
                is_bigint: false,
                declared_type: None,
                value: Expr::Lambda {
                    params: vec!["a".to_string(), "b".to_string()],
                    body: Box::new(Stmt::Expr(Expr::Binary {
                        left: Box::new(Expr::Ident("a".to_string())),
                        op: BinOp::Add,
                        right: Box::new(Expr::Ident("b".to_string())),
                    })),
                    is_simple: true,
                },
            },
            Stmt::Expr(Expr::Call {
                func: Box::new(Expr::Ident("add".to_string())),
                args: vec![Expr::Int(10), Expr::Int(20)],
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
    fn test_compile_and_run_lambda_with_closure() {
        use crate::vm::VM;
        use std::cell::RefCell;
        use std::collections::HashMap;
        use std::rc::Rc;

        let mut compiler = Compiler::new();

        // Compile: let x = 5; let add_x = |a| a + x; add_x(10)
        let stmts = vec![
            Stmt::VarDecl {
                name: "x".to_string(),
                is_bigint: false,
                declared_type: None,
                value: Expr::Int(5),
            },
            Stmt::VarDecl {
                name: "add_x".to_string(),
                is_bigint: false,
                declared_type: None,
                value: Expr::Lambda {
                    params: vec!["a".to_string()],
                    body: Box::new(Stmt::Expr(Expr::Binary {
                        left: Box::new(Expr::Ident("a".to_string())),
                        op: BinOp::Add,
                        right: Box::new(Expr::Ident("x".to_string())),
                    })),
                    is_simple: true,
                },
            },
            Stmt::Expr(Expr::Call {
                func: Box::new(Expr::Ident("add_x".to_string())),
                args: vec![Expr::Int(10)],
            }),
        ];

        let bytecode = compiler.compile(stmts).unwrap();

        let globals = Rc::new(RefCell::new(HashMap::new()));
        let mut vm = VM::new(globals);
        vm.load(bytecode);

        let result = vm.run().unwrap();
        match result {
            Some(Value::Int(n)) => assert_eq!(n, 15), // 10 + 5 = 15
            _ => panic!("Expected Int(15), got {:?}", result),
        }
    }
}
