/// Bytecode compiler for Rumina
///
/// This module compiles AST to bytecode instructions for the VM.
use crate::ast::*;
use crate::error::RuminaError;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::value::Value;
use crate::vm::{ByteCode, OpCode, Register};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

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

/// Bytecode compiler with register allocation
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

    /// Set of already included files to prevent circular includes
    included_files: HashSet<String>,

    /// Current working directory for resolving relative includes
    current_dir: Option<String>,

    /// Module namespace mappings (module_name -> prefix for variables)
    module_namespaces: HashMap<String, String>,

    /// Next temporary register to allocate (R8-R15)
    next_temp_reg: u8,

    /// Register allocation stack for nested expressions
    reg_stack: Vec<Register>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            bytecode: ByteCode::new(),
            symbols: SymbolTable::new(),
            loop_stack: Vec::new(),
            current_line: None,
            lambda_counter: 0,
            included_files: HashSet::new(),
            current_dir: None,
            module_namespaces: HashMap::new(),
            next_temp_reg: 8, // Start from R8 (0-7 are reserved, R8-R15 are temps)
            reg_stack: Vec::new(),
        }
    }

    /// Create a new compiler with a specific working directory
    pub fn with_current_dir(current_dir: String) -> Self {
        Compiler {
            bytecode: ByteCode::new(),
            symbols: SymbolTable::new(),
            loop_stack: Vec::new(),
            current_line: None,
            lambda_counter: 0,
            included_files: HashSet::new(),
            current_dir: Some(current_dir),
            module_namespaces: HashMap::new(),
            next_temp_reg: 8,
            reg_stack: Vec::new(),
        }
    }

    /// Allocate a temporary register (R8-R15)
    fn alloc_reg(&mut self) -> Register {
        assert!(self.next_temp_reg >= 8 && self.next_temp_reg <= 15, 
            "Register allocation out of bounds: {}", self.next_temp_reg);
        let reg = Register::from_u8(self.next_temp_reg)
            .expect("next_temp_reg should always be valid (8-15)");
        self.next_temp_reg += 1;
        if self.next_temp_reg > 15 {
            // Wrap around - in a production compiler, this should error or spill to stack
            self.next_temp_reg = 8;
        }
        self.reg_stack.push(reg);
        reg
    }

    /// Free a temporary register
    /// Note: Current implementation uses LIFO (stack) semantics
    /// Registers must be freed in reverse order of allocation
    fn free_reg(&mut self, reg: Register) {
        // Validate that we're freeing the most recently allocated register
        if let Some(&top_reg) = self.reg_stack.last() {
            assert_eq!(top_reg, reg, 
                "Register free order violation: expected {:?}, got {:?}", top_reg, reg);
        }
        self.reg_stack.pop();
        if let Some(last_reg) = self.reg_stack.last() {
            self.next_temp_reg = last_reg.as_u8() + 1;
        } else {
            self.next_temp_reg = 8;
        }
    }

    /// Reset register allocation (for new scope/function)
    fn reset_regs(&mut self) {
        self.next_temp_reg = 8;
        self.reg_stack.clear();
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
                // Compile expression and store result in RAX for potential return
                let reg = self.compile_expr(expr)?;
                // Move result to RAX if not already there
                if reg != Register::RAX {
                    self.emit(OpCode::MovReg(Register::RAX, reg));
                    self.free_reg(reg);
                }
            }

            Stmt::VarDecl {
                name,
                value,
                is_bigint,
                declared_type,
            } => {
                // Compile the value expression into a register
                let reg = self.compile_expr(value)?;

                // Apply type conversion if declared_type is specified
                if let Some(dtype) = declared_type {
                    self.emit(OpCode::ConvertType(reg, dtype.clone()));
                } else if *is_bigint {
                    // Backward compatibility
                    self.emit(OpCode::ConvertType(reg, DeclaredType::BigInt));
                }

                // Store register in variable
                self.emit(OpCode::StoreVar(name.clone(), reg));
                self.free_reg(reg);
                self.symbols.define(name.clone());
            }

            Stmt::Assign { name, value } => {
                // Compile the value expression into a register
                let reg = self.compile_expr(value)?;

                // Store register in variable
                self.emit(OpCode::StoreVar(name.clone(), reg));
                self.free_reg(reg);
            }

            Stmt::MemberAssign {
                object,
                member,
                value,
            } => {
                // Check if object is a variable identifier
                if let Expr::Ident(var_name) = object {
                    // For variable identifiers, use MemberAssignVar to enable null auto-vivification
                    // Compile the value expression
                    let val_reg = self.compile_expr(value)?;

                    // Emit member assignment with variable name
                    self.emit(OpCode::MemberAssignVar(var_name.clone(), member.clone(), val_reg));
                    self.free_reg(val_reg);
                } else {
                    // For other expressions, use regular MemberAssign
                    // Compile the object expression
                    let obj_reg = self.compile_expr(object)?;

                    // Compile the value expression
                    let val_reg = self.compile_expr(value)?;

                    // Emit member assignment
                    self.emit(OpCode::MemberAssign(obj_reg, member.clone(), val_reg));
                    self.free_reg(val_reg);
                    self.free_reg(obj_reg);
                }
            }

            Stmt::Block(statements) => {
                self.symbols.enter_scope();

                for stmt in statements {
                    self.compile_stmt(stmt)?;
                }

                self.symbols.exit_scope();
            }

            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                // Compile condition into a register
                let cond_reg = self.compile_expr(condition)?;

                // Jump to else if false
                let else_jump = self.emit_jump(OpCode::Jz(cond_reg, 0));
                self.free_reg(cond_reg);

                // Compile then branch
                for stmt in then_branch {
                    self.compile_stmt(stmt)?;
                }

                if let Some(else_stmts) = else_branch {
                    // Jump over else branch
                    let end_jump = self.emit_jump(OpCode::Jmp(0));

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
                let cond_reg = self.compile_expr(condition)?;

                // Jump to end if false
                let end_jump = self.emit_jump(OpCode::Jz(cond_reg, 0));
                self.free_reg(cond_reg);

                // Compile body
                for stmt in body {
                    self.compile_stmt(stmt)?;
                }

                // Jump back to start
                self.emit(OpCode::Jmp(loop_start));

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
                    let cond_reg = self.compile_expr(cond_expr)?;
                    let jump = Some(self.emit_jump(OpCode::Jz(cond_reg, 0)));
                    self.free_reg(cond_reg);
                    jump
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
                let body_jump = self.emit_jump(OpCode::Jmp(0));

                // Compile update section
                let update_start = self.current_address();
                if let Some(update_stmt) = update {
                    self.compile_stmt(update_stmt)?;
                }
                // Jump back to condition
                self.emit(OpCode::Jmp(condition_start));

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
                self.emit(OpCode::Jmp(update_start));

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
                    // Compile expression and put result in RAX
                    let reg = self.compile_expr(expr)?;
                    if reg != Register::RAX {
                        self.emit(OpCode::MovReg(Register::RAX, reg));
                        self.free_reg(reg);
                    }
                } else {
                    // Return null
                    self.emit(OpCode::MovImm(Register::RAX, Value::Null));
                }
                self.emit(OpCode::Ret);
            }

            Stmt::Break => {
                let jump_addr = self.emit_jump(OpCode::Jmp(0));
                if let Some(loop_ctx) = self.loop_stack.last_mut() {
                    loop_ctx.break_patches.push(jump_addr);
                } else {
                    return Err(RuminaError::runtime("Break outside of loop".to_string()));
                }
            }

            Stmt::Continue => {
                if let Some(loop_ctx) = self.loop_stack.last() {
                    let target = loop_ctx.continue_target;
                    self.emit(OpCode::Jmp(target));
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
                let skip_jump = self.emit_jump(OpCode::Jmp(0));

                let body_start = self.current_address();

                // Compile function body
                self.symbols.enter_scope();
                self.reset_regs(); // Reset register allocation for function
                
                for param in params {
                    self.symbols.define(param.clone());
                }

                for stmt in body {
                    self.compile_stmt(stmt)?;
                }

                // Implicit return null if no explicit return
                self.emit(OpCode::MovImm(Register::RAX, Value::Null));
                self.emit(OpCode::Ret);

                self.symbols.exit_scope();
                self.reset_regs();

                let body_end = self.current_address();

                // Patch skip jump
                self.patch_jump(skip_jump);

                // Define the function
                self.emit(OpCode::DefineFunc(Box::new(crate::vm::FuncDefInfo {
                    name: name.clone(),
                    params: params.clone(),
                    body_start,
                    body_end,
                    decorators: decorators.clone(),
                })));

                self.symbols.define(name.clone());
            }

            Stmt::Include(path) => {
                // Resolve include at compile time
                self.compile_include(path)?;
            }
            Stmt::Empty => {}

            _ => {
                return Err(RuminaError::runtime(format!(
                    "Unimplemented statement compilation: {:?}",
                    stmt
                )));
            }
        }

        Ok(())
    }

    /// Compile an include statement by reading and inlining the included file
    fn compile_include(&mut self, path: &str) -> Result<(), RuminaError> {
        // Construct file path
        let mut file_path = path.to_string();

        // Add .lm extension if not present
        if !file_path.ends_with(".lm") {
            file_path.push_str(".lm");
        }

        // Resolve relative path based on current directory
        let resolved_path = if let Some(ref current_dir) = self.current_dir {
            Path::new(current_dir).join(&file_path)
        } else {
            Path::new(&file_path).to_path_buf()
        };

        // Convert to canonical string for duplicate checking
        let canonical_path = resolved_path
            .canonicalize()
            .unwrap_or_else(|_| resolved_path.clone())
            .to_string_lossy()
            .to_string();

        // Check if already included to prevent circular includes
        if self.included_files.contains(&canonical_path) {
            return Ok(()); // Already included, skip
        }

        // Mark as included
        self.included_files.insert(canonical_path.clone());

        // Read the file
        let contents = fs::read_to_string(&resolved_path).map_err(|e| {
            RuminaError::runtime(format!(
                "Cannot read included file '{}': {}",
                resolved_path.display(),
                e
            ))
        })?;

        // Parse the included file
        let mut lexer = Lexer::new(contents.clone());
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let statements = parser.parse().map_err(|e| {
            RuminaError::runtime(format!(
                "Error parsing included file '{}': {}",
                resolved_path.display(),
                e
            ))
        })?;

        // Extract module name from the included file
        // Look for: define module_name = "..."
        let module_name = self.extract_module_name(&statements, &contents, path);

        // Store the module namespace mapping
        self.module_namespaces
            .insert(module_name.clone(), module_name.clone());

        // Compile each statement from the included file with namespace prefix
        for stmt in statements {
            self.compile_stmt_with_namespace(&stmt, &module_name)?;
        }

        Ok(())
    }

    /// Extract module name from statements or derive from file path
    fn extract_module_name(&self, statements: &[Stmt], _contents: &str, path: &str) -> String {
        // Look for: define module_name = "..." or var module_name = "..."
        for stmt in statements {
            match stmt {
                Stmt::VarDecl { name, value, .. } if name == "module_name" => {
                    if let Expr::String(s) = value {
                        return s.clone();
                    }
                }
                Stmt::Assign { name, value } if name == "module_name" => {
                    if let Expr::String(s) = value {
                        return s.clone();
                    }
                }
                // Also check expression statements that might be assignments
                Stmt::Expr(expr) => {
                    // Check for: define module_name = "..." (which is parsed as a call expression)
                    if let Expr::Call { func, args } = expr {
                        if let Expr::Ident(fn_name) = &**func {
                            if fn_name == "define" && args.len() == 2 {
                                if let Expr::Ident(var_name) = &args[0] {
                                    if var_name == "module_name" {
                                        if let Expr::String(s) = &args[1] {
                                            return s.clone();
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        // Fallback: use filename without extension
        path.split('/')
            .last()
            .or_else(|| path.split('\\').last())
            .unwrap_or(path)
            .trim_end_matches(".lm")
            .to_string()
    }

    /// Compile a statement with namespace prefix for top-level items
    fn compile_stmt_with_namespace(
        &mut self,
        stmt: &Stmt,
        namespace: &str,
    ) -> Result<(), RuminaError> {
        match stmt {
            // Skip module_name variable declaration or assignment
            Stmt::VarDecl { name, .. } if name == "module_name" => Ok(()),
            Stmt::Assign { name, .. } if name == "module_name" => Ok(()),

            // Skip standalone "define" expression (which precedes module_name assignment)
            Stmt::Expr(Expr::Ident(name)) if name == "define" => Ok(()),

            // Prefix top-level variable declarations
            Stmt::VarDecl {
                name,
                value,
                is_bigint,
                declared_type,
            } => {
                let prefixed_name = format!("{}::{}", namespace, name);

                // Compile the value expression
                let reg = self.compile_expr(value)?;

                // Apply type conversion if declared_type is specified
                if let Some(dtype) = declared_type {
                    self.emit(OpCode::ConvertType(reg, dtype.clone()));
                } else if *is_bigint {
                    self.emit(OpCode::ConvertType(reg, DeclaredType::BigInt));
                }

                // Store in prefixed variable
                self.emit(OpCode::StoreVar(prefixed_name.clone(), reg));
                self.free_reg(reg);
                self.symbols.define(prefixed_name);
                Ok(())
            }

            // Prefix top-level function definitions
            Stmt::FuncDef {
                name,
                params,
                body,
                decorators,
            } => {
                let prefixed_name = format!("{}::{}", namespace, name);

                // Store function definition
                let skip_jump = self.emit_jump(OpCode::Jmp(0));

                let body_start = self.current_address();

                // Compile function body
                self.symbols.enter_scope();
                self.reset_regs();
                
                for param in params {
                    self.symbols.define(param.clone());
                }

                for stmt in body {
                    self.compile_stmt(stmt)?;
                }

                // Implicit return null if no explicit return
                self.emit(OpCode::MovImm(Register::RAX, Value::Null));
                self.emit(OpCode::Ret);

                self.symbols.exit_scope();
                self.reset_regs();

                let body_end = self.current_address();

                // Patch skip jump
                self.patch_jump(skip_jump);

                // Define the function with prefixed name
                self.emit(OpCode::DefineFunc(Box::new(crate::vm::FuncDefInfo {
                    name: prefixed_name.clone(),
                    params: params.clone(),
                    body_start,
                    body_end,
                    decorators: decorators.clone(),
                })));

                self.symbols.define(prefixed_name);
                Ok(())
            }

            // Other statements compile normally
            _ => self.compile_stmt(stmt),
        }
    }

    /// Compile an expression and return the register containing the result
    fn compile_expr(&mut self, expr: &Expr) -> Result<Register, RuminaError> {
        match expr {
            Expr::Int(n) => {
                let reg = self.alloc_reg();
                let index = self.bytecode.add_constant(Value::Int(*n));
                self.emit(OpCode::MovConst(reg, index));
                Ok(reg)
            }

            Expr::Float(f) => {
                let reg = self.alloc_reg();
                let index = self.bytecode.add_constant(Value::Float(*f));
                self.emit(OpCode::MovConst(reg, index));
                Ok(reg)
            }

            Expr::String(s) => {
                let reg = self.alloc_reg();
                let index = self.bytecode.add_constant(Value::String(s.clone()));
                self.emit(OpCode::MovConst(reg, index));
                Ok(reg)
            }

            Expr::Bool(b) => {
                let reg = self.alloc_reg();
                self.emit(OpCode::MovImm(reg, Value::Bool(*b)));
                Ok(reg)
            }

            Expr::Null => {
                let reg = self.alloc_reg();
                self.emit(OpCode::MovImm(reg, Value::Null));
                Ok(reg)
            }

            Expr::Ident(name) => {
                let reg = self.alloc_reg();
                self.emit(OpCode::MovVar(reg, name.clone()));
                Ok(reg)
            }

            Expr::Binary { left, op, right } => {
                // Compile operands into registers
                let left_reg = self.compile_expr(left)?;
                let right_reg = self.compile_expr(right)?;

                // Emit operation - result goes in left_reg
                let opcode = match op {
                    BinOp::Add => OpCode::Add(left_reg, right_reg),
                    BinOp::Sub => OpCode::Sub(left_reg, right_reg),
                    BinOp::Mul => OpCode::Mul(left_reg, right_reg),
                    BinOp::Div => OpCode::Div(left_reg, right_reg),
                    BinOp::Mod => OpCode::ModOp(left_reg, right_reg),
                    BinOp::Pow => OpCode::Pow(left_reg, right_reg),
                    BinOp::Equal => OpCode::CmpEq(left_reg, right_reg),
                    BinOp::NotEqual => OpCode::CmpNe(left_reg, right_reg),
                    BinOp::Greater => OpCode::CmpGt(left_reg, right_reg),
                    BinOp::GreaterEq => OpCode::CmpGe(left_reg, right_reg),
                    BinOp::Less => OpCode::CmpLt(left_reg, right_reg),
                    BinOp::LessEq => OpCode::CmpLe(left_reg, right_reg),
                    BinOp::And => OpCode::And(left_reg, right_reg),
                    BinOp::Or => OpCode::Or(left_reg, right_reg),
                };

                self.emit(opcode);
                self.free_reg(right_reg);
                Ok(left_reg)
            }

            Expr::Unary { op, expr } => {
                let reg = self.compile_expr(expr)?;

                let opcode = match op {
                    UnaryOp::Neg => OpCode::Neg(reg),
                    UnaryOp::Not => OpCode::Not(reg),
                    UnaryOp::Factorial => OpCode::Factorial(reg),
                };

                self.emit(opcode);
                Ok(reg)
            }

            Expr::Array(elements) => {
                // Push each element onto the stack
                for elem in elements {
                    let elem_reg = self.compile_expr(elem)?;
                    self.emit(OpCode::Push(elem_reg));
                    self.free_reg(elem_reg);
                }

                // Create array from N elements on stack
                let result_reg = self.alloc_reg();
                self.emit(OpCode::MakeArray(result_reg, elements.len()));
                Ok(result_reg)
            }

            Expr::Struct(fields) => {
                // Push each field (key, value) pair onto the stack
                for (key, value) in fields {
                    // Push key as string constant
                    let key_reg = self.alloc_reg();
                    let key_index = self.bytecode.add_constant(Value::String(key.clone()));
                    self.emit(OpCode::MovConst(key_reg, key_index));
                    self.emit(OpCode::Push(key_reg));
                    self.free_reg(key_reg);

                    // Push value
                    let val_reg = self.compile_expr(value)?;
                    self.emit(OpCode::Push(val_reg));
                    self.free_reg(val_reg);
                }

                // Create struct from N field pairs on stack
                let result_reg = self.alloc_reg();
                self.emit(OpCode::MakeStruct(result_reg, fields.len()));
                Ok(result_reg)
            }

            Expr::Call { func, args } => {
                // Check if it's a simple function call
                if let Expr::Ident(name) = &**func {
                    // Compile arguments into R8-R15, or push to stack if more than 8
                    let mut arg_regs = Vec::new();
                    for arg in args.iter() {
                        let arg_reg = self.compile_expr(arg)?;
                        arg_regs.push(arg_reg);
                    }
                    
                    // Now move them to the correct argument registers
                    for (i, arg_reg) in arg_regs.iter().enumerate() {
                        if i < 8 {
                            // First 8 args go in R8-R15
                            let target_reg = Register::from_u8(8 + i as u8).unwrap();
                            if *arg_reg != target_reg {
                                self.emit(OpCode::MovReg(target_reg, *arg_reg));
                            }
                        } else {
                            // Rest go on stack
                            self.emit(OpCode::Push(*arg_reg));
                        }
                    }
                    
                    // Free all argument registers in reverse order (LIFO)
                    for arg_reg in arg_regs.iter().rev() {
                        self.free_reg(*arg_reg);
                    }
                    
                    self.emit(OpCode::CallVar(name.clone(), args.len()));
                    // Result is in RAX, allocate a temp reg and move it
                    let result_reg = self.alloc_reg();
                    self.emit(OpCode::MovReg(result_reg, Register::RAX));
                    Ok(result_reg)
                } else if let Expr::Namespace { module, name } = &**func {
                    // Namespace function call: module::function(args)
                    let mut arg_regs = Vec::new();
                    for arg in args.iter() {
                        let arg_reg = self.compile_expr(arg)?;
                        arg_regs.push(arg_reg);
                    }
                    
                    for (i, arg_reg) in arg_regs.iter().enumerate() {
                        if i < 8 {
                            let target_reg = Register::from_u8(8 + i as u8).unwrap();
                            if *arg_reg != target_reg {
                                self.emit(OpCode::MovReg(target_reg, *arg_reg));
                            }
                        } else {
                            self.emit(OpCode::Push(*arg_reg));
                        }
                    }
                    
                    // Free all argument registers in reverse order (LIFO)
                    for arg_reg in arg_regs.iter().rev() {
                        self.free_reg(*arg_reg);
                    }
                    
                    let prefixed_name = format!("{}::{}", module, name);
                    self.emit(OpCode::CallVar(prefixed_name, args.len()));
                    let result_reg = self.alloc_reg();
                    self.emit(OpCode::MovReg(result_reg, Register::RAX));
                    Ok(result_reg)
                } else if let Expr::Member { object, member } = &**func {
                    // Method call: obj.method(args)
                    // Compile the object into RDI
                    let obj_reg = self.compile_expr(object)?;
                    self.emit(OpCode::MovReg(Register::RDI, obj_reg));
                    self.free_reg(obj_reg);
                    
                    // Get the method value into RSI
                    self.emit(OpCode::Member(Register::RSI, Register::RDI, member.clone()));
                    
                    // Compile arguments
                    let mut arg_regs = Vec::new();
                    for arg in args.iter() {
                        let arg_reg = self.compile_expr(arg)?;
                        arg_regs.push(arg_reg);
                    }
                    
                    for (i, arg_reg) in arg_regs.iter().enumerate() {
                        if i < 8 {
                            let target_reg = Register::from_u8(8 + i as u8).unwrap();
                            if *arg_reg != target_reg {
                                self.emit(OpCode::MovReg(target_reg, *arg_reg));
                            }
                        } else {
                            self.emit(OpCode::Push(*arg_reg));
                        }
                    }
                    
                    // Free all argument registers in reverse order (LIFO)
                    for arg_reg in arg_regs.iter().rev() {
                        self.free_reg(*arg_reg);
                    }
                    
                    // Emit method call (object in RDI, method in RSI)
                    self.emit(OpCode::CallMethod(args.len()));
                    let result_reg = self.alloc_reg();
                    self.emit(OpCode::MovReg(result_reg, Register::RAX));
                    Ok(result_reg)
                } else {
                    // Dynamic function call (e.g., (expr)())
                    // Compile the function expression
                    let func_reg = self.compile_expr(func)?;
                    
                    // Compile arguments
                    let mut arg_regs = Vec::new();
                    for arg in args.iter() {
                        let arg_reg = self.compile_expr(arg)?;
                        arg_regs.push(arg_reg);
                    }
                    
                    for (i, arg_reg) in arg_regs.iter().enumerate() {
                        if i < 8 {
                            let target_reg = Register::from_u8(8 + i as u8).unwrap();
                            if *arg_reg != target_reg {
                                self.emit(OpCode::MovReg(target_reg, *arg_reg));
                            }
                        } else {
                            self.emit(OpCode::Push(*arg_reg));
                        }
                    }
                    
                    // Free all argument registers in reverse order (LIFO)
                    for arg_reg in arg_regs.iter().rev() {
                        self.free_reg(*arg_reg);
                    }
                    
                    // Emit dynamic call
                    self.emit(OpCode::CallReg(func_reg, args.len()));
                    self.free_reg(func_reg);
                    let result_reg = self.alloc_reg();
                    self.emit(OpCode::MovReg(result_reg, Register::RAX));
                    Ok(result_reg)
                }
            }

            Expr::Index { object, index } => {
                let obj_reg = self.compile_expr(object)?;
                let idx_reg = self.compile_expr(index)?;
                // Emit the instruction while source registers are still allocated
                // (register numbers are captured in the instruction)
                let result_reg = self.alloc_reg();
                self.emit(OpCode::Index(result_reg, obj_reg, idx_reg));
                // Free registers in correct LIFO order: result, idx, obj
                self.free_reg(result_reg);
                self.free_reg(idx_reg);
                self.free_reg(obj_reg);
                // Re-allocate for the result we're returning
                let final_result = self.alloc_reg();
                // The result value is at runtime in result_reg, need to preserve it
                // Emit move if registers differ (they will if we consumed >1 register)
                if final_result != result_reg {
                    self.emit(OpCode::MovReg(final_result, result_reg));
                }
                Ok(final_result)
            }

            Expr::Member { object, member } => {
                let obj_reg = self.compile_expr(object)?;
                // Emit the instruction while source register is still allocated
                let result_reg = self.alloc_reg();
                self.emit(OpCode::Member(result_reg, obj_reg, member.clone()));
                // Free registers in correct LIFO order: result, then obj
                self.free_reg(result_reg);
                self.free_reg(obj_reg);
                // Re-allocate for the result we're returning
                let final_result = self.alloc_reg();
                // Emit move if registers differ
                if final_result != result_reg {
                    self.emit(OpCode::MovReg(final_result, result_reg));
                }
                Ok(final_result)
            }

            Expr::Lambda { params, body, .. } => {
                // Generate unique lambda ID
                let lambda_id = format!("__lambda_{}", self.lambda_counter);
                self.lambda_counter += 1;

                // Skip over the lambda body (similar to function definition)
                let skip_jump = self.emit_jump(OpCode::Jmp(0));

                let body_start = self.current_address();

                // Compile lambda body
                self.symbols.enter_scope();
                self.reset_regs();
                
                for param in params {
                    self.symbols.define(param.clone());
                }

                // Lambda body is a statement - could be a block or an expression
                self.compile_stmt(body)?;

                // Ensure result is in RAX and return
                self.emit(OpCode::Ret);

                self.symbols.exit_scope();
                self.reset_regs();

                let body_end = self.current_address();

                // Patch skip jump
                self.patch_jump(skip_jump);

                // Create the lambda value and push it on stack
                // Store lambda_id in the bytecode so VM can register it
                self.emit(OpCode::DefineFunc(Box::new(crate::vm::FuncDefInfo {
                    name: lambda_id.clone(),
                    params: params.clone(),
                    body_start,
                    body_end,
                    decorators: vec![],
                })));

                // Create lambda and return it in a register
                let lambda_reg = self.alloc_reg();
                let id_index = self.bytecode.add_constant(Value::String(lambda_id.clone()));
                self.emit(OpCode::MovConst(lambda_reg, id_index));
                // Push lambda ID to stack for MakeLambda
                self.emit(OpCode::Push(lambda_reg));
                self.emit(OpCode::MakeLambda(Box::new(crate::vm::LambdaInfo {
                    params: params.clone(),
                    body_start,
                    body_end,
                })));
                // MakeLambda places result in RAX, move to our register
                self.emit(OpCode::MovReg(lambda_reg, Register::RAX));
                Ok(lambda_reg)
            }

            Expr::Namespace { module, name } => {
                // Namespace access: module::name
                let prefixed_name = format!("{}::{}", module, name);
                let reg = self.alloc_reg();
                self.emit(OpCode::MovVar(reg, prefixed_name));
                Ok(reg)
            }
        }
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
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
