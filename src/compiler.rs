/// Bytecode compiler for Rumina
///
/// This module compiles AST to bytecode instructions for the VM.
use crate::ast::*;
use crate::error::RuminaError;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::value::Value;
use crate::vm::{ByteCode, OpCode};
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

    /// Set of already included files to prevent circular includes
    included_files: HashSet<String>,

    /// Current working directory for resolving relative includes
    current_dir: Option<String>,

    /// Module namespace mappings (module_name -> prefix for variables)
    module_namespaces: HashMap<String, String>,
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

            Stmt::MemberAssign {
                object,
                member,
                value,
            } => {
                // Compile the object expression
                self.compile_expr(object)?;

                // Compile the value expression
                self.compile_expr(value)?;

                // Emit member assignment
                self.emit(OpCode::MemberAssign(member.clone()));
            }

            Stmt::Block(statements) => {
                self.symbols.enter_scope();
                // EnterScope and ExitScope opcodes are no-ops, so we omit them for performance

                for stmt in statements {
                    self.compile_stmt(stmt)?;
                }

                // ExitScope is also a no-op
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
                self.compile_expr(value)?;

                // Apply type conversion if declared_type is specified
                if let Some(dtype) = declared_type {
                    self.emit(OpCode::ConvertType(dtype.clone()));
                } else if *is_bigint {
                    self.emit(OpCode::ConvertType(DeclaredType::BigInt));
                }

                // Store in prefixed variable
                self.emit(OpCode::PopVar(prefixed_name.clone()));
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

                // Emit operation - always use generic opcodes
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

            Expr::Struct(fields) => {
                // Compile each field (key, value) pair
                for (key, value) in fields {
                    // Push key as string constant
                    let key_index = self.bytecode.add_constant(Value::String(key.clone()));
                    self.emit(OpCode::PushConstPooled(key_index));
                    // Push value
                    self.compile_expr(value)?;
                }

                // Create struct from N field pairs
                self.emit(OpCode::MakeStruct(fields.len()));
            }

            Expr::Call { func, args } => {
                // Check if it's a simple function call
                if let Expr::Ident(name) = &**func {
                    // Compile arguments
                    for arg in args {
                        self.compile_expr(arg)?;
                    }
                    self.emit(OpCode::CallVar(name.clone(), args.len()));
                } else if let Expr::Namespace { module, name } = &**func {
                    // Namespace function call: module::function(args)
                    for arg in args {
                        self.compile_expr(arg)?;
                    }
                    let prefixed_name = format!("{}::{}", module, name);
                    self.emit(OpCode::CallVar(prefixed_name, args.len()));
                } else if let Expr::Member { object, member } = &**func {
                    // Method call: obj.method(args)
                    // Compile the object
                    self.compile_expr(object)?;
                    // Duplicate it (one for self, one for getting the method)
                    self.emit(OpCode::Dup);
                    // Get the method value (consumes the top copy)
                    self.emit(OpCode::Member(member.clone()));
                    // Compile arguments
                    for arg in args {
                        self.compile_expr(arg)?;
                    }
                    // Emit method call (expects stack: [object, method, args...])
                    self.emit(OpCode::CallMethod(args.len()));
                } else {
                    // Dynamic function call (e.g., (expr)())
                    // First compile the function expression
                    self.compile_expr(func)?;
                    // Then compile arguments
                    for arg in args {
                        self.compile_expr(arg)?;
                    }
                    // Emit dynamic call
                    self.emit(OpCode::Call(args.len()));
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
                self.emit(OpCode::DefineFunc(Box::new(crate::vm::FuncDefInfo {
                    name: lambda_id.clone(),
                    params: params.clone(),
                    body_start,
                    body_end,
                    decorators: vec![],
                })));

                // Now push a marker that tells VM this is a lambda with this ID
                let index = self.bytecode.add_constant(Value::String(lambda_id.clone()));
                self.emit(OpCode::PushConstPooled(index));
                self.emit(OpCode::MakeLambda(Box::new(crate::vm::LambdaInfo {
                    params: params.clone(),
                    body_start,
                    body_end,
                })));
            }

            Expr::Namespace { module, name } => {
                // Namespace access: module::name
                // We compile this as a regular variable access with :: separator
                let prefixed_name = format!("{}::{}", module, name);
                self.emit(OpCode::PushVar(prefixed_name));
            }
        }

        Ok(())
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
