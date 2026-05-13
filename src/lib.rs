pub mod ast;
pub mod builtin;
pub mod bytecode_optimizer;
pub mod compiler;
pub mod error;
pub mod interpreter;
pub mod lexer;
pub mod numeric;
pub mod optimizer;
pub mod parser;
pub mod token;
pub mod value;
pub mod value_ops;
pub mod vm;
pub mod vm_ops;

// WASM 接口模块
#[cfg(target_arch = "wasm32")]
pub mod wasm;

pub use bytecode_optimizer::BytecodeOptimizer;
pub use compiler::Compiler;
pub use error::{ErrorType, RuminaError, StackFrame};
pub use interpreter::Interpreter;
pub use lexer::Lexer;
pub use optimizer::ASTOptimizer;
pub use parser::Parser;
pub use value::Value;
pub use vm::{ByteCode, VM};

/// Run Lamina code using the VM
///
/// This is the primary way to execute Lamina code. It compiles the AST to bytecode
/// and executes it on the VM, returning the result of the last expression.
///
/// # Arguments
/// * `source` - Lamina source code string
///
/// # Returns
/// * `Ok(Some(Value))` - The result of the last expression
/// * `Ok(None)` - No expression result
/// * `Err(RuminaError)` - Compilation or runtime error
pub fn run_rumina(source: &str) -> Result<Option<Value>, RuminaError> {
    run_rumina_with_dir(source, None)
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum BindingKind {
    Var,
    Immutable,
}

fn check_lsr006_lambda_capture_mutation(statements: &[ast::Stmt]) -> Result<(), RuminaError> {
    fn lookup_captured_var(
        scopes: &[std::collections::HashMap<String, BindingKind>],
        name: &str,
    ) -> bool {
        if scopes.last().is_some_and(|scope| scope.contains_key(name)) {
            return false;
        }
        scopes
            .iter()
            .rev()
            .skip(1)
            .find_map(|scope| scope.get(name).copied())
            == Some(BindingKind::Var)
    }

    fn stmt_check(
        stmt: &ast::Stmt,
        scopes: &mut Vec<std::collections::HashMap<String, BindingKind>>,
        lambda_depth: usize,
    ) -> Result<(), RuminaError> {
        match stmt {
            ast::Stmt::VarDecl { name, value, .. } => {
                expr_check(value, scopes, lambda_depth)?;
                if let Some(scope) = scopes.last_mut() {
                    scope.insert(name.clone(), BindingKind::Var);
                }
            }
            ast::Stmt::LetDecl { name, value, .. } => {
                expr_check(value, scopes, lambda_depth)?;
                if let Some(scope) = scopes.last_mut() {
                    scope.insert(name.clone(), BindingKind::Immutable);
                }
            }
            ast::Stmt::Assign { name, value } => {
                expr_check(value, scopes, lambda_depth)?;
                if lambda_depth > 0 && lookup_captured_var(scopes, name) {
                    return Err(RuminaError::runtime(format!(
                        "LambdaCaptureMutation: cannot assign to captured outer var '{}' inside lambda",
                        name
                    )));
                }
            }
            ast::Stmt::MemberAssign { object, value, .. } => {
                expr_check(object, scopes, lambda_depth)?;
                expr_check(value, scopes, lambda_depth)?;
            }
            ast::Stmt::IndexAssign {
                object,
                index,
                value,
            } => {
                expr_check(object, scopes, lambda_depth)?;
                expr_check(index, scopes, lambda_depth)?;
                expr_check(value, scopes, lambda_depth)?;
            }
            ast::Stmt::Expr(expr) | ast::Stmt::Return(Some(expr)) => {
                expr_check(expr, scopes, lambda_depth)?
            }
            ast::Stmt::FuncDef {
                name, params, body, ..
            } => {
                if let Some(scope) = scopes.last_mut() {
                    scope.insert(name.clone(), BindingKind::Var);
                }
                scopes.push(
                    params
                        .iter()
                        .cloned()
                        .map(|param| (param, BindingKind::Var))
                        .collect(),
                );
                for stmt in body {
                    stmt_check(stmt, scopes, lambda_depth)?;
                }
                scopes.pop();
            }
            ast::Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                expr_check(condition, scopes, lambda_depth)?;
                for stmt in then_branch {
                    stmt_check(stmt, scopes, lambda_depth)?;
                }
                if let Some(branch) = else_branch {
                    for stmt in branch {
                        stmt_check(stmt, scopes, lambda_depth)?;
                    }
                }
            }
            ast::Stmt::While { condition, body } => {
                expr_check(condition, scopes, lambda_depth)?;
                for stmt in body {
                    stmt_check(stmt, scopes, lambda_depth)?;
                }
            }
            ast::Stmt::For {
                init,
                condition,
                update,
                body,
            } => {
                scopes.push(Default::default());
                if let Some(init) = init {
                    stmt_check(init, scopes, lambda_depth)?;
                }
                if let Some(condition) = condition {
                    expr_check(condition, scopes, lambda_depth)?;
                }
                if let Some(update) = update {
                    stmt_check(update, scopes, lambda_depth)?;
                }
                for stmt in body {
                    stmt_check(stmt, scopes, lambda_depth)?;
                }
                scopes.pop();
            }
            ast::Stmt::Loop { body } | ast::Stmt::Block(body) => {
                for stmt in body {
                    stmt_check(stmt, scopes, lambda_depth)?;
                }
            }
            ast::Stmt::TryCatch(try_block, _, catch_block) => {
                stmt_check(try_block, scopes, lambda_depth)?;
                stmt_check(catch_block, scopes, lambda_depth)?;
            }
            ast::Stmt::UnitDecl {
                value: Some(value), ..
            } => expr_check(value, scopes, lambda_depth)?,
            ast::Stmt::Return(None)
            | ast::Stmt::ExtensionModule { .. }
            | ast::Stmt::Import { .. }
            | ast::Stmt::Use { .. }
            | ast::Stmt::UnitDecl { value: None, .. }
            | ast::Stmt::Break
            | ast::Stmt::Continue
            | ast::Stmt::Include(_)
            | ast::Stmt::Empty => {}
        }
        Ok(())
    }

    fn expr_check(
        expr: &ast::Expr,
        scopes: &mut Vec<std::collections::HashMap<String, BindingKind>>,
        lambda_depth: usize,
    ) -> Result<(), RuminaError> {
        match expr {
            ast::Expr::Array(items)
            | ast::Expr::Vector(items)
            | ast::Expr::Set(items)
            | ast::Expr::Multi(items) => {
                for item in items {
                    expr_check(item, scopes, lambda_depth)?;
                }
            }
            ast::Expr::Matrix(rows) => {
                for row in rows {
                    for item in row {
                        expr_check(item, scopes, lambda_depth)?;
                    }
                }
            }
            ast::Expr::Struct(fields) => {
                for (_, value) in fields {
                    expr_check(value, scopes, lambda_depth)?;
                }
            }
            ast::Expr::Table(fields) => {
                for (key, value) in fields {
                    expr_check(key, scopes, lambda_depth)?;
                    expr_check(value, scopes, lambda_depth)?;
                }
            }
            ast::Expr::Binary { left, right, .. } => {
                expr_check(left, scopes, lambda_depth)?;
                expr_check(right, scopes, lambda_depth)?;
            }
            ast::Expr::Unary { expr, .. }
            | ast::Expr::UnitStrip { expr, .. }
            | ast::Expr::UnitConvert { expr, .. }
            | ast::Expr::UnitAttach { expr, .. }
            | ast::Expr::Try(expr) => expr_check(expr, scopes, lambda_depth)?,
            ast::Expr::Range { start, end } => {
                expr_check(start, scopes, lambda_depth)?;
                expr_check(end, scopes, lambda_depth)?;
            }
            ast::Expr::Call { func, args } => {
                expr_check(func, scopes, lambda_depth)?;
                for arg in args {
                    expr_check(arg, scopes, lambda_depth)?;
                }
            }
            ast::Expr::Member { object, .. } => expr_check(object, scopes, lambda_depth)?,
            ast::Expr::Index { object, index } => {
                expr_check(object, scopes, lambda_depth)?;
                expr_check(index, scopes, lambda_depth)?;
            }
            ast::Expr::Lambda { params, body, .. } => {
                scopes.push(
                    params
                        .iter()
                        .cloned()
                        .map(|param| (param, BindingKind::Var))
                        .collect(),
                );
                stmt_check(body, scopes, lambda_depth + 1)?;
                scopes.pop();
            }
            ast::Expr::Match { target, arms } => {
                expr_check(target, scopes, lambda_depth)?;
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        expr_check(guard, scopes, lambda_depth)?;
                    }
                    expr_check(&arm.expr, scopes, lambda_depth)?;
                }
            }
            ast::Expr::Int(_)
            | ast::Expr::BigInt(_)
            | ast::Expr::Float(_)
            | ast::Expr::String(_)
            | ast::Expr::Bool(_)
            | ast::Expr::Null
            | ast::Expr::Ident(_)
            | ast::Expr::Wildcard
            | ast::Expr::Namespace { .. } => {}
        }
        Ok(())
    }

    let mut scopes = vec![std::collections::HashMap::new()];
    for stmt in statements {
        stmt_check(stmt, &mut scopes, 0)?;
    }
    Ok(())
}

fn should_use_interpreter_runtime(statements: &[ast::Stmt]) -> bool {
    fn stmt_requires_interpreter(stmt: &ast::Stmt) -> bool {
        match stmt {
            ast::Stmt::Include(path) => path.starts_with("rumina:"),
            ast::Stmt::ExtensionModule { .. } => true,
            ast::Stmt::Import { .. } | ast::Stmt::Use { .. } => true,
            ast::Stmt::UnitDecl { .. } => true,
            ast::Stmt::TryCatch(_, _, _) => true,
            ast::Stmt::Expr(expr)
            | ast::Stmt::VarDecl { value: expr, .. }
            | ast::Stmt::LetDecl { value: expr, .. }
            | ast::Stmt::Assign { value: expr, .. } => expr_requires_interpreter(expr),
            ast::Stmt::MemberAssign { object, value, .. } => {
                expr_requires_interpreter(object) || expr_requires_interpreter(value)
            }
            ast::Stmt::FuncDef { body, .. } | ast::Stmt::Block(body) => {
                body.iter().any(stmt_requires_interpreter)
            }
            ast::Stmt::Return(Some(expr)) => expr_requires_interpreter(expr),
            ast::Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                expr_requires_interpreter(condition)
                    || then_branch.iter().any(stmt_requires_interpreter)
                    || else_branch
                        .as_ref()
                        .map(|branch| branch.iter().any(stmt_requires_interpreter))
                        .unwrap_or(false)
            }
            ast::Stmt::While { condition, body } => {
                expr_requires_interpreter(condition) || body.iter().any(stmt_requires_interpreter)
            }
            ast::Stmt::For {
                init,
                condition,
                update,
                body,
            } => {
                init.as_ref()
                    .map(|stmt| stmt_requires_interpreter(stmt))
                    .unwrap_or(false)
                    || condition
                        .as_ref()
                        .map(expr_requires_interpreter)
                        .unwrap_or(false)
                    || update
                        .as_ref()
                        .map(|stmt| stmt_requires_interpreter(stmt))
                        .unwrap_or(false)
                    || body.iter().any(stmt_requires_interpreter)
            }
            ast::Stmt::Loop { body } => body.iter().any(stmt_requires_interpreter),
            _ => false,
        }
    }

    fn expr_requires_interpreter(expr: &ast::Expr) -> bool {
        match expr {
            ast::Expr::Try(_) => true,
            ast::Expr::Wildcard => true,
            ast::Expr::Range { .. } => true,
            ast::Expr::Member { object, .. } => {
                matches!(object.as_ref(), ast::Expr::Array(_)) || expr_requires_interpreter(object)
            }
            ast::Expr::Call { func, args } => {
                matches!(func.as_ref(), ast::Expr::Member { .. })
                    || expr_requires_interpreter(func)
                    || args.iter().any(expr_requires_interpreter)
            }
            ast::Expr::Binary { left, right, .. } => {
                matches!(
                    expr,
                    ast::Expr::Binary {
                        op: ast::BinOp::Pipe | ast::BinOp::Equivalent,
                        ..
                    }
                ) || expr_requires_interpreter(left)
                    || expr_requires_interpreter(right)
            }
            ast::Expr::Index { object, index } => {
                expr_requires_interpreter(object) || expr_requires_interpreter(index)
            }
            ast::Expr::Unary { expr, .. } => expr_requires_interpreter(expr),
            ast::Expr::UnitStrip { .. } => true,
            ast::Expr::UnitConvert { .. } => true,
            ast::Expr::UnitAttach { .. } => true,
            ast::Expr::Array(items) => items.iter().any(expr_requires_interpreter),
            ast::Expr::Vector(_) => true,
            ast::Expr::Matrix(_) => true,
            ast::Expr::Set(_) => true,
            ast::Expr::Struct(fields) => fields
                .iter()
                .any(|(_, expr)| expr_requires_interpreter(expr)),
            ast::Expr::Table(_) => true,
            ast::Expr::Lambda { body, .. } => stmt_requires_interpreter(body),
            ast::Expr::Match { .. } => true,
            ast::Expr::Multi(_) => true,
            _ => false,
        }
    }

    statements.iter().any(stmt_requires_interpreter)
}

/// Run Lamina code using the VM with a specific working directory
///
/// This allows proper resolution of include statements with relative paths.
///
/// # Arguments
/// * `source` - Lamina source code string
/// * `current_dir` - Optional current directory for resolving includes
///
/// # Returns
/// * `Ok(Some(Value))` - The result of the last expression
/// * `Ok(None)` - No expression result
/// * `Err(RuminaError)` - Compilation or runtime error
pub fn run_rumina_with_dir(
    source: &str,
    current_dir: Option<String>,
) -> Result<Option<Value>, RuminaError> {
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse().map_err(RuminaError::runtime)?;
    check_lsr006_lambda_capture_mutation(&ast)?;

    if should_use_interpreter_runtime(&ast) {
        let mut interpreter = Interpreter::new();
        return interpreter.interpret(ast);
    }

    // Apply AST optimization passes
    let mut optimizer = ASTOptimizer::new();
    let optimized_ast = optimizer.optimize(ast)?;

    // Compile to bytecode with directory context
    let mut compiler = if let Some(dir) = current_dir {
        Compiler::with_current_dir(dir)
    } else {
        Compiler::new()
    };
    let mut bytecode = compiler.compile(optimized_ast)?;

    // Apply bytecode optimization passes
    let mut bytecode_optimizer = BytecodeOptimizer::new();
    bytecode_optimizer.optimize(&mut bytecode);

    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    let mut vm = VM::new(globals);
    vm.load(bytecode);

    vm.run()
}

/// Run Lamina code (backward compatibility wrapper)
///
/// This is the main entry point for executing Lamina code. It uses the VM
/// for execution and discards the return value for backward compatibility.
pub fn run(source: &str) -> Result<(), RuminaError> {
    run_rumina(source)?;
    Ok(())
}

#[cfg(test)]
mod vm_integration_tests {
    use super::*;

    #[test]
    fn test_run_rumina_basic() {
        let result = run_rumina("10 + 20;").unwrap();
        match result {
            Some(Value::Int(n)) => assert_eq!(n, 30),
            _ => panic!("Expected Int(30)"),
        }
    }

    #[test]
    fn test_run_compatibility() {
        // run() should work without returning value
        let result = run("15 + 15;");
        assert!(result.is_ok());
    }

    #[test]
    fn test_run_with_variables() {
        let result = run_rumina("var x = 10; var y = 20; x + y;").unwrap();
        match result {
            Some(Value::Int(n)) => assert_eq!(n, 30),
            _ => panic!("Expected Int(30)"),
        }
    }

    #[test]
    fn test_run_with_builtins() {
        let result = run_rumina("abs(-42);").unwrap();
        match result {
            Some(Value::Int(n)) => assert_eq!(n, 42),
            _ => panic!("Expected Int(42)"),
        }
    }

    #[test]
    fn test_run_include_returns_final_if_branch_expression() {
        let result = run_rumina(
            r#"
include "rumina:path";

if (path.basename("/tmp/demo.txt") == "demo.txt") {
    "ok";
} else {
    "bad";
}
"#,
        )
        .unwrap();

        match result {
            Some(Value::String(s)) => assert_eq!(s, "ok"),
            other => panic!("Expected Some(String), got {:?}", other),
        }
    }

    #[test]
    fn test_trig_with_bigint() {
        // Test cos with bigint - should not panic with "Cannot convert bigint to float"
        let result = run_rumina("cos(10^10);").unwrap();
        match result {
            Some(Value::Float(_)) => (), // Should return a float, actual value doesn't matter
            _ => panic!("Expected Float result from cos(10^10)"),
        }
    }

    #[test]
    fn test_sin_with_bigint() {
        // Test sin with bigint
        let result = run_rumina("sin(10^10);").unwrap();
        match result {
            Some(Value::Float(_)) => (), // Should return a float
            _ => panic!("Expected Float result from sin(10^10)"),
        }
    }

    #[test]
    fn test_tan_with_bigint() {
        // Test tan with bigint
        let result = run_rumina("tan(10^10);").unwrap();
        match result {
            Some(Value::Float(_)) => (), // Should return a float
            _ => panic!("Expected Float result from tan(10^10)"),
        }
    }

    #[test]
    fn test_exp_with_bigint() {
        // Test exp with bigint - should return inf for large values
        let result = run_rumina("exp(10^10);").unwrap();
        match result {
            Some(Value::Float(f)) => assert!(f.is_infinite()),
            _ => panic!("Expected Float result from exp(10^10)"),
        }
    }

    #[test]
    fn test_log_with_bigint() {
        // Test log with bigint
        let result = run_rumina("log(10^10);").unwrap();
        match result {
            Some(Value::Float(_)) => (), // Should return a float
            _ => panic!("Expected Float result from log(10^10)"),
        }
    }

    #[test]
    fn test_cos_with_very_large_bigint() {
        // Test with extremely large bigint (like the original issue)
        // This should overflow to infinity and cos(infinity) = NaN
        let result = run_rumina("cos(114514^114514);").unwrap();
        match result {
            Some(Value::Float(f)) => assert!(f.is_nan()),
            _ => panic!("Expected Float NaN result from cos(114514^114514)"),
        }
    }
}
