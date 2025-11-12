pub mod ast;
pub mod builtin;
pub mod compiler;
pub mod error;
pub mod interpreter;
pub mod lexer;
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
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse().map_err(|e| RuminaError::runtime(e))?;

    // Apply optimization passes
    let mut optimizer = ASTOptimizer::new();
    let optimized_ast = optimizer.optimize(ast)?;

    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(optimized_ast)?;

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
}
