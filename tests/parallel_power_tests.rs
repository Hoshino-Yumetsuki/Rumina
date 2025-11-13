// Performance tests for parallel BigInt power operations
use rumina::{Compiler, Interpreter, Lexer, Parser, VM, Value};
use std::time::Instant;

#[test]
fn test_large_power_computation() {
    // Test that large power computations work correctly
    const CODE: &str = r#"
var result = 100^10000;
result;
"#;

    let mut lexer = Lexer::new(CODE.to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(ast).unwrap();
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    let mut vm = VM::new(globals);
    vm.load(bytecode);
    let result = vm.run().unwrap();

    // Verify we got a BigInt result
    match result {
        Some(Value::BigInt(val)) => {
            // 100^10000 should have 20001 digits (10000 * 2 + 1)
            let digits = val.to_string().len();
            assert!(digits > 20000, "Expected > 20000 digits, got {}", digits);
        }
        _ => panic!("Expected BigInt result"),
    }
}

#[test]
fn test_parallel_power_performance() {
    // Test that parallel power computation provides reasonable performance
    // This test measures the time taken for a large power computation
    const CODE: &str = r#"
var result = 114514^50000;
result;
"#;

    let start = Instant::now();
    let mut lexer = Lexer::new(CODE.to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(ast).unwrap();
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    let mut vm = VM::new(globals);
    vm.load(bytecode);
    let result = vm.run().unwrap();
    let elapsed = start.elapsed();

    // Verify we got a result
    match result {
        Some(Value::BigInt(_)) => {
            println!("114514^50000 computed in {:?}", elapsed);
            // Just verify it completes in reasonable time (not a strict performance test)
            // In release mode, this should complete in well under 1 second
            assert!(
                elapsed.as_secs() < 5,
                "Computation took too long: {:?}",
                elapsed
            );
        }
        _ => panic!("Expected BigInt result"),
    }
}

#[test]
fn test_threshold_behavior() {
    // Test that small exponents still work correctly (below parallel threshold)
    const CODE_SMALL: &str = r#"
var result = 2^1000;
result;
"#;

    let mut lexer = Lexer::new(CODE_SMALL.to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(ast).unwrap();
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    let mut vm = VM::new(globals);
    vm.load(bytecode);
    let result = vm.run().unwrap();

    // 2^1000 is a well-known value
    match result {
        Some(Value::BigInt(val)) => {
            let digits = val.to_string().len();
            // 2^1000 has 302 digits
            assert_eq!(digits, 302, "2^1000 should have 302 digits");
        }
        _ => panic!("Expected BigInt result"),
    }
}

#[test]
fn test_very_large_exponent() {
    // Test with an even larger exponent to ensure parallel implementation works
    const CODE: &str = r#"
var result = 2^100000;
result;
"#;

    let start = Instant::now();
    let mut lexer = Lexer::new(CODE.to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(ast).unwrap();
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    let mut vm = VM::new(globals);
    vm.load(bytecode);
    let result = vm.run().unwrap();
    let elapsed = start.elapsed();

    match result {
        Some(Value::BigInt(val)) => {
            println!("2^100000 computed in {:?}", elapsed);
            let digits = val.to_string().len();
            // 2^100000 should have approximately 30103 digits (100000 * log10(2))
            assert!(
                digits > 30000 && digits < 31000,
                "Expected ~30103 digits, got {}",
                digits
            );
        }
        _ => panic!("Expected BigInt result"),
    }
}
