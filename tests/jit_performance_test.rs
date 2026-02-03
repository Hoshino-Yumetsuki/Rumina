// JIT Performance Tests - Compare JIT-enabled vs JIT-disabled execution
use rumina::{Compiler, Interpreter, Lexer, Parser, Value, VM};
use std::time::Instant;

#[test]
fn test_jit_simple_loop_performance() {
    const LOOP_CODE: &str = r#"
var sum = 0;
var i = 0;
while (i < 1000) {
    sum = sum + i;
    i = i + 1;
}
sum;
"#;

    // Test with JIT enabled
    let jit_start = Instant::now();
    let mut lexer = Lexer::new(LOOP_CODE.to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(ast).unwrap();
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    let mut vm_jit = VM::new(globals);
    vm_jit.set_jit_enabled(true);
    vm_jit.load(bytecode.clone());
    let jit_result = vm_jit.run().unwrap();
    let jit_time = jit_start.elapsed();

    // Test with JIT disabled
    let no_jit_start = Instant::now();
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    let mut vm_no_jit = VM::new(globals);
    vm_no_jit.set_jit_enabled(false);
    vm_no_jit.load(bytecode);
    let no_jit_result = vm_no_jit.run().unwrap();
    let no_jit_time = no_jit_start.elapsed();

    // Verify results match
    match (jit_result, no_jit_result) {
        (Some(Value::Int(jit_val)), Some(Value::Int(no_jit_val))) => {
            assert_eq!(jit_val, no_jit_val, "JIT and non-JIT results should match");
            assert_eq!(jit_val, 499500, "Sum should be 499500");
        }
        _ => panic!("Unexpected result types"),
    }

    println!(
        "Simple loop: JIT time: {:?}, No-JIT time: {:?}, Speedup: {:.2}x",
        jit_time,
        no_jit_time,
        no_jit_time.as_secs_f64() / jit_time.as_secs_f64()
    );

    // Get JIT stats
    let stats = vm_jit.jit_stats();
    println!(
        "JIT Stats - Hot spots: {}, Compiled traces: {}, Total executions: {}",
        stats.hot_spots, stats.compiled_traces, stats.total_executions
    );
}

#[test]
fn test_jit_arithmetic_intensive() {
    const ARITHMETIC_CODE: &str = r#"
var result = 0;
var i = 0;
while (i < 500) {
    result = result + i * 2 - 1;
    i = i + 1;
}
result;
"#;

    // Test with JIT enabled
    let jit_start = Instant::now();
    let mut lexer = Lexer::new(ARITHMETIC_CODE.to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(ast).unwrap();
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    let mut vm_jit = VM::new(globals);
    vm_jit.set_jit_enabled(true);
    vm_jit.load(bytecode.clone());
    let jit_result = vm_jit.run().unwrap();
    let jit_time = jit_start.elapsed();

    // Test with JIT disabled
    let no_jit_start = Instant::now();
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    let mut vm_no_jit = VM::new(globals);
    vm_no_jit.set_jit_enabled(false);
    vm_no_jit.load(bytecode);
    let no_jit_result = vm_no_jit.run().unwrap();
    let no_jit_time = no_jit_start.elapsed();

    // Verify results match
    match (jit_result, no_jit_result) {
        (Some(Value::Int(jit_val)), Some(Value::Int(no_jit_val))) => {
            assert_eq!(jit_val, no_jit_val, "JIT and non-JIT results should match");
            assert_eq!(jit_val, 249000, "Result should be 249000");
        }
        _ => panic!("Unexpected result types"),
    }

    println!(
        "Arithmetic intensive: JIT time: {:?}, No-JIT time: {:?}, Speedup: {:.2}x",
        jit_time,
        no_jit_time,
        no_jit_time.as_secs_f64() / jit_time.as_secs_f64()
    );

    // Get JIT stats
    let stats = vm_jit.jit_stats();
    println!(
        "JIT Stats - Hot spots: {}, Compiled traces: {}, Total executions: {}",
        stats.hot_spots, stats.compiled_traces, stats.total_executions
    );
}

#[test]
fn test_jit_nested_loops() {
    const NESTED_CODE: &str = r#"
var total = 0;
var i = 0;
while (i < 50) {
    var j = 0;
    while (j < 50) {
        total = total + 1;
        j = j + 1;
    }
    i = i + 1;
}
total;
"#;

    // Test with JIT enabled
    let jit_start = Instant::now();
    let mut lexer = Lexer::new(NESTED_CODE.to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(ast).unwrap();
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    let mut vm_jit = VM::new(globals);
    vm_jit.set_jit_enabled(true);
    vm_jit.load(bytecode.clone());
    let jit_result = vm_jit.run().unwrap();
    let jit_time = jit_start.elapsed();

    // Test with JIT disabled
    let no_jit_start = Instant::now();
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    let mut vm_no_jit = VM::new(globals);
    vm_no_jit.set_jit_enabled(false);
    vm_no_jit.load(bytecode);
    let no_jit_result = vm_no_jit.run().unwrap();
    let no_jit_time = no_jit_start.elapsed();

    // Verify results match
    match (jit_result, no_jit_result) {
        (Some(Value::Int(jit_val)), Some(Value::Int(no_jit_val))) => {
            assert_eq!(jit_val, no_jit_val, "JIT and non-JIT results should match");
            assert_eq!(jit_val, 2500, "Total should be 2500");
        }
        _ => panic!("Unexpected result types"),
    }

    println!(
        "Nested loops: JIT time: {:?}, No-JIT time: {:?}, Speedup: {:.2}x",
        jit_time,
        no_jit_time,
        no_jit_time.as_secs_f64() / jit_time.as_secs_f64()
    );

    // Get JIT stats
    let stats = vm_jit.jit_stats();
    println!(
        "JIT Stats - Hot spots: {}, Compiled traces: {}, Total executions: {}",
        stats.hot_spots, stats.compiled_traces, stats.total_executions
    );
}

#[test]
fn test_jit_variable_operations() {
    const VAR_CODE: &str = r#"
var a = 10;
var b = 20;
var c = 30;
var result = 0;
var i = 0;
while (i < 500) {
    result = a + b + c;
    a = a + 1;
    b = b - 1;
    c = c + 2;
    i = i + 1;
}
result;
"#;

    // Test with JIT enabled
    let jit_start = Instant::now();
    let mut lexer = Lexer::new(VAR_CODE.to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(ast).unwrap();
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    let mut vm_jit = VM::new(globals);
    vm_jit.set_jit_enabled(true);
    vm_jit.load(bytecode.clone());
    let jit_result = vm_jit.run().unwrap();
    let jit_time = jit_start.elapsed();

    // Test with JIT disabled
    let no_jit_start = Instant::now();
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    let mut vm_no_jit = VM::new(globals);
    vm_no_jit.set_jit_enabled(false);
    vm_no_jit.load(bytecode);
    let no_jit_result = vm_no_jit.run().unwrap();
    let no_jit_time = no_jit_start.elapsed();

    // Verify results match
    match (jit_result, no_jit_result) {
        (Some(Value::Int(jit_val)), Some(Value::Int(no_jit_val))) => {
            assert_eq!(jit_val, no_jit_val, "JIT and non-JIT results should match");
        }
        _ => panic!("Unexpected result types"),
    }

    println!(
        "Variable operations: JIT time: {:?}, No-JIT time: {:?}, Speedup: {:.2}x",
        jit_time,
        no_jit_time,
        no_jit_time.as_secs_f64() / jit_time.as_secs_f64()
    );

    // Get JIT stats
    let stats = vm_jit.jit_stats();
    println!(
        "JIT Stats - Hot spots: {}, Compiled traces: {}, Total executions: {}",
        stats.hot_spots, stats.compiled_traces, stats.total_executions
    );
}

#[test]
fn test_jit_correctness_complex() {
    // Test various code patterns to ensure JIT doesn't break correctness
    const PATTERNS: &[&str] = &[
        "var x = 5; x + 10;",
        "var x = 0; var i = 0; while (i < 10) { x = x + i; i = i + 1; } x;",
        "var a = 1; var b = 2; var c = a + b; c * 3;",
        "var sum = 0; var i = 0; while (i < 5) { sum = sum + i * 2; i = i + 1; } sum;",
    ];

    for (idx, &code) in PATTERNS.iter().enumerate() {
        // Run with JIT
        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let mut compiler = Compiler::new();
        let bytecode = compiler.compile(ast).unwrap();
        let interpreter = Interpreter::new();
        let globals = interpreter.get_globals();
        let mut vm_jit = VM::new(globals);
        vm_jit.set_jit_enabled(true);
        vm_jit.load(bytecode.clone());
        let jit_result = vm_jit.run().unwrap();

        // Run without JIT
        let interpreter = Interpreter::new();
        let globals = interpreter.get_globals();
        let mut vm_no_jit = VM::new(globals);
        vm_no_jit.set_jit_enabled(false);
        vm_no_jit.load(bytecode);
        let no_jit_result = vm_no_jit.run().unwrap();

        // Verify results match
        assert_eq!(
            jit_result, no_jit_result,
            "Pattern {} failed: JIT and non-JIT results don't match",
            idx
        );
        println!("Pattern {} passed correctness test", idx);
    }
}
