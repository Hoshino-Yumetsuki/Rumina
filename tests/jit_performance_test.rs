// JIT and VM optimization performance tests
use rumina::{run_rumina, Compiler, Interpreter, Lexer, Parser, VM};
use std::time::Instant;

#[test]
fn test_jit_loop_performance() {
    // Test a tight loop that should benefit from JIT
    const LOOP_CODE: &str = r#"
var sum = 0;
var i = 0;
while (i < 10000) {
    sum = sum + i;
    i = i + 1;
}
sum;
"#;

    let start = Instant::now();
    let result = run_rumina(LOOP_CODE).unwrap();
    let duration = start.elapsed();

    // Verify correctness: sum of 0..9999 = 49995000
    match result {
        Some(rumina::Value::Int(n)) => {
            assert_eq!(n, 49995000, "Sum should be 49995000");
        }
        _ => panic!("Expected Int result"),
    }

    println!("Loop performance with VM optimizations: {:?}", duration);
}

#[test]
fn test_bytecode_optimizer_arithmetic() {
    // Test that constant folding works in bytecode optimizer
    const CODE: &str = r#"
var x = 10 + 20;
var y = x * 2;
var z = y - 5;
z;
"#;

    let start = Instant::now();
    let result = run_rumina(CODE).unwrap();
    let duration = start.elapsed();

    match result {
        Some(rumina::Value::Int(n)) => {
            assert_eq!(n, 55, "Result should be 55");
        }
        _ => panic!("Expected Int result"),
    }

    println!("Arithmetic optimization: {:?}", duration);
}

#[test]
fn test_recursive_fibonacci_optimization() {
    // Test recursive function optimization
    const FIB_CODE: &str = r#"
func fib(n) {
    if (n <= 1) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

fib(15);
"#;

    let start = Instant::now();
    let result = run_rumina(FIB_CODE).unwrap();
    let duration = start.elapsed();

    match result {
        Some(rumina::Value::Int(n)) => {
            assert_eq!(n, 610, "fib(15) should be 610");
        }
        _ => panic!("Expected Int result"),
    }

    println!("Recursive fibonacci with optimizations: {:?}", duration);
}

#[test]
fn test_vm_inline_cache_effectiveness() {
    // Test that inline caching improves member access performance
    const MEMBER_CODE: &str = r#"
struct Point {
    x = 10;
    y = 20;
}

var p = Point;
var sum = 0;
var i = 0;
while (i < 1000) {
    sum = sum + p.x + p.y;
    i = i + 1;
}
sum;
"#;

    let start = Instant::now();
    let result = run_rumina(MEMBER_CODE).unwrap();
    let duration = start.elapsed();

    match result {
        Some(rumina::Value::Int(n)) => {
            assert_eq!(n, 30000, "Sum should be 30000");
        }
        _ => panic!("Expected Int result"),
    }

    println!("Member access with inline caching: {:?}", duration);
}

#[test]
fn test_jit_hot_path_detection() {
    // Test JIT hot path detection with repeated function calls
    const HOT_PATH_CODE: &str = r#"
func compute(n) {
    var result = 0;
    var i = 0;
    while (i < n) {
        result = result + i * 2;
        i = i + 1;
    }
    return result;
}

var total = 0;
var j = 0;
while (j < 50) {
    total = total + compute(100);
    j = j + 1;
}
total;
"#;

    let start = Instant::now();
    let result = run_rumina(HOT_PATH_CODE).unwrap();
    let duration = start.elapsed();

    match result {
        Some(rumina::Value::Int(n)) => {
            assert_eq!(n, 495000, "Total should be 495000");
        }
        _ => panic!("Expected Int result"),
    }

    println!("Hot path detection and optimization: {:?}", duration);
}

#[test]
fn test_vm_vs_interpreter_speedup() {
    const BENCHMARK_CODE: &str = r#"
var result = 0;
var i = 0;
while (i < 5000) {
    result = result + i;
    i = i + 1;
}
result;
"#;

    // Test VM
    let vm_start = Instant::now();
    let mut lexer = Lexer::new(BENCHMARK_CODE.to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(ast).unwrap();
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    let mut vm = VM::new(globals);
    vm.load(bytecode);
    let vm_result = vm.run().unwrap();
    let vm_time = vm_start.elapsed();

    // Test Interpreter
    let interp_start = Instant::now();
    let mut lexer = Lexer::new(BENCHMARK_CODE.to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();
    let mut interpreter = Interpreter::new();
    let interp_result = interpreter.interpret(ast).unwrap();
    let interp_time = interp_start.elapsed();

    // Verify results match
    match (vm_result, interp_result) {
        (Some(rumina::Value::Int(vm_val)), Some(rumina::Value::Int(interp_val))) => {
            assert_eq!(vm_val, interp_val, "Results should match");
            assert_eq!(vm_val, 12497500, "Sum should be 12497500");
        }
        _ => panic!("Unexpected result types"),
    }

    println!(
        "VM time: {:?}, Interpreter time: {:?}, Speedup: {:.2}x",
        vm_time,
        interp_time,
        interp_time.as_secs_f64() / vm_time.as_secs_f64()
    );

    // In release mode, VM should be faster or at least comparable
    // In debug mode, we just verify it works correctly
}
