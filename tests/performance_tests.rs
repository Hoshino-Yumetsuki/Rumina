// Performance tests for VM vs Interpreter
use rumina::{Compiler, Interpreter, Lexer, Parser, Value, VM};
use std::time::Instant;

#[test]
fn test_vm_performance_fibonacci() {
    const FIB_CODE: &str = r#"
func fib(n) {
    if (n <= 1) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

fib(20);
"#;

    // Test VM
    let vm_start = Instant::now();
    let mut lexer = Lexer::new(FIB_CODE.to_string());
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
    let mut lexer = Lexer::new(FIB_CODE.to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();
    let mut interpreter = Interpreter::new();
    let interp_result = interpreter.interpret(ast).unwrap();
    let interp_time = interp_start.elapsed();

    // Verify results match
    match (vm_result, interp_result) {
        (Some(Value::Int(vm_val)), Some(Value::Int(interp_val))) => {
            assert_eq!(vm_val, interp_val, "Results should match");
            assert_eq!(vm_val, 6765, "fib(20) should be 6765");
        }
        _ => panic!("Unexpected result types"),
    }

    // VM should be faster (or at least not significantly slower)
    println!("VM time: {:?}, Interpreter time: {:?}", vm_time, interp_time);
    
    // In debug mode, just verify the VM completes successfully
    // Performance comparison is more meaningful in release mode
}

#[test]
fn test_vm_arithmetic_performance() {
    const ARITHMETIC_CODE: &str = r#"
var sum = 0;
var i = 0;
while (i < 1000) {
    sum = sum + i;
    i = i + 1;
}
sum;
"#;

    // Test VM
    let vm_start = Instant::now();
    let mut lexer = Lexer::new(ARITHMETIC_CODE.to_string());
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
    let mut lexer = Lexer::new(ARITHMETIC_CODE.to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().unwrap();
    let mut interpreter = Interpreter::new();
    let interp_result = interpreter.interpret(ast).unwrap();
    let interp_time = interp_start.elapsed();

    // Verify results match
    match (vm_result, interp_result) {
        (Some(Value::Int(vm_val)), Some(Value::Int(interp_val))) => {
            assert_eq!(vm_val, interp_val, "Results should match");
            assert_eq!(vm_val, 499500, "Sum should be 499500");
        }
        _ => panic!("Unexpected result types"),
    }

    println!(
        "Arithmetic: VM time: {:?}, Interpreter time: {:?}",
        vm_time, interp_time
    );
}
