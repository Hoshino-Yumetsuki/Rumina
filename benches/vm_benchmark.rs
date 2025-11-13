use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rumina::{Compiler, Interpreter, Lexer, Parser, VM, Value};

fn fibonacci_benchmark(c: &mut Criterion) {
    let code = r#"
func fib(n) {
    if (n <= 1) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

fib(15);
"#;

    c.bench_function("fibonacci_15", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(code.to_string());
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
            black_box(result)
        });
    });
}

fn arithmetic_loop_benchmark(c: &mut Criterion) {
    let code = r#"
var sum = 0;
var i = 0;
while (i < 1000) {
    sum = sum + i;
    i = i + 1;
}
sum;
"#;

    c.bench_function("arithmetic_loop_1000", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(code.to_string());
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
            black_box(result)
        });
    });
}

fn nested_loop_benchmark(c: &mut Criterion) {
    let code = r#"
var sum = 0;
var i = 0;
while (i < 50) {
    var j = 0;
    while (j < 50) {
        sum = sum + 1;
        j = j + 1;
    }
    i = i + 1;
}
sum;
"#;

    c.bench_function("nested_loop_50x50", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(code.to_string());
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
            black_box(result)
        });
    });
}

fn function_call_overhead_benchmark(c: &mut Criterion) {
    let code = r#"
func add(a, b) {
    return a + b;
}

var sum = 0;
var i = 0;
while (i < 100) {
    sum = add(sum, i);
    i = i + 1;
}
sum;
"#;

    c.bench_function("function_calls_100", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(code.to_string());
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
            black_box(result)
        });
    });
}

fn array_operations_benchmark(c: &mut Criterion) {
    let code = r#"
var arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
var sum = 0;
var i = 0;
while (i < 100) {
    var j = 0;
    while (j < 10) {
        sum = sum + arr[j];
        j = j + 1;
    }
    i = i + 1;
}
sum;
"#;

    c.bench_function("array_access_100x10", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(code.to_string());
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
            black_box(result)
        });
    });
}

fn constant_folding_benchmark(c: &mut Criterion) {
    // Test code with many constant expressions that should be optimized
    let code = r#"
var x = 2 + 3 * 4 - 1;
var y = 10 / 2 + 5;
var z = x * y;
z;
"#;

    c.bench_function("constant_folding", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(code.to_string());
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
            black_box(result)
        });
    });
}

criterion_group!(
    benches,
    fibonacci_benchmark,
    arithmetic_loop_benchmark,
    nested_loop_benchmark,
    function_call_overhead_benchmark,
    array_operations_benchmark,
    constant_folding_benchmark
);
criterion_main!(benches);
