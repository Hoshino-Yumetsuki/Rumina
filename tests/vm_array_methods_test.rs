use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use rumina::vm::{ByteCode, OpCode};
use rumina::{Compiler, Lexer, Parser, VM, Value, builtin};

fn run_source_on_vm(source: &str) -> Result<Option<Value>, rumina::RuminaError> {
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().map_err(rumina::RuminaError::runtime)?;
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(ast)?;
    let globals = Rc::new(RefCell::new(HashMap::new()));
    builtin::register_builtins(&mut globals.borrow_mut());
    let mut vm = VM::new(globals);
    vm.load(bytecode);
    vm.run()
}

fn expect_array(result: Option<Value>) -> Vec<Value> {
    match result {
        Some(Value::Array(values)) => values.borrow().clone(),
        other => panic!("Expected array, got {other:?}"),
    }
}

#[test]
fn vm_array_method_map_runs_lambda() {
    let values =
        expect_array(run_source_on_vm("var nums = [1, 2, 3]; nums.map(|x| -> x * x);").unwrap());

    assert_eq!(values, vec![Value::Int(1), Value::Int(4), Value::Int(9)]);
}

#[test]
fn vm_array_method_filter_runs_lambda() {
    let values = expect_array(
        run_source_on_vm("var nums = [1, 2, 3, 4]; nums.filter(|x| -> x % 2 == 0);").unwrap(),
    );

    assert_eq!(values, vec![Value::Int(2), Value::Int(4)]);
}

#[test]
fn vm_array_method_reduce_runs_lambda() {
    let result =
        run_source_on_vm("var nums = [1, 2, 3, 4]; nums.reduce(|acc, x| -> acc + x, 0);").unwrap();

    assert_eq!(result, Some(Value::Int(10)));
}

#[test]
fn vm_array_method_fold_alias_runs_lambda() {
    let result =
        run_source_on_vm("var nums = [1, 2, 3, 4]; nums.fold(|acc, x| -> acc + x, 0);").unwrap();

    assert_eq!(result, Some(Value::Int(10)));
}

#[test]
fn vm_array_method_foreach_runs_lambda_with_index_and_element() {
    let values = expect_array(
        run_source_on_vm(
            "var seen = []; var nums = [4, 5]; nums.foreach(|i, x| -> seen.push(i + x)); seen;",
        )
        .unwrap(),
    );

    assert_eq!(values, vec![Value::Int(4), Value::Int(6)]);
}

#[test]
fn vm_global_array_higher_order_functions_run_lambdas() {
    let mapped =
        expect_array(run_source_on_vm("var nums = [1, 2, 3]; map(nums, |x| -> x * x);").unwrap());
    assert_eq!(mapped, vec![Value::Int(1), Value::Int(4), Value::Int(9)]);

    let filtered = expect_array(
        run_source_on_vm("var nums = [1, 2, 3, 4]; filter(nums, |x| -> x % 2 == 0);").unwrap(),
    );
    assert_eq!(filtered, vec![Value::Int(2), Value::Int(4)]);

    let reduced =
        run_source_on_vm("var nums = [1, 2, 3, 4]; reduce(nums, |acc, x| -> acc + x, 0);").unwrap();
    assert_eq!(reduced, Some(Value::Int(10)));

    let seen = expect_array(
        run_source_on_vm(
            "var seen = []; var nums = [4, 5]; foreach(nums, |i, x| -> seen.push(i + x)); seen;",
        )
        .unwrap(),
    );
    assert_eq!(seen, vec![Value::Int(4), Value::Int(6)]);
}

#[test]
fn vm_index_assign_opcode_updates_array() {
    let array = Value::Array(Rc::new(RefCell::new(vec![Value::Int(1), Value::Int(2)])));
    let mut bytecode = ByteCode::new();
    let array_index = bytecode.add_constant(array);
    let index_index = bytecode.add_constant(Value::Int(1));
    let value_index = bytecode.add_constant(Value::Int(42));

    bytecode.emit(OpCode::PushConstPooled(array_index), None);
    bytecode.emit(OpCode::PushConstPooled(index_index), None);
    bytecode.emit(OpCode::PushConstPooled(value_index), None);
    bytecode.emit(OpCode::IndexAssign, None);
    bytecode.emit(OpCode::PushConstPooled(array_index), None);
    bytecode.emit(OpCode::Halt, None);

    let globals = Rc::new(RefCell::new(HashMap::new()));
    builtin::register_builtins(&mut globals.borrow_mut());
    let mut vm = VM::new(globals);
    vm.load(bytecode);

    let values = expect_array(vm.run().unwrap());

    assert_eq!(values, vec![Value::Int(1), Value::Int(42)]);
}
