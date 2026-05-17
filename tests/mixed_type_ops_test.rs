use rumina::ast::BinOp;
use rumina::interpreter::Interpreter;
use rumina::numeric::{BigInt, rational_from_integer};
use rumina::value::Value;

#[test]
fn test_int_float_mod() {
    let mut interpreter = Interpreter::new();
    let int_val = Value::Int(10);
    let float_val = Value::Float(3.5);

    let result = interpreter.eval_binary_op(&int_val, BinOp::Mod, &float_val);
    assert!(result.is_ok(), "Int % Float should be supported");
    if let Ok(Value::Float(f)) = result {
        assert!((f - 3.0).abs() < 1e-10);
    } else {
        panic!("Expected Float result, got {:?}", result);
    }
}

#[test]
fn test_int_float_comparison() {
    let mut interpreter = Interpreter::new();
    let int_val = Value::Int(1);
    let float_val = Value::Float(1.0);

    let result = interpreter.eval_binary_op(&int_val, BinOp::Equal, &float_val);
    assert!(result.is_ok(), "Int == Float should be supported");
    assert_eq!(result.unwrap(), Value::Bool(true));
}

#[test]
fn test_float_int_sub() {
    let mut interpreter = Interpreter::new();
    let float_val = Value::Float(3.5);
    let int_val = Value::Int(1);

    let result = interpreter.eval_binary_op(&float_val, BinOp::Sub, &int_val);
    assert!(result.is_ok());
    if let Ok(Value::Float(f)) = result {
        assert!((f - 2.5).abs() < 1e-10, "Expected 2.5, got {}", f);
    } else {
        panic!("Expected Float result");
    }
}

#[test]
fn test_int_rational_mod() {
    let mut interpreter = Interpreter::new();
    let int_val = Value::Int(10);
    let rational_val = Value::Rational(rational_from_integer(BigInt::from(3)));

    let result = interpreter.eval_binary_op(&int_val, BinOp::Mod, &rational_val);
    assert!(result.is_ok(), "Int % Rational should be supported");
    if let Ok(Value::Rational(r)) = result {
        assert_eq!(r, rational_from_integer(BigInt::from(1)));
    } else {
        panic!("Expected Rational result, got {:?}", result);
    }
}
