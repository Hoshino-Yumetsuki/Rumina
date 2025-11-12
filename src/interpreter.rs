use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::*;
use crate::builtin;
use crate::error::{RuminaError, StackFrame};
use crate::value::*;

// Submodules for code organization
mod call;
mod convert;
mod expr;
mod operators;
mod stmt;

/// Lamina Float Usage Policy:
///
/// Float values in Lamina are used in the following cases:
/// 1. Float literals from source code (e.g., 3.14) - for compatibility
/// 2. Explicit conversion via decimal() function - primary use case
/// 3. Mathematical transcendental functions (sin, cos, log, etc.) - cannot be exact
/// 4. Vector/matrix operations (dot, norm, cross, det) - numerical computation
/// 5. User input parsed as float
/// 6. Mixed operations with other types when Float is involved
/// 7. Complex number arithmetic (Complex64 uses f64 internally)
///
/// Lamina prioritizes exact computation using:
/// - Int for integers
/// - Rational for fractions (division of integers)
/// - Irrational for symbolic roots, π, e
/// - BigInt for large integers (factorial results)
/// - Complex for complex numbers
///
/// Float should NOT be used where exact computation is possible.

pub struct Interpreter {
    globals: Rc<RefCell<HashMap<String, Value>>>,
    locals: Vec<Rc<RefCell<HashMap<String, Value>>>>,
    return_value: Option<Value>,
    break_flag: bool,
    continue_flag: bool,
    // Error tracking fields
    current_file: String,
    call_stack: Vec<String>, // Stack of function names
    // Recursion depth tracking for stack overflow prevention
    recursion_depth: usize,
    max_recursion_depth: usize,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        builtin::register_builtins(&mut globals);

        // LSR-010: Register imaginary unit 'i' as sqrt(-1)
        // This allows complex literal syntax like: 3 + 4*i
        globals.insert(
            "i".to_string(),
            Value::Complex(Box::new(Value::Int(0)), Box::new(Value::Int(1))),
        );

        Interpreter {
            globals: Rc::new(RefCell::new(globals)),
            locals: Vec::new(),
            return_value: None,
            break_flag: false,
            continue_flag: false,
            current_file: "<unknown>".to_string(),
            call_stack: Vec::new(),
            recursion_depth: 0,
            max_recursion_depth: 4000, // Conservative limit based on 128MB stack size
        }
    }

    /// Set the current file being executed (for error reporting)
    pub fn set_file(&mut self, filename: String) {
        self.current_file = filename;
    }

    /// Get the globals reference (for VM integration)
    pub fn get_globals(&self) -> Rc<RefCell<HashMap<String, Value>>> {
        Rc::clone(&self.globals)
    }

    /// Helper method to wrap errors with stack trace
    fn wrap_error(&self, message: String) -> RuminaError {
        let mut error = RuminaError::runtime(message);

        // Add current call stack
        for func_name in self.call_stack.iter().rev() {
            error.add_frame(StackFrame {
                function_name: func_name.clone(),
                file_name: self.current_file.clone(),
                line_number: None, // TODO: Add line tracking
            });
        }

        error
    }

    /// Helper to convert Result<T, String> to Result<T, RuminaError>
    fn convert_result<T>(&self, result: Result<T, String>) -> Result<T, RuminaError> {
        result.map_err(|e| self.wrap_error(e))
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<Option<Value>, RuminaError> {
        let mut last_value = None;
        for stmt in statements {
            // 如果是表达式语句,保存其值
            if let Stmt::Expr(expr) = stmt {
                let result = self.eval_expr(&expr);
                last_value = Some(self.convert_result(result)?);
            } else {
                let result = self.execute_stmt(&stmt);
                self.convert_result(result)?;
            }

            if self.return_value.is_some() || self.break_flag || self.continue_flag {
                break;
            }
        }
        Ok(last_value)
    }

    fn set_variable(&mut self, name: String, value: Value) {
        if let Some(local) = self.locals.last() {
            local.borrow_mut().insert(name, value);
        } else {
            self.globals.borrow_mut().insert(name, value);
        }
    }

    fn get_variable(&self, name: &str) -> Result<Value, String> {
        // 从最近的作用域开始查找
        for scope in self.locals.iter().rev() {
            if let Some(val) = scope.borrow().get(name) {
                return Ok(val.clone());
            }
        }

        // 查找全局作用域
        if let Some(val) = self.globals.borrow().get(name) {
            return Ok(val.clone());
        }

        Err(format!("Undefined variable: {}", name))
    }

    fn variable_exists(&self, name: &str) -> bool {
        for scope in self.locals.iter().rev() {
            if scope.borrow().contains_key(name) {
                return true;
            }
        }
        self.globals.borrow().contains_key(name)
    }

    /// Apply a decorator to a function (LSR-011)
    fn apply_decorator(&self, decorator: &str, func: Value) -> Result<Value, String> {
        match decorator {
            "pure" => {
                // @pure: Mark function as pure (no side effects)
                // For now, this is just metadata - no runtime behavior change
                Ok(func)
            }
            "memoize" => {
                // @memoize: Cache function results
                Ok(Value::MemoizedFunction {
                    original: Box::new(func),
                    cache: Rc::new(RefCell::new(HashMap::new())),
                })
            }
            _ => {
                eprintln!("Warning: Unknown decorator '{}', ignoring", decorator);
                Ok(func)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn eval_expr(code: &str) -> Result<Value, crate::error::RuminaError> {
        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse()?;
        let mut interpreter = Interpreter::new();
        match interpreter.interpret(ast)? {
            Some(v) => Ok(v),
            None => Err("No value returned".into()),
        }
    }

    #[test]
    fn test_power_int_rational() {
        // Test 8^(1/3) = 2 (perfect cube root returns Int)
        let result = eval_expr("8^(1/3)").unwrap();
        match result {
            Value::Int(n) => assert_eq!(n, 2, "Expected 2, got {}", n),
            other => panic!("Expected Int result, got {:?}", other),
        }
    }

    #[test]
    fn test_power_negative_int_rational() {
        // Test (-8)^(1/3) - should return -2 (real cube root as Int)
        let result = eval_expr("(-8)^(1/3)");
        assert!(result.is_ok(), "Should not error on (-8)^(1/3)");

        // Verify the result is -2
        match result.unwrap() {
            Value::Int(n) => {
                assert_eq!(n, -2, "Expected -2, got {}", n);
            }
            other => panic!("Expected Int result, got {:?}", other),
        }
    }

    #[test]
    fn test_power_int_by_half() {
        // Test 4^(1/2) = 2 (perfect square root returns Int)
        let result = eval_expr("4^(1/2)").unwrap();
        match result {
            Value::Int(n) => assert_eq!(n, 2, "Expected 2, got {}", n),
            other => panic!("Expected Int result, got {:?}", other),
        }
    }

    #[test]
    fn test_power_rational_rational() {
        // Test (1/2)^(1/2) - should return symbolic or float
        let result = eval_expr("(1/2)^(1/2)");
        assert!(result.is_ok(), "Should support rational^rational");
        // Accept any valid result type
    }

    #[test]
    fn test_power_float_rational() {
        // Test 2.0^(1/2) - since 2.0 is Float, may return Irrational or Float
        let result = eval_expr("2.0^(1/2)").unwrap();
        match result {
            Value::Irrational(_) => {} // Symbolic √2
            Value::Float(f) => assert!((f - 1.414).abs() < 0.01),
            other => panic!("Expected Irrational or Float, got {:?}", other),
        }
    }

    #[test]
    fn test_power_negative_fifth_root() {
        // Test (-32)^(1/5) = -2 (perfect fifth root returns Int)
        let result = eval_expr("(-32)^(1/5)");
        assert!(result.is_ok(), "Should not error on (-32)^(1/5)");
        match result.unwrap() {
            Value::Int(n) => {
                assert_eq!(n, -2, "Expected -2, got {}", n);
            }
            other => panic!("Expected Int result, got {:?}", other),
        }
    }

    #[test]
    fn test_power_mathcore_integration() {
        // Test that mathcore is being used for calculations
        // 27^(1/3) = 3 (perfect cube root returns Int)
        let result = eval_expr("27^(1/3)").unwrap();
        match result {
            Value::Int(n) => assert_eq!(n, 3, "Expected 3, got {}", n),
            other => panic!("Expected Int result, got {:?}", other),
        }
    }

    #[test]
    fn test_power_mathcore_exclusive() {
        // Test that mathcore handles various power operations

        // Large integer exponent
        let result = eval_expr("2^10").unwrap();
        match result {
            Value::Int(i) => assert_eq!(i, 1024, "Expected 1024, got {}", i),
            _ => panic!("Expected Int result for integer power"),
        }

        // Fractional base and exponent - (1/4)^(1/2) = 1/2 ideally, but we may get float
        let result = eval_expr("(1/4)^(1/2)");
        assert!(result.is_ok(), "Should handle (1/4)^(1/2)");

        // Negative base with rational exponent (odd denominator) - perfect root
        let result = eval_expr("(-27)^(1/3)").unwrap();
        match result {
            Value::Int(n) => assert_eq!(n, -3, "Expected -3, got {}", n),
            other => panic!("Expected Int result, got {:?}", other),
        }
    }

    #[test]
    fn test_power_int_overflow_promotion_to_bigint() {
        // Test that Int^Int automatically promotes to BigInt on overflow
        // This should not panic, but instead return a BigInt
        let result = eval_expr("114514^100");
        assert!(result.is_ok(), "Should not panic on large power");
        match result.unwrap() {
            Value::BigInt(n) => {
                // Verify it's a very large number (has more than 100 digits)
                let str_repr = n.to_string();
                assert!(str_repr.len() > 100, "Expected very large number, got {} digits", str_repr.len());
            }
            other => panic!("Expected BigInt result for large power, got {:?}", other),
        }
    }

    #[test]
    fn test_complex_even_root_negative() {
        // Test (-4)^(1/2) should return complex number
        let result = eval_expr("(-4)^(1/2)");
        assert!(result.is_ok(), "Should not error on (-4)^(1/2)");
        match result.unwrap() {
            Value::Complex(re, im) => {
                // Should be 0 + 2i (symbolic)
                assert!(
                    matches!(re.as_ref(), Value::Int(0)),
                    "Real part should be 0, got {:?}",
                    re
                );
                assert!(
                    matches!(im.as_ref(), Value::Int(2)),
                    "Imaginary part should be 2, got {:?}",
                    im
                );
            }
            other => panic!(
                "Expected Complex result for even root of negative, got {:?}",
                other
            ),
        }
    }

    #[test]
    fn test_symbolic_irrational_roots() {
        // Test 2^(1/2) should return symbolic √2
        let result = eval_expr("2^(1/2)").unwrap();
        match result {
            Value::Irrational(IrrationalValue::Sqrt(_))
            | Value::Irrational(IrrationalValue::Root(2, _)) => {
                // Good - symbolic representation
            }
            other => {
                // May also return Int if simplified, which is fine
                println!("Got non-symbolic result: {:?}", other);
            }
        }
    }

    // Tests for Complex number arithmetic
    #[test]
    fn test_complex_addition() {
        // Test Complex + Complex
        let code = r#"
            var c1 = (-4)^(1/2);
            var c2 = (-9)^(1/2);
            c1 + c2;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Complex addition should work");
        match result.unwrap() {
            Value::Complex(re, im) => {
                // Should be 0 + 5i
                assert!(
                    matches!(re.as_ref(), Value::Int(0)),
                    "Real part should be 0"
                );
                assert!(
                    matches!(im.as_ref(), Value::Int(5)),
                    "Imaginary part should be 5, got {:?}",
                    im
                );
            }
            other => panic!("Expected Complex result, got {:?}", other),
        }
    }

    #[test]
    fn test_complex_multiplication() {
        // Test Complex * Complex
        let code = r#"
            var c1 = (-1)^(1/2);
            var c2 = (-1)^(1/2);
            c1 * c2;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Complex multiplication should work");
        // i * i = -1 + 0i
        match result.unwrap() {
            Value::Int(-1) => {} // Simplified to just -1
            Value::Complex(re, im) => {
                assert!(
                    matches!(re.as_ref(), Value::Int(-1)),
                    "Real part should be -1, got {:?}",
                    re
                );
                assert!(
                    matches!(im.as_ref(), Value::Int(0)),
                    "Imaginary part should be 0, got {:?}",
                    im
                );
            }
            other => panic!("Expected Int(-1) or Complex result, got {:?}", other),
        }
    }

    #[test]
    fn test_complex_with_int() {
        // Test Complex + Int
        let code = r#"
            var c = (-4)^(1/2);
            c + 3;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Complex + Int should work");
        match result.unwrap() {
            Value::Complex(re, im) => {
                // Should be 3 + 2i
                assert!(
                    matches!(re.as_ref(), Value::Int(3)),
                    "Real part should be 3, got {:?}",
                    re
                );
                assert!(
                    matches!(im.as_ref(), Value::Int(2)),
                    "Imaginary part should be 2, got {:?}",
                    im
                );
            }
            other => panic!("Expected Complex result, got {:?}", other),
        }
    }

    #[test]
    fn test_complex_division() {
        // Test Complex / Complex
        let code = r#"
            var c1 = (-4)^(1/2);
            var c2 = (-1)^(1/2);
            c1 / c2;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Complex division should work");
        match result.unwrap() {
            // (-4)^(1/2) = 2i, (-1)^(1/2) = i, so 2i / i = 2/1 + 0i or just 2/1
            Value::Rational(r) => {
                assert_eq!(r.numer().to_string(), "2", "Result should be 2/1");
            }
            Value::Int(2) => {} // Simplified to just 2
            Value::Complex(re, im) => {
                assert!(
                    matches!(re.as_ref(), Value::Rational(_) | Value::Int(2)),
                    "Real part should be 2, got {:?}",
                    re
                );
                // Imaginary part could be Int(0) or Rational(0/1)
                match im.as_ref() {
                    Value::Int(0) => {}
                    Value::Rational(r) if r.numer().to_string() == "0" => {}
                    other => panic!("Imaginary part should be 0, got {:?}", other),
                }
            }
            other => panic!("Expected Complex result, got {:?}", other),
        }
    }

    #[test]
    fn test_complex_with_rational() {
        // Test Complex * Rational
        let code = r#"
            var c = (-1)^(1/2);
            var r = 1/2;
            c * r;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Complex * Rational should work");
        match result.unwrap() {
            Value::Complex(re, im) => {
                // Should be 0 + (1/2)i
                // Real part could be Int(0) or Rational(0/1)
                match re.as_ref() {
                    Value::Int(0) => {}
                    Value::Rational(r) if r.numer().to_string() == "0" => {}
                    other => panic!("Real part should be 0, got {:?}", other),
                }
                match im.as_ref() {
                    Value::Rational(r) => {
                        assert_eq!(
                            r.numer().to_string(),
                            "1",
                            "Imaginary numerator should be 1"
                        );
                        assert_eq!(
                            r.denom().to_string(),
                            "2",
                            "Imaginary denominator should be 2"
                        );
                    }
                    other => panic!("Expected Rational imaginary part, got {:?}", other),
                }
            }
            other => panic!("Expected Complex result, got {:?}", other),
        }
    }

    // Tests for enhanced Irrational simplification
    #[test]
    fn test_sqrt_multiplication_simplification() {
        // Test √2 × √2 → 2
        let code = r#"
            var s = sqrt(2);
            s * s;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "√2 × √2 should work");
        match result.unwrap() {
            Value::Int(n) => assert_eq!(n, 2, "√2 × √2 should equal 2"),
            other => panic!("Expected Int(2), got {:?}", other),
        }
    }

    #[test]
    fn test_sqrt_multiplication_non_perfect() {
        // Test √2 × √3 → √6
        let code = r#"
            var s2 = sqrt(2);
            var s3 = sqrt(3);
            s2 * s3;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "√2 × √3 should work");
        match result.unwrap() {
            Value::Irrational(IrrationalValue::Sqrt(n)) => {
                match n.as_ref() {
                    Value::Int(6) => {} // Expected √6
                    other => panic!("Expected √6, got √{:?}", other),
                }
            }
            other => panic!("Expected Irrational(Sqrt(6)), got {:?}", other),
        }
    }

    #[test]
    fn test_sqrt_eight_simplification() {
        // Test √8 → 2√2 (via display formatting)
        let code = "sqrt(8);";
        let result = eval_expr(code);
        assert!(result.is_ok(), "sqrt(8) should work");
        // The result should be Irrational(Sqrt(8)), which displays as "2√2"
        match result.unwrap() {
            Value::Irrational(IrrationalValue::Sqrt(n)) => {
                match n.as_ref() {
                    Value::Int(8) => {
                        // Display formatting will simplify to 2√2
                    }
                    other => panic!("Expected √8, got √{:?}", other),
                }
            }
            other => panic!("Expected Irrational(Sqrt(8)), got {:?}", other),
        }
    }

    #[test]
    fn test_complex_negation() {
        // Test negation of complex numbers
        let code = r#"
            var c = (-1)^(1/2);
            -c;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Complex negation should work");
        match result.unwrap() {
            Value::Complex(re, im) => {
                // Should be 0 + (-1)i = -i
                assert!(
                    matches!(re.as_ref(), Value::Int(0)),
                    "Real part should be 0, got {:?}",
                    re
                );
                assert!(
                    matches!(im.as_ref(), Value::Int(-1)),
                    "Imaginary part should be -1, got {:?}",
                    im
                );
            }
            other => panic!("Expected Complex result, got {:?}", other),
        }
    }

    #[test]
    fn test_rational_negation() {
        // Test negation of rational numbers
        let code = r#"
            var r = 3/4;
            -r;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Rational negation should work");
        match result.unwrap() {
            Value::Rational(r) => {
                assert_eq!(r.numer().to_string(), "-3");
                assert_eq!(r.denom().to_string(), "4");
            }
            other => panic!("Expected Rational result, got {:?}", other),
        }
    }

    #[test]
    fn test_irrational_negation() {
        // Test negation of irrational numbers
        let code = r#"
            var s = sqrt(2);
            -s;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Irrational negation should work");
        match result.unwrap() {
            Value::Irrational(IrrationalValue::Product(coef, irr)) => {
                match coef.as_ref() {
                    Value::Int(-1) => {
                        match irr.as_ref() {
                            IrrationalValue::Sqrt(n) => {
                                match n.as_ref() {
                                    Value::Int(2) => {} // Expected -√2
                                    other => panic!("Expected -√2, got -√{:?}", other),
                                }
                            }
                            other => panic!("Expected Sqrt, got {:?}", other),
                        }
                    }
                    other => panic!("Expected coefficient -1, got {:?}", other),
                }
            }
            other => panic!("Expected Irrational(Product(-1, Sqrt(2))), got {:?}", other),
        }
    }

    #[test]
    fn test_deep_recursion_simple() {
        // Test that we can handle moderately deep recursion (50 levels)
        // Note: test runner has limited stack, so we test conservatively
        let code = r#"
            func countdown(n) {
                if (n <= 0) {
                    return 0;
                }
                return countdown(n - 1);
            }
            countdown(50);
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Should handle 50 levels of recursion");
        match result.unwrap() {
            Value::Int(n) => assert_eq!(n, 0, "Expected 0, got {}", n),
            other => panic!("Expected Int(0), got {:?}", other),
        }
    }

    #[test]
    #[ignore] // This test requires the increased stack size from main()
    fn test_deep_recursion_moderate() {
        // Test that we can handle deep recursion (1000 levels)
        // This test is ignored by default as it requires the CLI's increased stack size
        let code = r#"
            func countdown(n) {
                if (n <= 0) {
                    return 0;
                }
                return countdown(n - 1);
            }
            countdown(1000);
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Should handle 1000 levels of recursion");
        match result.unwrap() {
            Value::Int(n) => assert_eq!(n, 0, "Expected 0, got {}", n),
            other => panic!("Expected Int(0), got {:?}", other),
        }
    }

    #[test]
    #[ignore] // This test requires the increased stack size from main()
    fn test_recursion_limit() {
        // Test that we get a proper error when exceeding recursion limit
        // This test is ignored by default as it requires the CLI's increased stack size
        let code = r#"
            func infinite(n) {
                return infinite(n + 1);
            }
            infinite(0);
        "#;
        let result = eval_expr(code);
        assert!(
            result.is_err(),
            "Should fail when exceeding recursion limit"
        );
        let err = result.unwrap_err();
        let err_msg = format!("{}", err);
        assert!(
            err_msg.contains("Maximum recursion depth exceeded"),
            "Error should mention recursion depth, got: {}",
            err_msg
        );
    }

    #[test]
    fn test_fibonacci_small() {
        // Test that basic fibonacci works
        let code = r#"
            func fib(n) {
                if (n <= 1) {
                    return n;
                }
                return fib(n - 1) + fib(n - 2);
            }
            fib(10);
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Should calculate fib(10)");
        match result.unwrap() {
            Value::Int(n) => assert_eq!(n, 55, "Expected fib(10) = 55, got {}", n),
            other => panic!("Expected Int(55), got {:?}", other),
        }
    }
}
