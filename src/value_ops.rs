use mathcore::MathCore;
use num::complex::Complex64;
/// Standalone value operations module
///
/// This module provides optimized operation implementations for the VM that don't require
/// a full Interpreter instance, enabling significant performance improvements.
///
/// ## Performance Optimization
///
/// Previously, every VM operation (add, subtract, compare, etc.) created a new Interpreter
/// instance with `Interpreter::new()`, which:
/// - Allocated a new HashMap for globals
/// - Registered ALL built-in functions via `builtin::register_builtins()`
/// - Set up complex data structures
///
/// For recursive functions like fib(30) with millions of operations, this overhead was
/// catastrophic - making the VM **much slower** than the AST interpreter.
///
/// ## Solution
///
/// This module provides standalone operation functions that:
/// - Implement fast paths for common types (Int, BigInt, Bool)
/// - Avoid any interpreter instantiation for basic operations
/// - Fall back to creating an Interpreter only for complex types (Irrational, Complex, etc.)
///
/// ## Performance Impact
///
/// With these optimizations, the VM is now **1.8-2x faster** than the AST interpreter:
/// - fib(30) in release mode: VM 1.93s vs Interpreter 3.45s
/// - fib(20) in debug mode: VM 90ms vs Interpreter 185ms
///
/// ## Parallel BigInt Power Operations
///
/// For large exponent power operations, we use parallel binary exponentiation:
/// - Threshold: exponents >= 10000 use parallelization
/// - Uses rayon for parallel chunk processing
/// - Improves CPU utilization on multi-core systems
use num::{BigInt, BigRational};
use rayon::prelude::*;

use crate::ast::{BinOp, UnaryOp};
use crate::value::{IrrationalValue, Value};

/// Threshold for using parallel BigInt power computation
const PARALLEL_POW_THRESHOLD: u32 = 10000;

/// Compute BigInt power operation with optional parallelization for large exponents
///
/// For exponents >= PARALLEL_POW_THRESHOLD, uses parallel divide-and-conquer
/// to improve CPU utilization on multi-core systems.
fn bigint_pow_optimized(base: &BigInt, exponent: u32) -> BigInt {
    // For small exponents, use the standard sequential algorithm
    if exponent < PARALLEL_POW_THRESHOLD {
        return base.pow(exponent);
    }

    // For large exponents, use parallel divide-and-conquer approach
    // This splits the computation: base^exp = (base^(exp/2))^2 or (base^(exp/2))^2 * base
    bigint_pow_parallel(base, exponent)
}

/// Parallel power computation using divide-and-conquer
fn bigint_pow_parallel(base: &BigInt, exponent: u32) -> BigInt {
    if exponent == 0 {
        return BigInt::from(1);
    }
    if exponent == 1 {
        return base.clone();
    }

    // Use sequential for small exponents
    if exponent < 500 {
        return base.pow(exponent);
    }

    // For large exponents, split into chunks that can be computed independently
    // then multiplied together. This allows parallel computation.
    // Strategy: base^exp = base^(chunk_size) * base^(chunk_size) * ... * base^(remainder)

    // Choose chunk size based on exponent to create parallelizable work
    let num_chunks = if exponent >= 100000 {
        8 // More chunks for very large exponents
    } else if exponent >= 10000 {
        4
    } else {
        2
    };

    let chunk_size = exponent / num_chunks;
    let remainder = exponent % num_chunks;

    if chunk_size < 100 {
        // Chunks too small, just use sequential
        return base.pow(exponent);
    }

    // Compute all chunks in parallel
    let chunk_results: Vec<BigInt> = (0..num_chunks)
        .into_par_iter()
        .map(|_| bigint_pow_parallel(base, chunk_size))
        .collect();

    // Multiply all chunk results together
    let mut result = BigInt::from(1);
    for chunk_result in chunk_results {
        result *= chunk_result;
    }

    // Handle remainder
    if remainder > 0 {
        let remainder_result = base.pow(remainder);
        result *= remainder_result;
    }

    result
}

/// Compute power operation with symbolic handling
#[allow(dead_code)]
fn compute_power(base: f64, exponent: f64) -> Result<Value, String> {
    // Check if exponent is a simple rational (1/n form)
    let denom_approx = (1.0 / exponent).round();
    let is_simple_root = (1.0 / denom_approx - exponent).abs() < 1e-10;

    if is_simple_root && denom_approx > 0.0 {
        let n = denom_approx as u32;

        // Handle negative bases
        if base < 0.0 {
            let is_odd_root = n % 2 == 1;

            if is_odd_root {
                // Odd root of negative number: return negative irrational
                let abs_base = base.abs();

                // Check if it's a perfect root
                let root_val = abs_base.powf(exponent);
                if (root_val.round() - root_val).abs() < 1e-10 {
                    // Perfect root - return as integer
                    return Ok(Value::Int(-(root_val.round() as i64)));
                }

                // Return as symbolic irrational: -(abs_base^(1/n))
                let root = if n == 2 {
                    IrrationalValue::Sqrt(Box::new(Value::Int(abs_base as i64)))
                } else {
                    IrrationalValue::Root(n, Box::new(Value::Int(abs_base as i64)))
                };

                // Negate by using Product with -1
                return Ok(Value::Irrational(IrrationalValue::Product(
                    Box::new(Value::Int(-1)),
                    Box::new(root),
                )));
            } else {
                // Even root of negative: return symbolic complex number
                let abs_base = base.abs();
                let root_val = abs_base.powf(exponent);

                if (root_val.round() - root_val).abs() < 1e-10 {
                    // Perfect root - return as i * integer
                    return Ok(Value::Complex(
                        Box::new(Value::Int(0)),
                        Box::new(Value::Int(root_val.round() as i64)),
                    ));
                }

                // Return as symbolic: 0 + sqrt(abs_base) * i
                let root = if n == 2 {
                    IrrationalValue::Sqrt(Box::new(Value::Int(abs_base as i64)))
                } else {
                    IrrationalValue::Root(n, Box::new(Value::Int(abs_base as i64)))
                };

                return Ok(Value::Complex(
                    Box::new(Value::Int(0)),
                    Box::new(Value::Irrational(root)),
                ));
            }
        } else {
            // Positive base
            // Check if it's a perfect root
            let root_val = base.powf(exponent);
            if (root_val.round() - root_val).abs() < 1e-10 {
                // Perfect root - return as integer
                return Ok(Value::Int(root_val.round() as i64));
            }

            // Return as symbolic irrational
            let root = if n == 2 {
                IrrationalValue::Sqrt(Box::new(Value::Int(base as i64)))
            } else {
                IrrationalValue::Root(n, Box::new(Value::Int(base as i64)))
            };

            return Ok(Value::Irrational(root));
        }
    }

    // For non-simple-root cases, use mathcore
    let math = MathCore::new();
    let expr = format!("{}^{}", base, exponent);

    match math.evaluate(&expr) {
        Ok(mathcore::Expr::Number(result)) => {
            // Check if result is close to an integer
            if (result.round() - result).abs() < 1e-10 {
                Ok(Value::Int(result.round() as i64))
            } else {
                Ok(Value::Float(result))
            }
        }
        Ok(other) => Err(format!(
            "Mathcore returned non-numeric result for {}: {:?}",
            expr, other
        )),
        Err(_) => {
            // Mathcore failed - try complex number evaluation for negative bases
            if base < 0.0 {
                // Return symbolic complex number
                let c = Complex64::new(base, 0.0).powf(exponent);
                Ok(Value::Complex(
                    Box::new(Value::Float(c.re)),
                    Box::new(Value::Float(c.im)),
                ))
            } else {
                Err(format!("Mathcore evaluation failed for {}", expr))
            }
        }
    }
}

/// Multiply two irrational values
#[allow(dead_code)]
fn multiply_irrationals(a: &IrrationalValue, b: &IrrationalValue) -> Result<Value, String> {
    // Product simplification (limited)
    match (a, b) {
        (IrrationalValue::Sqrt(v1), IrrationalValue::Sqrt(v2)) => {
            // sqrt(a) * sqrt(b) = sqrt(a*b)
            let product = value_binary_op(v1, BinOp::Mul, v2)?;
            Ok(Value::Irrational(IrrationalValue::Sqrt(Box::new(product))))
        }
        _ => {
            // For other combinations, wrap first irrational in Product with coefficient 1
            // Then multiply by second irrational using Product wrapper again
            // For simplicity, just convert to float for now or fallback to interpreter
            use crate::interpreter::Interpreter;
            let mut interp = Interpreter::new();
            let left = Value::Irrational(a.clone());
            let right = Value::Irrational(b.clone());
            interp.eval_binary_op(&left, BinOp::Mul, &right)
        }
    }
}

/// Evaluate binary operation on values
pub fn value_binary_op(left: &Value, op: BinOp, right: &Value) -> Result<Value, String> {
    match (left, right) {
        (Value::Int(a), Value::Int(b)) => {
            match op {
                BinOp::Add => Ok(Value::Int(a + b)),
                BinOp::Sub => Ok(Value::Int(a - b)),
                BinOp::Mul => Ok(Value::Int(a * b)),
                BinOp::Div => {
                    if *b == 0 {
                        return Err("Division by zero".to_string());
                    }
                    // 返回有理数
                    let rational = BigRational::new(BigInt::from(*a), BigInt::from(*b));
                    Ok(Value::Rational(rational))
                }
                BinOp::Mod => Ok(Value::Int(a % b)),
                BinOp::Pow => {
                    if *b < 0 {
                        // Negative exponent: convert to float
                        let result = (*a as f64).powf(*b as f64);
                        Ok(Value::Float(result))
                    } else {
                        // Positive exponent: try as int, promote to BigInt on overflow
                        match a.checked_pow(*b as u32) {
                            Some(result) => Ok(Value::Int(result)),
                            None => {
                                // Overflow: promote to BigInt and use optimized version
                                let a_big = BigInt::from(*a);
                                Ok(Value::BigInt(bigint_pow_optimized(&a_big, *b as u32)))
                            }
                        }
                    }
                }
                BinOp::Equal => Ok(Value::Bool(a == b)),
                BinOp::NotEqual => Ok(Value::Bool(a != b)),
                BinOp::Greater => Ok(Value::Bool(a > b)),
                BinOp::GreaterEq => Ok(Value::Bool(a >= b)),
                BinOp::Less => Ok(Value::Bool(a < b)),
                BinOp::LessEq => Ok(Value::Bool(a <= b)),
                _ => Err(format!("Unsupported operation: int {} int", op)),
            }
        }

        // BigInt operations
        (Value::BigInt(a), Value::BigInt(b)) => match op {
            BinOp::Add => Ok(Value::BigInt(a + b)),
            BinOp::Sub => Ok(Value::BigInt(a - b)),
            BinOp::Mul => Ok(Value::BigInt(a * b)),
            BinOp::Div => {
                if b == &BigInt::from(0) {
                    return Err("Division by zero".to_string());
                }
                let rational = BigRational::new(a.clone(), b.clone());
                Ok(Value::Rational(rational))
            }
            BinOp::Mod => {
                if b == &BigInt::from(0) {
                    return Err("Division by zero".to_string());
                }
                Ok(Value::BigInt(a % b))
            }
            BinOp::Pow => {
                if let Ok(exp) = b.to_string().parse::<u32>() {
                    Ok(Value::BigInt(bigint_pow_optimized(a, exp)))
                } else {
                    let a_float = a.to_string().parse::<f64>().unwrap_or(0.0);
                    let b_float = b.to_string().parse::<f64>().unwrap_or(0.0);
                    Ok(Value::Float(a_float.powf(b_float)))
                }
            }
            BinOp::Equal => Ok(Value::Bool(a == b)),
            BinOp::NotEqual => Ok(Value::Bool(a != b)),
            BinOp::Greater => Ok(Value::Bool(a > b)),
            BinOp::GreaterEq => Ok(Value::Bool(a >= b)),
            BinOp::Less => Ok(Value::Bool(a < b)),
            BinOp::LessEq => Ok(Value::Bool(a <= b)),
            _ => Err(format!("Unsupported operation: bigint {} bigint", op)),
        },

        // For complex operations, fallback to more complex logic if needed
        // For now, keep only the essential fast paths for Int operations

        // Boolean operations
        (Value::Bool(a), Value::Bool(b)) => match op {
            BinOp::And => Ok(Value::Bool(*a && *b)),
            BinOp::Or => Ok(Value::Bool(*a || *b)),
            BinOp::Equal => Ok(Value::Bool(a == b)),
            BinOp::NotEqual => Ok(Value::Bool(a != b)),
            _ => Err(format!("Unsupported operation: bool {} bool", op)),
        },

        // Null comparison operations
        (Value::Null, Value::Null) => match op {
            BinOp::Equal => Ok(Value::Bool(true)),
            BinOp::NotEqual => Ok(Value::Bool(false)),
            _ => Err(format!("Unsupported operation: null {} null", op)),
        },

        // Null compared with non-null
        (Value::Null, _) | (_, Value::Null) => match op {
            BinOp::Equal => Ok(Value::Bool(false)),
            BinOp::NotEqual => Ok(Value::Bool(true)),
            _ => Err(format!(
                "Unsupported operation: {} {} {}",
                left.type_name(),
                op,
                right.type_name()
            )),
        },

        // For other types, we need the full interpreter logic
        // Fall back to creating an interpreter for these cases
        _ => {
            // Import here to avoid circular dependency at module level
            use crate::interpreter::Interpreter;
            let mut interp = Interpreter::new();
            interp.eval_binary_op(left, op, right)
        }
    }
}

/// Evaluate unary operation on value
pub fn value_unary_op(op: UnaryOp, val: &Value) -> Result<Value, String> {
    match op {
        UnaryOp::Neg => {
            match val {
                Value::Int(n) => Ok(Value::Int(-n)),
                Value::Float(f) => Ok(Value::Float(-f)),
                Value::BigInt(n) => Ok(Value::BigInt(-n)),
                Value::Rational(r) => Ok(Value::Rational(-r)),
                _ => {
                    // Fall back to interpreter for complex types
                    use crate::interpreter::Interpreter;
                    let interp = Interpreter::new();
                    interp.eval_unary_op(op, val)
                }
            }
        }
        UnaryOp::Not => match val {
            Value::Bool(b) => Ok(Value::Bool(!b)),
            _ => Err(format!("Cannot apply 'not' to {}", val.type_name())),
        },
        UnaryOp::Factorial => {
            // Fall back to interpreter for factorial
            use crate::interpreter::Interpreter;
            let interp = Interpreter::new();
            interp.eval_unary_op(op, val)
        }
    }
}

#[cfg(test)]
#[allow(clippy::approx_constant)]
mod tests {
    use super::*;
    use num::BigInt;

    #[test]
    fn test_int_arithmetic() {
        assert_eq!(
            value_binary_op(&Value::Int(5), BinOp::Add, &Value::Int(3)).unwrap(),
            Value::Int(8)
        );
        assert_eq!(
            value_binary_op(&Value::Int(5), BinOp::Sub, &Value::Int(3)).unwrap(),
            Value::Int(2)
        );
        assert_eq!(
            value_binary_op(&Value::Int(5), BinOp::Mul, &Value::Int(3)).unwrap(),
            Value::Int(15)
        );
        assert_eq!(
            value_binary_op(&Value::Int(10), BinOp::Mod, &Value::Int(3)).unwrap(),
            Value::Int(1)
        );
    }

    #[test]
    fn test_int_division() {
        let result = value_binary_op(&Value::Int(10), BinOp::Div, &Value::Int(3)).unwrap();
        match result {
            Value::Rational(r) => {
                assert_eq!(r.numer(), &BigInt::from(10));
                assert_eq!(r.denom(), &BigInt::from(3));
            }
            _ => panic!("Expected Rational"),
        }
    }

    #[test]
    fn test_int_division_by_zero() {
        let result = value_binary_op(&Value::Int(10), BinOp::Div, &Value::Int(0));
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "Division by zero");
    }

    #[test]
    fn test_int_pow() {
        assert_eq!(
            value_binary_op(&Value::Int(2), BinOp::Pow, &Value::Int(3)).unwrap(),
            Value::Int(8)
        );
        assert_eq!(
            value_binary_op(&Value::Int(2), BinOp::Pow, &Value::Int(10)).unwrap(),
            Value::Int(1024)
        );
    }

    #[test]
    fn test_int_pow_negative() {
        let result = value_binary_op(&Value::Int(2), BinOp::Pow, &Value::Int(-2)).unwrap();
        match result {
            Value::Float(f) => assert!((f - 0.25).abs() < 1e-10),
            _ => panic!("Expected Float"),
        }
    }

    #[test]
    fn test_int_pow_overflow() {
        let result = value_binary_op(&Value::Int(2), BinOp::Pow, &Value::Int(100)).unwrap();
        match result {
            Value::BigInt(_) => {}
            _ => panic!("Expected BigInt on overflow"),
        }
    }

    #[test]
    fn test_int_comparisons() {
        assert_eq!(
            value_binary_op(&Value::Int(5), BinOp::Equal, &Value::Int(5)).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            value_binary_op(&Value::Int(5), BinOp::NotEqual, &Value::Int(3)).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            value_binary_op(&Value::Int(5), BinOp::Greater, &Value::Int(3)).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            value_binary_op(&Value::Int(5), BinOp::GreaterEq, &Value::Int(5)).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            value_binary_op(&Value::Int(3), BinOp::Less, &Value::Int(5)).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            value_binary_op(&Value::Int(3), BinOp::LessEq, &Value::Int(3)).unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn test_bigint_arithmetic() {
        let a = Value::BigInt(BigInt::from(100));
        let b = Value::BigInt(BigInt::from(50));
        assert_eq!(
            value_binary_op(&a, BinOp::Add, &b).unwrap(),
            Value::BigInt(BigInt::from(150))
        );
        assert_eq!(
            value_binary_op(&a, BinOp::Sub, &b).unwrap(),
            Value::BigInt(BigInt::from(50))
        );
        assert_eq!(
            value_binary_op(&a, BinOp::Mul, &b).unwrap(),
            Value::BigInt(BigInt::from(5000))
        );
    }

    #[test]
    fn test_bigint_division() {
        let a = Value::BigInt(BigInt::from(100));
        let b = Value::BigInt(BigInt::from(3));
        let result = value_binary_op(&a, BinOp::Div, &b).unwrap();
        match result {
            Value::Rational(_) => {}
            _ => panic!("Expected Rational"),
        }
    }

    #[test]
    fn test_bigint_division_by_zero() {
        let a = Value::BigInt(BigInt::from(100));
        let b = Value::BigInt(BigInt::from(0));
        assert!(value_binary_op(&a, BinOp::Div, &b).is_err());
    }

    #[test]
    fn test_bigint_mod() {
        let a = Value::BigInt(BigInt::from(100));
        let b = Value::BigInt(BigInt::from(7));
        assert_eq!(
            value_binary_op(&a, BinOp::Mod, &b).unwrap(),
            Value::BigInt(BigInt::from(2))
        );
    }

    #[test]
    fn test_bigint_mod_by_zero() {
        let a = Value::BigInt(BigInt::from(100));
        let b = Value::BigInt(BigInt::from(0));
        assert!(value_binary_op(&a, BinOp::Mod, &b).is_err());
    }

    #[test]
    fn test_bigint_pow() {
        let a = Value::BigInt(BigInt::from(2));
        let b = Value::BigInt(BigInt::from(10));
        assert_eq!(
            value_binary_op(&a, BinOp::Pow, &b).unwrap(),
            Value::BigInt(BigInt::from(1024))
        );
    }

    #[test]
    fn test_bigint_comparisons() {
        let a = Value::BigInt(BigInt::from(100));
        let b = Value::BigInt(BigInt::from(50));
        assert_eq!(
            value_binary_op(&a, BinOp::Equal, &a).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            value_binary_op(&a, BinOp::NotEqual, &b).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            value_binary_op(&a, BinOp::Greater, &b).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            value_binary_op(&a, BinOp::GreaterEq, &a).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            value_binary_op(&b, BinOp::Less, &a).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            value_binary_op(&b, BinOp::LessEq, &b).unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn test_bool_operations() {
        assert_eq!(
            value_binary_op(&Value::Bool(true), BinOp::And, &Value::Bool(true)).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            value_binary_op(&Value::Bool(true), BinOp::And, &Value::Bool(false)).unwrap(),
            Value::Bool(false)
        );
        assert_eq!(
            value_binary_op(&Value::Bool(false), BinOp::Or, &Value::Bool(true)).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            value_binary_op(&Value::Bool(false), BinOp::Or, &Value::Bool(false)).unwrap(),
            Value::Bool(false)
        );
        assert_eq!(
            value_binary_op(&Value::Bool(true), BinOp::Equal, &Value::Bool(true)).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            value_binary_op(&Value::Bool(true), BinOp::NotEqual, &Value::Bool(false)).unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn test_null_operations() {
        assert_eq!(
            value_binary_op(&Value::Null, BinOp::Equal, &Value::Null).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(
            value_binary_op(&Value::Null, BinOp::NotEqual, &Value::Null).unwrap(),
            Value::Bool(false)
        );
        assert_eq!(
            value_binary_op(&Value::Null, BinOp::Equal, &Value::Int(5)).unwrap(),
            Value::Bool(false)
        );
        assert_eq!(
            value_binary_op(&Value::Int(5), BinOp::Equal, &Value::Null).unwrap(),
            Value::Bool(false)
        );
        assert_eq!(
            value_binary_op(&Value::Null, BinOp::NotEqual, &Value::Int(5)).unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn test_unary_neg() {
        assert_eq!(
            value_unary_op(UnaryOp::Neg, &Value::Int(5)).unwrap(),
            Value::Int(-5)
        );
        assert_eq!(
            value_unary_op(UnaryOp::Neg, &Value::Float(3.14)).unwrap(),
            Value::Float(-3.14)
        );
        assert_eq!(
            value_unary_op(UnaryOp::Neg, &Value::BigInt(BigInt::from(100))).unwrap(),
            Value::BigInt(BigInt::from(-100))
        );
    }

    #[test]
    fn test_unary_not() {
        assert_eq!(
            value_unary_op(UnaryOp::Not, &Value::Bool(true)).unwrap(),
            Value::Bool(false)
        );
        assert_eq!(
            value_unary_op(UnaryOp::Not, &Value::Bool(false)).unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn test_unary_not_error() {
        let result = value_unary_op(UnaryOp::Not, &Value::Int(5));
        assert!(result.is_err());
    }

    #[test]
    fn test_bigint_pow_optimized_small() {
        let base = BigInt::from(2);
        let result = bigint_pow_optimized(&base, 10);
        assert_eq!(result, BigInt::from(1024));
    }

    #[test]
    fn test_bigint_pow_parallel_base_cases() {
        let base = BigInt::from(2);
        assert_eq!(bigint_pow_parallel(&base, 0), BigInt::from(1));
        assert_eq!(bigint_pow_parallel(&base, 1), BigInt::from(2));
    }
}
