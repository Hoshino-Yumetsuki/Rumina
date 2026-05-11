/// Tests for decimal precision - ensuring 0.1 + 0.2 == 0.3
use rumina::numeric::{BigInt, BigRational, rational_new};
use rumina::{Value, run_rumina};

fn expected_rational(numerator: i64, denominator: i64) -> BigRational {
    rational_new(BigInt::from(numerator), BigInt::from(denominator))
}

#[test]
fn test_decimal_addition_precision() {
    // The classic precision test: 0.1 + 0.2 should equal 0.3
    let result = run_rumina("0.1 + 0.2;");
    assert!(result.is_ok(), "0.1 + 0.2 should not error");

    if let Ok(Some(value)) = result {
        // Should be a rational: 3/10
        match value {
            Value::Rational(r) => {
                let expected = expected_rational(3, 10);
                assert_eq!(r, expected, "0.1 + 0.2 should equal 3/10");
            }
            _ => panic!("Expected Rational, got {:?}", value),
        }
    }
}

#[test]
fn test_decimal_equality() {
    // Test that 0.1 + 0.2 == 0.3 returns true
    let result = run_rumina("0.1 + 0.2 == 0.3;");
    assert!(result.is_ok());

    if let Ok(Some(value)) = result {
        match value {
            Value::Bool(b) => assert!(b, "0.1 + 0.2 should equal 0.3"),
            _ => panic!("Expected Bool, got {:?}", value),
        }
    }
}

#[test]
fn test_simple_decimal_parsing() {
    // Test that 0.1 parses to 1/10
    let result = run_rumina("0.1;");
    assert!(result.is_ok());

    if let Ok(Some(value)) = result {
        match value {
            Value::Rational(r) => {
                let expected = expected_rational(1, 10);
                assert_eq!(r, expected, "0.1 should equal 1/10");
            }
            _ => panic!("Expected Rational, got {:?}", value),
        }
    }
}

#[test]
fn test_decimal_quarter() {
    // Test that 0.25 parses to 1/4
    let result = run_rumina("0.25;");
    assert!(result.is_ok());

    if let Ok(Some(value)) = result {
        match value {
            Value::Rational(r) => {
                let expected = expected_rational(1, 4);
                assert_eq!(r, expected, "0.25 should equal 1/4");
            }
            _ => panic!("Expected Rational, got {:?}", value),
        }
    }
}

#[test]
fn test_decimal_multiplication() {
    // Test 0.5 * 0.5 = 0.25 (1/4)
    let result = run_rumina("0.5 * 0.5;");
    assert!(result.is_ok());

    if let Ok(Some(value)) = result {
        match value {
            Value::Rational(r) => {
                let expected = expected_rational(1, 4);
                assert_eq!(r, expected, "0.5 * 0.5 should equal 1/4");
            }
            _ => panic!("Expected Rational, got {:?}", value),
        }
    }
}

#[test]
fn test_decimal_subtraction() {
    // Test 0.3 - 0.1 = 0.2 (1/5)
    let result = run_rumina("0.3 - 0.1;");
    assert!(result.is_ok());

    if let Ok(Some(value)) = result {
        match value {
            Value::Rational(r) => {
                let expected = expected_rational(1, 5);
                assert_eq!(r, expected, "0.3 - 0.1 should equal 1/5");
            }
            _ => panic!("Expected Rational, got {:?}", value),
        }
    }
}

#[test]
fn test_decimal_display() {
    // Test that decimals display as rationals
    let result = run_rumina("0.1;");
    assert!(result.is_ok());

    if let Ok(Some(value)) = result {
        let display = value.to_string();
        assert_eq!(display, "1/10", "0.1 should display as 1/10");
    }
}

#[test]
fn test_multiple_decimal_places() {
    // Test 0.125 = 1/8
    let result = run_rumina("0.125;");
    assert!(result.is_ok());

    if let Ok(Some(value)) = result {
        match value {
            Value::Rational(r) => {
                let expected = expected_rational(1, 8);
                assert_eq!(r, expected, "0.125 should equal 1/8");
            }
            _ => panic!("Expected Rational, got {:?}", value),
        }
    }
}

#[test]
fn test_decimal_with_integer_part() {
    // Test 1.5 = 3/2
    let result = run_rumina("1.5;");
    assert!(result.is_ok());

    if let Ok(Some(value)) = result {
        match value {
            Value::Rational(r) => {
                let expected = expected_rational(3, 2);
                assert_eq!(r, expected, "1.5 should equal 3/2");
            }
            _ => panic!("Expected Rational, got {:?}", value),
        }
    }
}

#[test]
fn test_complex_decimal_expression() {
    // Test (0.1 + 0.2) * 2 = 0.6 (3/5)
    let result = run_rumina("(0.1 + 0.2) * 2;");
    assert!(result.is_ok());

    if let Ok(Some(value)) = result {
        match value {
            Value::Rational(r) => {
                let expected = expected_rational(3, 5);
                assert_eq!(r, expected, "(0.1 + 0.2) * 2 should equal 3/5");
            }
            _ => panic!("Expected Rational, got {:?}", value),
        }
    }
}

#[test]
fn test_negative_decimal() {
    // Test negative decimals: -0.1 should equal -1/10
    let result = run_rumina("-0.1;");
    assert!(result.is_ok());

    if let Ok(Some(value)) = result {
        match value {
            Value::Rational(r) => {
                let expected = expected_rational(-1, 10);
                assert_eq!(r, expected, "-0.1 should equal -1/10");
            }
            _ => panic!("Expected Rational, got {:?}", value),
        }
    }
}

#[test]
fn test_large_decimal() {
    // Test a decimal with many places: 0.123456789012345678 (18 places, max allowed)
    let result = run_rumina("0.123456789012345678;");
    assert!(result.is_ok());

    if let Ok(Some(value)) = result {
        match value {
            Value::Rational(_) => {
                // Just verify it's a rational, actual value doesn't matter
            }
            _ => panic!("Expected Rational, got {:?}", value),
        }
    }
}

#[test]
fn test_zero_decimal() {
    // Test 0.0 should equal 0/1
    let result = run_rumina("0.0;");
    assert!(result.is_ok());

    if let Ok(Some(value)) = result {
        match value {
            Value::Rational(r) => {
                let expected = expected_rational(0, 1);
                assert_eq!(r, expected, "0.0 should equal 0/1");
            }
            _ => panic!("Expected Rational, got {:?}", value),
        }
    }
}
