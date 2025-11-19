/// Tests for math built-in functions
use rumina::run_rumina;

#[test]
fn test_sqrt_with_irrational() {
    // Test sqrt with irrational value (7 + 2*sqrt(10))
    // Should preserve symbolic form
    let result = run_rumina("sqrt(7 + 2*sqrt(10));");
    assert!(result.is_ok(), "sqrt should handle irrational values");
    
    if let Ok(Some(value)) = result {
        let value_str = value.to_string();
        
        // Result should be in symbolic form: √(7+2√10)
        assert!(
            value_str.contains("√"),
            "Result should be in symbolic form with √, got {}",
            value_str
        );
    }
}

#[test]
fn test_sqrt_basic() {
    // Test basic sqrt functionality
    let result = run_rumina("sqrt(4);");
    assert!(result.is_ok());
    
    if let Ok(Some(value)) = result {
        assert_eq!(value.to_string(), "2");
    }
}

#[test]
fn test_sqrt_non_perfect_square() {
    // Test sqrt of non-perfect square (should return irrational in symbolic form)
    let result = run_rumina("sqrt(2);");
    assert!(result.is_ok());
    
    if let Ok(Some(value)) = result {
        let value_str = value.to_string();
        // Should be in symbolic form √2
        assert_eq!(value_str, "√2", "sqrt(2) should be √2, got {}", value_str);
    }
}

#[test]
fn test_nested_sqrt() {
    // Test nested sqrt: sqrt(sqrt(16))
    let result = run_rumina("sqrt(sqrt(16));");
    assert!(result.is_ok());
    
    if let Ok(Some(value)) = result {
        assert_eq!(value.to_string(), "2");
    }
}

#[test]
fn test_sqrt_with_multiplication() {
    // Test sqrt(4 * 9) = 6
    let result = run_rumina("sqrt(4 * 9);");
    assert!(result.is_ok());
    
    if let Ok(Some(value)) = result {
        assert_eq!(value.to_string(), "6");
    }
}

#[test]
fn test_sqrt_negative() {
    // Test sqrt of negative number (should return complex)
    let result = run_rumina("sqrt(-4);");
    assert!(result.is_ok());
    
    if let Ok(Some(value)) = result {
        let value_str = value.to_string();
        // Should contain 'i' for imaginary unit
        assert!(value_str.contains("i") || value_str.contains("2i"));
    }
}

#[test]
fn test_symbolic_multiplication() {
    // Test that 2*sqrt(2) preserves symbolic form
    let result = run_rumina("2*sqrt(2);");
    assert!(result.is_ok());
    
    if let Ok(Some(value)) = result {
        let value_str = value.to_string();
        assert_eq!(value_str, "2√2", "2*sqrt(2) should be 2√2, got {}", value_str);
    }
}

#[test]
fn test_symbolic_addition() {
    // Test that sqrt(2) + sqrt(3) preserves symbolic form
    let result = run_rumina("sqrt(2) + sqrt(3);");
    assert!(result.is_ok());
    
    if let Ok(Some(value)) = result {
        let value_str = value.to_string();
        assert!(value_str.contains("√2") && value_str.contains("√3"), 
                "sqrt(2) + sqrt(3) should contain √2 and √3, got {}", value_str);
    }
}

#[test]
fn test_symbolic_sqrt_product() {
    // Test that sqrt(2) * sqrt(3) = sqrt(6)
    let result = run_rumina("sqrt(2) * sqrt(3);");
    assert!(result.is_ok());
    
    if let Ok(Some(value)) = result {
        let value_str = value.to_string();
        assert_eq!(value_str, "√6", "sqrt(2) * sqrt(3) should be √6, got {}", value_str);
    }
}

