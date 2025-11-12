/// LSR 007 Verification Tests
/// 
/// This test file verifies whether the current implementation meets LSR 007 requirements.
/// LSR 007 specifies that BigInt should be the default integer type in Lamina.

use rumina::{run_rumina, Lexer, Parser};

/// Test 1: Check if integer literals use BigInt by default
#[test]
fn test_integer_literal_type() {
    // Parse a simple integer literal
    let mut lexer = Lexer::new("42".to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().expect("Parse failed");
    
    // Check the AST structure
    println!("AST for '42': {:?}", ast);
    
    // The current implementation creates Expr::Int(i64)
    // LSR 007 requires this to use BigInt internally
}

/// Test 2: Check if large integers beyond i64 max work
#[test]
#[should_panic(expected = "PosOverflow")]
fn test_large_integer_support() {
    // i64::MAX is 9,223,372,036,854,775,807
    // Let's test with a number larger than that
    let code = "var x = 99999999999999999999999999999;";
    
    // This should work if BigInt is the default
    // Currently it panics because lexer uses i64::parse()
    let _ = run_rumina(code);
}

/// Test 3: Check arithmetic with large numbers
#[test]
#[should_panic(expected = "overflow")]
fn test_large_integer_arithmetic() {
    // i64::MAX + 1 should work with BigInt, but overflows with i64
    let code = "var a = 9223372036854775807; var b = 1; a + b;";
    
    let _ = run_rumina(code);
    // Currently panics with "attempt to add with overflow"
}

/// Test 4: Demonstrate current behavior with bigint keyword
#[test]
#[should_panic(expected = "PosOverflow")]
fn test_bigint_keyword() {
    // The current implementation has a separate 'bigint' keyword
    // But even with bigint, the lexer can't parse large numbers
    let code = "bigint x = 99999999999999999999;";
    
    let _ = run_rumina(code);
    // Panics because lexer tries to parse as i64 first
}

/// Test 5: Check type_name of integer values
#[test]
fn test_integer_type_name() {
    // In LSR 007, all integers should be "int" type (but backed by BigInt)
    // Currently, there are separate "int" and "bigint" types
    let code = r#"
        var a = 42;
        var bigint b = 42;
        print(type(a));
        print(type(b));
    "#;
    
    // Note: This requires a type() builtin which may not exist
    // Just demonstrating the concept
    let result = run_rumina(code);
    
    match result {
        Ok(_) => println!("Type checking test completed"),
        Err(e) => println!("Type checking test error (expected): {}", e),
    }
}

#[test]
fn test_lsr_007_summary() {
    println!("\n=== LSR 007 Compliance Check ===\n");
    
    println!("LSR 007 Requirements:");
    println!("1. 舍弃原来的int部分 (Discard the old int part)");
    println!("2. 将BigInt改为Lamina标准整数Int (Rename BigInt to standard Int)");
    println!("3. 默认使用大整数来计算 (Use big integers by default)\n");
    
    println!("Current Implementation:");
    println!("- Value::Int(i64) - uses 64-bit signed integers");
    println!("- Value::BigInt(BigInt) - uses arbitrary precision");
    println!("- Default integer literals → Value::Int(i64)");
    println!("- bigint keyword → Value::BigInt(BigInt)\n");
    
    println!("Compliance Status: ✗ NOT COMPLIANT");
    println!("Reason: Integer literals use i64 by default, not BigInt\n");
    
    println!("To achieve LSR 007 compliance:");
    println!("- Replace Value::Int(i64) with Value::Int(BigInt)");
    println!("- Remove Value::BigInt variant");
    println!("- All integer operations use arbitrary precision");
    println!("- No performance optimization for small integers (or use internal optimization)");
}
