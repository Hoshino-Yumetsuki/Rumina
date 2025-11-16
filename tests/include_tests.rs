/// Test include statement compilation
use rumina::{Compiler, Lexer, Parser, run_rumina_with_dir};
use std::fs;

#[test]
fn test_include_statement_compilation() {
    // Create temporary test files
    let temp_dir = std::env::temp_dir().join("rumina_include_test");
    fs::create_dir_all(&temp_dir).unwrap();

    // Create the included file
    let included_file = temp_dir.join("test_module.lm");
    fs::write(
        &included_file,
        r#"define module_name = "test_module";
var pi = 3.14159;
func add(a, b) {
    return a + b;
}
"#,
    )
    .unwrap();

    // Create the main file
    let main_file = temp_dir.join("main.lm");
    fs::write(
        &main_file,
        r#"include "test_module.lm";
var x = test_module::pi;
var y = test_module::add(10, 20);
"#,
    )
    .unwrap();

    // Test compilation
    let source = fs::read_to_string(&main_file).unwrap();
    let temp_dir_str = temp_dir.to_str().unwrap().to_string();

    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().expect("Failed to parse");

    let mut compiler = Compiler::with_current_dir(temp_dir_str.clone());
    let bytecode = compiler.compile(ast).expect("Failed to compile");

    // Check that bytecode contains namespaced variables
    let serialized = bytecode.serialize();
    assert!(
        serialized.contains("test_module::pi"),
        "Bytecode should contain namespaced variable"
    );
    assert!(
        serialized.contains("test_module::add"),
        "Bytecode should contain namespaced function"
    );

    // Test execution
    let source = fs::read_to_string(&main_file).unwrap();
    let result = run_rumina_with_dir(&source, Some(temp_dir_str));
    assert!(result.is_ok(), "Execution should succeed");

    // Cleanup
    fs::remove_dir_all(&temp_dir).ok();
}

#[test]
fn test_include_prevents_circular_includes() {
    // Create temporary test files
    let temp_dir = std::env::temp_dir().join("rumina_circular_test");
    fs::create_dir_all(&temp_dir).unwrap();

    // Create file A that includes B
    let file_a = temp_dir.join("a.lm");
    fs::write(&file_a, r#"include "b.lm";"#).unwrap();

    // Create file B that includes A (circular)
    let file_b = temp_dir.join("b.lm");
    fs::write(&file_b, r#"include "a.lm";"#).unwrap();

    // Test compilation should not hang or error
    let source = fs::read_to_string(&file_a).unwrap();
    let temp_dir_str = temp_dir.to_str().unwrap().to_string();

    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().expect("Failed to parse");

    let mut compiler = Compiler::with_current_dir(temp_dir_str);
    let result = compiler.compile(ast);

    // Should succeed (circular includes are prevented by tracking)
    assert!(result.is_ok(), "Circular includes should be handled");

    // Cleanup
    fs::remove_dir_all(&temp_dir).ok();
}

#[test]
fn test_include_namespace_function_call() {
    // Create temporary test files
    let temp_dir = std::env::temp_dir().join("rumina_namespace_call_test");
    fs::create_dir_all(&temp_dir).unwrap();

    // Create the included file
    let included_file = temp_dir.join("math_utils.lm");
    fs::write(
        &included_file,
        r#"define module_name = "math_utils";
func multiply(x, y) {
    return x * y;
}
"#,
    )
    .unwrap();

    // Create the main file
    let main_file = temp_dir.join("caller.lm");
    fs::write(
        &main_file,
        r#"include "math_utils.lm";
math_utils::multiply(5, 6);
"#,
    )
    .unwrap();

    // Test compilation and execution
    let source = fs::read_to_string(&main_file).unwrap();
    let temp_dir_str = temp_dir.to_str().unwrap().to_string();

    let result = run_rumina_with_dir(&source, Some(temp_dir_str));
    assert!(result.is_ok(), "Execution should succeed");

    // The result should be 30 (5 * 6)
    if let Ok(Some(value)) = result {
        assert_eq!(value.to_string(), "30");
    }

    // Cleanup
    fs::remove_dir_all(&temp_dir).ok();
}
