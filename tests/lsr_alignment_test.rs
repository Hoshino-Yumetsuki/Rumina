use rumina::{Value, run_rumina};

fn expect_int(result: Result<Option<Value>, rumina::RuminaError>) -> i64 {
    match result.unwrap() {
        Some(Value::Int(n)) => n,
        other => panic!("Expected Int, got {:?}", other),
    }
}

fn expect_array(result: Result<Option<Value>, rumina::RuminaError>) -> Vec<Value> {
    match result.unwrap() {
        Some(Value::Array(values)) => values.borrow().clone(),
        other => panic!("Expected Array, got {:?}", other),
    }
}

fn expect_float(result: Result<Option<Value>, rumina::RuminaError>) -> f64 {
    match result.unwrap() {
        Some(Value::Float(f)) => f,
        other => panic!("Expected Float, got {:?}", other),
    }
}

#[test]
fn test_let_is_immutable() {
    let result = run_rumina("let x = 1; x = 2;");
    assert!(result.is_err(), "reassigning let should error");
}

#[test]
fn test_const_is_immutable() {
    let result = run_rumina("const x = 1; x = 2;");
    assert!(result.is_err(), "reassigning const should error");
}

#[test]
fn test_lsr_declaration_type_annotation_after_name() {
    let result = expect_int(run_rumina("let x num = 42; x;"));
    assert_eq!(result, 42);
}

#[test]
fn test_lsr_logical_keywords() {
    let result = run_rumina("true and not false or false;").unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_legacy_logical_operators_are_rejected() {
    assert!(run_rumina("true && true;").is_err(), "&& should be rejected");
    assert!(run_rumina("true || false;").is_err(), "|| should be rejected");
    assert!(run_rumina("!false;").is_err(), "! should be rejected");
}

#[test]
fn test_legacy_type_first_declaration_is_rejected() {
    assert!(run_rumina("num x = 42;").is_err(), "type-first declarations should be rejected");
    assert!(run_rumina("int x = 42;").is_err(), "type-first declarations should be rejected");
}

#[test]
fn test_lsr_lambda_requires_arrow() {
    let values = expect_array(run_rumina("var nums = [1, 2, 3]; nums.map(|x| -> x * x);"));
    assert_eq!(values, vec![Value::Int(1), Value::Int(4), Value::Int(9)]);

    let legacy = run_rumina("var nums = [1, 2, 3]; nums.map(|x| x * x);");
    assert!(legacy.is_err(), "lambda without -> should be rejected");
}

#[test]
fn test_let_member_assign_is_immutable() {
    let result = run_rumina("let s = null; s.a = 1;");
    assert!(
        result.is_err(),
        "member assignment on let binding should error"
    );
}

#[test]
fn test_pipeline_operator_basic() {
    let result = run_rumina("-3 |> abs;").unwrap();
    match result {
        Some(Value::Int(n)) => assert_eq!(n, 3),
        other => panic!("Expected Int(3), got {:?}", other),
    }
}

#[test]
fn test_array_instance_map() {
    let values = expect_array(run_rumina("var nums = [1, 2, 3]; nums.map(|x| -> x * x);"));
    assert_eq!(values, vec![Value::Int(1), Value::Int(4), Value::Int(9)]);
}

#[test]
fn test_array_instance_filter() {
    let values = expect_array(run_rumina(
        "var nums = [1, 2, 3, 4]; nums.filter(|x| -> x % 2 == 0);",
    ));
    assert_eq!(values, vec![Value::Int(2), Value::Int(4)]);
}

#[test]
fn test_array_instance_reduce() {
    let result = expect_int(run_rumina(
        "var nums = [1, 2, 3, 4]; nums.reduce(|acc, x| -> acc + x, 0);",
    ));
    assert_eq!(result, 10);
}

#[test]
fn test_try_catch_catches_runtime_error() {
    let result =
        run_rumina("var caught = \"\"; try { missing(); } catch (e) { caught = e; } caught;")
            .unwrap();
    match result {
        Some(Value::String(message)) => {
            assert!(
                message.contains("Undefined variable: missing"),
                "got {message}"
            );
        }
        other => panic!("Expected caught error string, got {:?}", other),
    }
}

#[test]
fn test_line_continuation_runtime() {
    let result = expect_int(run_rumina(
        "var x = 1\\
 + 2; x;",
    ));
    assert_eq!(result, 3);
}

#[test]
fn test_fold_alias_registered() {
    let result = run_rumina("typeof(fold);").unwrap();
    match result {
        Some(Value::String(s)) => assert_eq!(s, "native_function"),
        other => panic!("Expected String(native_function), got {:?}", other),
    }
}

#[test]
fn test_hash_comments_line_and_block() {
    let line = run_rumina("# line comment\n1 + 1;").unwrap();
    match line {
        Some(Value::Int(n)) => assert_eq!(n, 2),
        other => panic!("Expected Int(2), got {:?}", other),
    }

    let block = run_rumina("### block\ncomment ###\n2 + 3;").unwrap();
    match block {
        Some(Value::Int(n)) => assert_eq!(n, 5),
        other => panic!("Expected Int(5), got {:?}", other),
    }
}

#[test]
fn test_decimal_precision_argument() {
    let f = expect_float(run_rumina("decimal(1/3, 4);"));
    assert!((f - 0.3333).abs() < 1e-10, "expected 0.3333, got {}", f);
}

#[test]
fn test_log_family_semantics() {
    let log10 = expect_float(run_rumina("log(100);"));
    assert!((log10 - 2.0).abs() < 1e-10, "expected 2.0, got {}", log10);

    let ln = expect_float(run_rumina("ln(e());"));
    assert!((ln - 1.0).abs() < 1e-10, "expected 1.0, got {}", ln);

    let base = expect_float(run_rumina("logBASE(2, 8);"));
    assert!((base - 3.0).abs() < 1e-10, "expected 3.0, got {}", base);
}

#[test]
fn test_lsr002_constants_available() {
    let g = expect_float(run_rumina("EARTH_GRAVITY;"));
    assert!(
        (g - 9.80665).abs() < 1e-12,
        "unexpected EARTH_GRAVITY: {}",
        g
    );

    let avogadro = expect_float(run_rumina("AVOGADRO;"));
    assert!(
        (avogadro - 6.02214076e23).abs() / 6.02214076e23 < 1e-12,
        "unexpected AVOGADRO: {}",
        avogadro
    );
}

#[test]
fn test_lsr004_use_std_constants() {
    let g = expect_float(run_rumina("use std.constants.{EARTH_GRAVITY}; EARTH_GRAVITY;"));
    assert!((g - 9.80665).abs() < 1e-12, "unexpected EARTH_GRAVITY: {}", g);
}

#[test]
fn test_lsr004_import_std_constants() {
    let g = expect_float(run_rumina("import std.constants; constants.EARTH_GRAVITY;"));
    assert!((g - 9.80665).abs() < 1e-12, "unexpected EARTH_GRAVITY: {}", g);
}

#[test]
fn test_lsr004_import_std_math() {
    let result = expect_int(run_rumina("import std.math; math.sqrt(9);"));
    assert_eq!(result, 3);
}

#[test]
fn test_lsr004_use_std_math_functions() {
    let result = expect_float(run_rumina(
        "use std.math.{pow, log, log10, floor, ceil, round, clamp}; pow(2, 3) + log(1) + log10(100) + floor(1.9) + ceil(1.1) + round(1.6) + clamp(10, 0, 3);",
    ));
    assert!((result - 18.0).abs() < 1e-10, "expected 18.0, got {}", result);
}

#[test]
fn test_lsr004_std_math_constants() {
    let result = expect_float(run_rumina("import std.math; math.phi;"));
    assert!((result - 1.618033988749895).abs() < 1e-12, "unexpected phi: {}", result);
}

#[test]
fn test_lsr004_std_random_seed_is_reproducible() {
    let result = run_rumina(
        "use std.random.{seed, rand, randint}; seed(123); let a = rand(); let b = randint(1, 10); seed(123); a == rand() and b == randint(1, 10);",
    )
    .unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr004_std_random_normal_and_choice() {
    let normal = expect_float(run_rumina("use std.random.{seed, normal}; seed(7); normal(0, 1);"));
    assert!(normal.is_finite(), "normal sample should be finite: {}", normal);

    let choice = expect_int(run_rumina("use std.random.{seed, choice}; seed(7); choice([10, 20, 30]);"));
    assert!([10, 20, 30].contains(&choice), "choice returned unexpected value: {}", choice);
}
