use rumina::{
    Lexer, Parser, Value,
    ast::{ExtensionParamOwnership, Stmt},
    run_rumina,
};

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

fn expect_vector(result: Result<Option<Value>, rumina::RuminaError>) -> Vec<Value> {
    match result.unwrap() {
        Some(Value::Vector(values)) => values.borrow().clone(),
        other => panic!("Expected Vector, got {:?}", other),
    }
}

fn expect_matrix(result: Result<Option<Value>, rumina::RuminaError>) -> Vec<Vec<Value>> {
    match result.unwrap() {
        Some(Value::Matrix(rows)) => rows.borrow().clone(),
        other => panic!("Expected Matrix, got {:?}", other),
    }
}

fn expect_set(result: Result<Option<Value>, rumina::RuminaError>) -> Value {
    match result.unwrap() {
        Some(value @ Value::Set(_)) => value,
        other => panic!("Expected Set, got {:?}", other),
    }
}

fn expect_float(result: Result<Option<Value>, rumina::RuminaError>) -> f64 {
    match result.unwrap() {
        Some(Value::Float(f)) => f,
        other => panic!("Expected Float, got {:?}", other),
    }
}

fn expect_string(result: Result<Option<Value>, rumina::RuminaError>) -> String {
    match result.unwrap() {
        Some(Value::String(s)) => s,
        other => panic!("Expected String, got {:?}", other),
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
fn test_const_requires_initializer() {
    assert!(
        run_rumina("const x;").is_err(),
        "const declarations must initialize"
    );
}

#[test]
fn test_lsr_declaration_type_annotation_after_name() {
    let result = expect_int(run_rumina("let x num = 42; x;"));
    assert_eq!(result, 42);
}

#[test]
fn test_lsr_extension_table_type_annotation() {
    let source = r#"
        module scores {
            func score_of(t table<text,num>, name text) -> num = "c_ext_score_of"
        }
    "#;
    let tokens = Lexer::new(source.to_string()).tokenize();
    let mut parser = Parser::new(tokens);
    let statements = parser.parse().unwrap();

    match &statements[0] {
        Stmt::ExtensionModule { functions, .. } => {
            assert_eq!(functions[0].params[0].1, "table<text,num>");
            assert_eq!(functions[0].params[1].1, "text");
            assert_eq!(functions[0].return_type, "num");
        }
        other => panic!("expected extension module, got {other:?}"),
    }
}

#[test]
fn test_lsr_comparison_with_identifier_is_not_unit_attach() {
    let result = run_rumina("let x = 2; 1 < x;").unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
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
fn test_lsr_broadcast_vector_scalar_add() {
    let values = expect_vector(run_rumina("vec[1, 2, 3] .+ 1;"));
    assert_eq!(values, vec![Value::Int(2), Value::Int(3), Value::Int(4)]);
}

#[test]
fn test_legacy_logical_operators_are_rejected() {
    assert!(
        run_rumina("true && true;").is_err(),
        "&& should be rejected"
    );
    assert!(
        run_rumina("true || false;").is_err(),
        "|| should be rejected"
    );
    assert!(run_rumina("!false;").is_err(), "! should be rejected");
}

#[test]
fn test_legacy_type_first_declaration_is_rejected() {
    assert!(
        run_rumina("num x = 42;").is_err(),
        "type-first declarations should be rejected"
    );
    assert!(
        run_rumina("int x = 42;").is_err(),
        "type-first declarations should be rejected"
    );
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
    let g = expect_float(run_rumina(
        "use std.constants.{EARTH_GRAVITY}; EARTH_GRAVITY;",
    ));
    assert!(
        (g - 9.80665).abs() < 1e-12,
        "unexpected EARTH_GRAVITY: {}",
        g
    );
}

#[test]
fn test_lsr004_import_std_constants() {
    let g = expect_float(run_rumina("import std.constants; constants.EARTH_GRAVITY;"));
    assert!(
        (g - 9.80665).abs() < 1e-12,
        "unexpected EARTH_GRAVITY: {}",
        g
    );
}

#[test]
fn test_lsr004_import_std_math() {
    let result = expect_int(run_rumina("import std.math; math.sqrt(9);"));
    assert_eq!(result, 3);
}

#[test]
fn test_lsr004_import_alias() {
    let result = expect_int(run_rumina("import std.math as m; m.sqrt(9);"));
    assert_eq!(result, 3);
}

#[test]
fn test_lsr004_use_alias() {
    let result = expect_float(run_rumina("use std.constants.{G as GRAVITY}; GRAVITY;"));
    assert!(
        (result - 6.67430e-11).abs() < 1e-20,
        "unexpected GRAVITY: {}",
        result
    );
}

#[test]
fn test_lsr004_use_std_math_functions() {
    let result = expect_float(run_rumina(
        "use std.math.{pow, log, log10, floor, ceil, round, clamp}; pow(2, 3) + log(1) + log10(100) + floor(1.9) + ceil(1.1) + round(1.6) + clamp(10, 0, 3);",
    ));
    assert!(
        (result - 18.0).abs() < 1e-10,
        "expected 18.0, got {}",
        result
    );
}

#[test]
fn test_lsr004_std_math_constants() {
    let result = expect_float(run_rumina("import std.math; math.phi;"));
    assert!(
        (result - 1.618033988749895).abs() < 1e-12,
        "unexpected phi: {}",
        result
    );
}

#[test]
fn test_lsr004_std_io_format_is_deterministic() {
    let imported = expect_string(run_rumina("import std.io; io.format([1, 2, 3]);"));
    assert_eq!(imported, "[1, 2, 3]");

    let used = expect_string(run_rumina("use std.io.{format}; format(mat[1, 2; 3, 4]);"));
    assert_eq!(used, "mat[1, 2; 3, 4]");
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
    let normal = expect_float(run_rumina(
        "use std.random.{seed, normal}; seed(7); normal(0, 1);",
    ));
    assert!(
        normal.is_finite(),
        "normal sample should be finite: {}",
        normal
    );

    let choice = expect_int(run_rumina(
        "use std.random.{seed, choice}; seed(7); choice([10, 20, 30]);",
    ));
    assert!(
        [10, 20, 30].contains(&choice),
        "choice returned unexpected value: {}",
        choice
    );
}

#[test]
fn test_lsr004_std_stats_descriptive_functions() {
    let result = expect_float(run_rumina(
        "use std.stats.{mean, median, var as variance, std, quantile}; mean([1, 2, 3, 4]) + median([1, 2, 3, 4]) + variance([1, 2, 3, 4]) + std([1, 2, 3, 4]) + quantile([1, 2, 3, 4], 0.5);",
    ));
    let expected = 2.5 + 2.5 + 1.25 + 1.25_f64.sqrt() + 2.5;
    assert!(
        (result - expected).abs() < 1e-10,
        "expected {}, got {}",
        expected,
        result
    );
}

#[test]
fn test_lsr004_stdlib_diagnostic_codes() {
    let domain_error = run_rumina("import std.math; math.log(-1);").unwrap_err();
    assert!(
        domain_error.to_string().contains("DomainError"),
        "expected DomainError diagnostic, got {domain_error}"
    );

    let empty_input = run_rumina("import std.stats; stats.mean([]);").unwrap_err();
    assert!(
        empty_input.to_string().contains("EmptyInput"),
        "expected EmptyInput diagnostic, got {empty_input}"
    );
}

#[test]
fn test_lsr004_std_stats_cov_corr() {
    let result = expect_float(run_rumina(
        "use std.stats.{cov, corr}; cov([1, 2, 3], [2, 4, 6]) + corr([1, 2, 3], [2, 4, 6]);",
    ));
    assert!(
        (result - (4.0 / 3.0 + 1.0)).abs() < 1e-10,
        "unexpected cov+corr: {}",
        result
    );
}

#[test]
fn test_lsr004_std_units_dimensionless_helpers() {
    let result = run_rumina(
        "use std.units.{strip, is_dimensionless, convert}; strip(42) == 42 and is_dimensionless(42) and convert(42, \"\") == 42;",
    )
    .unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr000_matrix_direct_two_dimensional_indexing() {
    let result = expect_int(run_rumina("let m = mat[1, 2, 3; 4, 5, 6]; m[2, 3];"));
    assert_eq!(result, 6);
}

#[test]
fn test_lsr000_matrix_column_wildcard_slice_returns_vector() {
    let values = expect_vector(run_rumina("let m = mat[1, 2, 3; 4, 5, 6]; m[*, 2];"));
    assert_eq!(values, vec![Value::Int(2), Value::Int(5)]);
}

#[test]
fn test_lsr000_matrix_row_wildcard_slice_returns_vector() {
    let values = expect_vector(run_rumina("let m = mat[1, 2, 3; 4, 5, 6]; m[2, *];"));
    assert_eq!(values, vec![Value::Int(4), Value::Int(5), Value::Int(6)]);
}

#[test]
fn test_lsr000_matrix_postfix_non_conjugate_transpose() {
    let rows = expect_matrix(run_rumina("mat[1, 2; 3, 4].';"));

    assert_eq!(
        rows,
        vec![
            vec![Value::Int(1), Value::Int(3)],
            vec![Value::Int(2), Value::Int(4)]
        ]
    );
}

#[test]
fn test_lsr004_std_linalg_shape_transpose_trace() {
    let result = run_rumina(
        "import std.linalg as la; let m = mat[1, 2; 3, 4]; let s = la.shape(m); let t = la.transpose(m); s[1] == 2 and s[2] == 2 and t[1][2] == 3 and la.trace(m) == 5;",
    )
    .unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr004_std_linalg_det() {
    let result = expect_int(run_rumina("use std.linalg.{det}; det(mat[1, 2; 3, 4]);"));
    assert_eq!(result, -2);
}

#[test]
fn test_lsr004_std_linalg_adjoint_inverse_rank() {
    let result = run_rumina(
        "import std.linalg as la; let m = mat[4, 7; 2, 6]; let a = la.adjoint(m); let inv = la.inv(m); a[1][2] == 2 and inv[1][1] == 0.6 and inv[1][2] == -0.7 and la.rank(m) == 2;",
    )
    .unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr004_std_linalg_solve_left_and_right() {
    let result = run_rumina(
        "import std.linalg as la; let a = mat[2, 0; 0, 4]; let b = mat[8; 12]; let x = la.solve_left(a, b); let y = la.solve_right(mat[8, 12], a); x[1][1] == 4 and x[2][1] == 3 and y[1][1] == 4 and y[1][2] == 3;",
    )
    .unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr004_std_linalg_eig_diagonal_matrix() {
    let result = run_rumina(
        "import std.linalg as la; let e = la.eig(mat[2, 0; 0, 3]); e.values[1][1] == 2 and e.values[2][1] == 3 and e.vectors[1][1] == 1 and e.vectors[2][2] == 1;",
    )
    .unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr004_std_linalg_svd_diagonal_matrix() {
    let result = run_rumina(
        "import std.linalg as la; let s = la.svd(mat[4, 0; 0, 2]); s.S[1][1] == 4 and s.S[2][2] == 2 and s.U[1][1] == 1 and s.V[2][2] == 1;",
    )
    .unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr005_match_constant_and_wildcard_patterns() {
    let result = expect_int(run_rumina("match 2 { 1 => 10, 2 => 20, _ => 0 };"));
    assert_eq!(result, 20);

    let fallback = expect_int(run_rumina("match 9 { 1 => 10, 2 => 20, _ => 0 };"));
    assert_eq!(fallback, 0);
}

#[test]
fn test_lsr005_match_binding_and_guard_patterns() {
    let result = expect_int(run_rumina("match 5 { v if v > 3 => v + 1, _ => 0 };"));
    assert_eq!(result, 6);
}

#[test]
fn test_lsr005_match_vector_destructuring_pattern() {
    let result = expect_int(run_rumina(
        "match vec[20, 22] { vec[a, b] => a + b, _ => 0 };",
    ));
    assert_eq!(result, 42);
}

#[test]
fn test_lsr005_match_wildcard_must_be_last() {
    assert!(run_rumina("match 1 { _ => 0, 1 => 1 };").is_err());
}

#[test]
fn test_lsr005_open_match_without_wildcard_reports_missing_wildcard() {
    let error = run_rumina("match 3 { 1 => 10, 2 => 20 };").unwrap_err();
    assert!(
        error.to_string().contains("MissingWildcard"),
        "expected MissingWildcard diagnostic, got {error}"
    );
}

#[test]
fn test_lsr005_exhaustive_bool_match_does_not_require_wildcard() {
    let result = expect_int(run_rumina("match true { true => 1, false => 0 };"));
    assert_eq!(result, 1);
}

#[test]
fn test_lsr005_match_duplicate_constant_arm_is_unreachable() {
    let error = run_rumina("match 1 { 1 => 10, 1 => 20, _ => 0 };").unwrap_err();
    assert!(
        error.to_string().contains("UnreachablePattern"),
        "expected UnreachablePattern diagnostic, got {error}"
    );
}

#[test]
fn test_lsr007_core_equivalence_identities() {
    let result = run_rumina("let x = 9; x + 0 === x and x * 1 === x and x - x === 0;").unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr007_equivalence_is_not_structural_equality() {
    let result = run_rumina("1 + 2 === 3 and not (1 + 2 === 4);").unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr007_core_equivalence_commutative_add_and_mul() {
    let result = run_rumina("x + 1 === 1 + x and x * 2 === 2 * x;").unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr007_core_equivalence_non_commutative_identities() {
    let result = run_rumina("x - 0 === x and x / 1 === x and x^1 === x and x^0 === 1;").unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr007_equivalence_rejects_bool_operands() {
    let error = run_rumina("true === true;").unwrap_err();
    assert!(
        error.to_string().contains("EqvTypeMismatch"),
        "expected EqvTypeMismatch diagnostic, got {error}"
    );
}

#[test]
fn test_lsr000_table_literal_and_read() {
    let result = expect_int(run_rumina(
        "let scores = table{\"alice\" => 98}; scores[\"alice\"];",
    ));
    assert_eq!(result, 98);
}

#[test]
fn test_lsr000_table_missing_key_returns_null() {
    let result =
        run_rumina("let scores = table{\"alice\" => 98}; scores[\"bob\"] == null;").unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr000_table_write_overwrites_existing_key() {
    let result = run_rumina(
        "var scores = table{\"alice\" => 98}; scores[\"alice\"] = 100; scores[\"alice\"] == 100;",
    )
    .unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr000_table_write_adds_new_key() {
    let result = run_rumina(
        "var scores = table{\"alice\" => 98}; scores[\"bob\"] = 91; scores[\"bob\"] == 91;",
    )
    .unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr000_table_write_requires_mutable_binding() {
    let result = run_rumina("let scores = table{\"alice\" => 98}; scores[\"alice\"] = 100;");
    assert!(
        result.is_err(),
        "table writes through let bindings should error"
    );
}

#[test]
fn test_lsr000_table_has_helper_returns_bool() {
    let result = run_rumina(
        "let scores = table{\"alice\" => 98}; scores.has(\"alice\") and not scores.has(\"bob\");",
    )
    .unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr000_table_keys_helper_returns_keys() {
    let mut keys = expect_vector(run_rumina(
        "let scores = table{\"alice\" => 98, \"bob\" => 91}; scores.keys();",
    ));
    keys.sort_by_key(|value| value.to_string());
    assert_eq!(
        keys,
        vec![
            Value::String("alice".to_string()),
            Value::String("bob".to_string()),
        ]
    );
}

#[test]
fn test_lsr000_table_values_helper_returns_values() {
    let mut values = expect_vector(run_rumina(
        "let scores = table{\"alice\" => 98, \"bob\" => 91}; scores.values();",
    ));
    values.sort_by_key(|value| value.to_string());
    assert_eq!(values, vec![Value::Int(91), Value::Int(98)]);
}

#[test]
fn test_lsr000_table_items_helper_returns_key_value_vectors() {
    let mut items = expect_vector(run_rumina(
        "let scores = table{\"alice\" => 98, \"bob\" => 91}; scores.items();",
    ));
    items.sort_by_key(|value| value.to_string());

    let rendered: Vec<String> = items.into_iter().map(|value| value.to_string()).collect();
    assert_eq!(rendered, vec!["vec[alice, 98]", "vec[bob, 91]"]);
}

#[test]
fn test_lsr000_vector_literal_uses_one_based_indexing() {
    let first = expect_int(run_rumina("let xs = vec[10, 20, 30]; xs[1];"));
    assert_eq!(first, 10);

    let third = expect_int(run_rumina("let xs = vec[10, 20, 30]; xs[3];"));
    assert_eq!(third, 30);
}

#[test]
fn test_lsr000_vector_zero_index_is_invalid() {
    assert!(run_rumina("let xs = vec[10, 20, 30]; xs[0];").is_err());
}

#[test]
fn test_lsr000_matrix_literal_uses_one_based_indexing() {
    let result = expect_int(run_rumina("let m = mat[1, 2; 3, 4]; m[1][2];"));
    assert_eq!(result, 2);
}

#[test]
fn test_lsr000_set_literal_deduplicates_values() {
    let result = run_rumina("let a = {1, 2, 2}; let b = {1, 2}; a == b;").unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr000_set_membership_operators() {
    let result = run_rumina("2 in {1, 2, 3} and 4 not in {1, 2, 3};").unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr000_set_subset_operator() {
    let contained = run_rumina("let a = {1, 2}; let b = {1, 2, 3}; a subset b;").unwrap();
    match contained {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }

    let missing = run_rumina("let a = {1, 4}; let b = {1, 2, 3}; a subset b;").unwrap();
    match missing {
        Some(Value::Bool(b)) => assert!(!b),
        other => panic!("Expected Bool(false), got {:?}", other),
    }
}

#[test]
fn test_lsr000_set_binary_operators_return_sets() {
    assert_eq!(
        expect_set(run_rumina("let a = {1, 2}; let b = {2, 3}; a | b;")),
        Value::Set(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
    );
    assert_eq!(
        expect_set(run_rumina("let a = {1, 2}; let b = {2, 3}; a & b;")),
        Value::Set(vec![Value::Int(2)])
    );
    assert_eq!(
        expect_set(run_rumina("let a = {1, 2, 3}; let b = {2}; a - b;")),
        Value::Set(vec![Value::Int(1), Value::Int(3)])
    );
    assert_eq!(
        expect_set(run_rumina("let a = {1, 2}; let b = {2, 3}; a xor b;")),
        Value::Set(vec![Value::Int(1), Value::Int(3)])
    );
}

#[test]
fn test_lsr008_strip_dimensionless_num_and_scalar() {
    let result = run_rumina("(42 as num) == 42 and (42 as scalar) == 42;").unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr008_strip_metric_unit_literal_as_num() {
    let result = run_rumina("(10<m> as num) == 10 and (10<km> as num) == 10000;").unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr008_strip_metric_unit_literal_as_scalar() {
    let result = run_rumina("(10<km> as scalar) == 10;").unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr008_strip_abstract_declared_unit_literal_as_num() {
    let result = run_rumina("unit score; (10<score> as num) == 10;").unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr008_convert_metric_unit_literal_to_target_unit() {
    let meters = run_rumina("10<km> as m;").unwrap();
    assert_eq!(
        meters,
        Some(Value::UnitNumber {
            value: Box::new(Value::Int(10000)),
            unit: "m".to_string(),
            scale: 1,
        })
    );
}

#[test]
fn test_lsr008_strip_after_metric_unit_conversion_uses_target_scale() {
    let result =
        run_rumina("((10<km> as m) as num) == 10000 and ((10<m> as km) as num) == (1 / 100);")
            .unwrap();
    match result {
        Some(Value::Bool(b)) => assert!(b),
        other => panic!("Expected Bool(true), got {:?}", other),
    }
}

#[test]
fn test_lsr008_rejects_incompatible_unit_conversion() {
    let err = run_rumina("unit score; 10<m> as score;").unwrap_err();
    assert!(
        err.to_string().contains("UnitStripInvalid"),
        "expected UnitStripInvalid diagnostic, got {err}"
    );
}

#[test]
fn test_lsr008_rejects_legacy_num_unit_syntax() {
    assert!(run_rumina("42 as num<m>;").is_err());
}

#[test]
fn test_lsr000_unit_declarations_are_accepted() {
    let result = expect_int(run_rumina("unit score; unit level = 100; 1;"));
    assert_eq!(result, 1);
}

#[test]
fn test_lsr006_lambda_accepts_typed_params() {
    let result = expect_int(run_rumina("let inc = |x num| -> x + 1; inc(41);"));
    assert_eq!(result, 42);
}

#[test]
fn test_lsr006_lambda_arity_mismatch_reports_diagnostic() {
    let err = run_rumina("let add = |x num, y num| -> x + y; add(1);").unwrap_err();
    assert!(
        err.to_string().contains("LambdaArityMismatch"),
        "expected LambdaArityMismatch diagnostic, got {err}"
    );
}

#[test]
fn test_lsr006_lambda_capture_mutation_reports_diagnostic() {
    let err = run_rumina("var counter = 0; let bump = do { counter = counter + 1; }; bump();")
        .unwrap_err();
    assert!(
        err.to_string().contains("LambdaCaptureMutation"),
        "expected LambdaCaptureMutation diagnostic, got {err}"
    );
}

#[test]
fn test_lsr000_function_accepts_typed_signature() {
    let result = expect_int(run_rumina(
        "func add(x num, y num) -> num { return x + y; } add(20, 22);",
    ));
    assert_eq!(result, 42);
}

#[test]
fn test_lsr003_extension_module_signature_parses() {
    let mut lexer = rumina::Lexer::new(
        r#"
module example {
  func det(m matrix) -> num = "c_ext_det"
}
"#
        .to_string(),
    );
    let mut parser = rumina::Parser::new(lexer.tokenize());
    let ast = parser
        .parse()
        .expect("extension module interface should parse");

    match &ast[..] {
        [rumina::ast::Stmt::ExtensionModule { name, functions }] => {
            assert_eq!(name, "example");
            assert_eq!(functions.len(), 1);
            assert_eq!(functions[0].name, "det");
            assert_eq!(
                functions[0].params,
                vec![("m".to_string(), "matrix".to_string())]
            );
            assert_eq!(functions[0].return_type, "num");
            assert_eq!(functions[0].symbol, "c_ext_det");
        }
        other => panic!("expected one extension module, got {other:?}"),
    }
}

#[test]
fn test_lsr003_extension_param_move_annotation_parses() {
    let mut lexer = rumina::Lexer::new(
        r#"
module example {
  func consume(t table<string,num> @move) -> bool = "c_ext_consume";
}
"#
        .to_string(),
    );
    let mut parser = rumina::Parser::new(lexer.tokenize());
    let ast = parser
        .parse()
        .expect("extension ownership annotation should parse");

    match &ast[..] {
        [rumina::ast::Stmt::ExtensionModule { functions, .. }] => {
            assert_eq!(
                functions[0].params[0],
                ("t".to_string(), "table<string,num>".to_string())
            );
            assert_eq!(
                functions[0].param_ownership[0],
                ExtensionParamOwnership::Move
            );
        }
        other => panic!("expected one extension module, got {other:?}"),
    }
}

#[test]
fn test_lsr003_extension_func_requires_c_symbol_string() {
    let mut lexer = rumina::Lexer::new(
        "module example { func det(m matrix) -> num { return 0; } }".to_string(),
    );
    let mut parser = rumina::Parser::new(lexer.tokenize());
    let error = parser.parse().unwrap_err();

    assert!(
        error.contains("InterfaceBindError") && error.contains("string C symbol"),
        "expected InterfaceBindError for missing C symbol string, got {error}"
    );
}

#[test]
fn test_lsr003_extension_declaration_exposes_callable_stub() {
    let result = run_rumina(
        r#"
module example {
  func det(x num) -> num = "c_ext_det";
}
typeof(example::det);
"#,
    )
    .unwrap();

    match result {
        Some(Value::String(kind)) => assert_eq!(kind, "extension_function"),
        other => panic!("Expected extension_function typeof result, got {other:?}"),
    }
}

#[test]
fn test_lsr003_extension_stub_call_routes_to_bound_symbol_diagnostic() {
    let err = run_rumina(
        r#"
module example {
  func det(x num) -> num = "c_ext_det";
}
example::det(1);
"#,
    )
    .unwrap_err();
    let message = err.to_string();

    assert!(
        message.contains("ExtensionRuntimeUnavailable"),
        "expected extension runtime diagnostic, got {message}"
    );
    assert!(
        message.contains("example::det") && message.contains("c_ext_det"),
        "expected routed function and C symbol in diagnostic, got {message}"
    );
}
