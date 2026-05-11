use rumina::run_rumina;

fn run_value_to_string(source: &str) -> String {
    let result = run_rumina(source).expect("program should execute without numeric overflow");
    result.expect("program should produce a value").to_string()
}

#[test]
fn parses_integer_literal_beyond_i64_max() {
    let value = run_value_to_string("9223372036854775808;");

    assert_eq!(value, "9223372036854775808");
}

#[test]
fn parses_decimal_literal_beyond_i64_scale_limit_exactly() {
    let value = run_value_to_string("0.0000000000000000001;");

    assert_eq!(value, "1/10000000000000000000");
}

#[test]
fn adds_large_integers_exactly() {
    let value = run_value_to_string("9223372036854775808 + 1;");

    assert_eq!(value, "9223372036854775809");
}

#[test]
fn promotes_i64_addition_overflow_to_big_integer() {
    let value = run_value_to_string("9223372036854775807 + 1;");

    assert_eq!(value, "9223372036854775808");
}

#[test]
fn promotes_i64_subtraction_overflow_to_big_integer() {
    let value = run_value_to_string("-9223372036854775807 - 2;");

    assert_eq!(value, "-9223372036854775809");
}

#[test]
fn promotes_i64_multiplication_overflow_to_big_integer() {
    let value = run_value_to_string("3037000500 * 3037000500;");

    assert_eq!(value, "9223372037000250000");
}
