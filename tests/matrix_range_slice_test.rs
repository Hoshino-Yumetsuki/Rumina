use rumina::{Value, run_rumina};

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

#[test]
fn test_lsr000_matrix_row_range_column_slice_returns_vector() {
    let values = expect_vector(run_rumina(
        "let m = mat[1, 2, 3; 4, 5, 6; 7, 8, 9]; m[1..2, 2];",
    ));

    assert_eq!(values, vec![Value::Int(2), Value::Int(5)]);
}

#[test]
fn test_lsr000_matrix_row_range_column_range_slice_returns_matrix() {
    let rows = expect_matrix(run_rumina(
        "let m = mat[1, 2, 3; 4, 5, 6; 7, 8, 9]; m[1..2, 2..3];",
    ));

    assert_eq!(
        rows,
        vec![
            vec![Value::Int(2), Value::Int(3)],
            vec![Value::Int(5), Value::Int(6)],
        ]
    );
}
