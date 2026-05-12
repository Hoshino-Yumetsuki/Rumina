use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::value::Value;

const EPSILON: f64 = 1e-12;

fn matrix_arg<'a>(args: &'a [Value], name: &str) -> Result<std::cell::Ref<'a, Vec<Vec<Value>>>, String> {
    if args.len() != 1 {
        return Err(format!("linalg.{} expects 1 argument", name));
    }

    let Value::Matrix(rows) = &args[0] else {
        return Err(format!("linalg.{} expects matrix, got {}", name, args[0].type_name()));
    };

    Ok(rows.borrow())
}

pub fn shape(args: &[Value]) -> Result<Value, String> {
    let rows = matrix_arg(args, "shape")?;
    let cols = rows.first().map_or(0, Vec::len);
    Ok(Value::Vector(Rc::new(RefCell::new(vec![
        Value::Int(rows.len() as i64),
        Value::Int(cols as i64),
    ]))))
}

pub fn transpose(args: &[Value]) -> Result<Value, String> {
    let rows = matrix_arg(args, "transpose")?;
    Ok(Value::Matrix(Rc::new(RefCell::new(transpose_values(&rows)?))))
}

pub fn adjoint(args: &[Value]) -> Result<Value, String> {
    transpose(args)
}

pub fn trace(args: &[Value]) -> Result<Value, String> {
    let rows = matrix_arg(args, "trace")?;
    if rows.iter().any(|row| row.len() != rows.len()) {
        return Err("linalg.trace expects square matrix".to_string());
    }

    let mut total = 0.0;
    for (index, row) in rows.iter().enumerate() {
        total += row[index].to_float()?;
    }

    if total.fract() == 0.0 {
        Ok(Value::Int(total as i64))
    } else {
        Ok(Value::Float(total))
    }
}

pub fn det(args: &[Value]) -> Result<Value, String> {
    let rows = matrix_arg(args, "det")?;
    if rows.iter().any(|row| row.len() != rows.len()) {
        return Err("linalg.det expects square matrix".to_string());
    }

    let value = determinant(&rows)?;
    if value.fract() == 0.0 {
        Ok(Value::Int(value as i64))
    } else {
        Ok(Value::Float(value))
    }
}

pub fn inv(args: &[Value]) -> Result<Value, String> {
    let rows = matrix_arg(args, "inv")?;
    let values = numeric_matrix(&rows, "inv")?;
    let inverse = invert_matrix(values)?;
    Ok(float_matrix_value(inverse))
}

pub fn rank(args: &[Value]) -> Result<Value, String> {
    let rows = matrix_arg(args, "rank")?;
    let mut values = numeric_matrix(&rows, "rank")?;
    Ok(Value::Int(matrix_rank(&mut values) as i64))
}

pub fn solve_left(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("linalg.solve_left expects 2 arguments".to_string());
    }
    let Value::Matrix(a_rows) = &args[0] else {
        return Err(format!("linalg.solve_left expects matrix, got {}", args[0].type_name()));
    };
    let Value::Matrix(b_rows) = &args[1] else {
        return Err(format!("linalg.solve_left expects matrix, got {}", args[1].type_name()));
    };

    let inverse = invert_matrix(numeric_matrix(&a_rows.borrow(), "solve_left")?)?;
    let b = numeric_matrix(&b_rows.borrow(), "solve_left")?;
    Ok(float_matrix_value(multiply_matrices(&inverse, &b)?))
}

pub fn solve_right(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("linalg.solve_right expects 2 arguments".to_string());
    }
    let Value::Matrix(b_rows) = &args[0] else {
        return Err(format!("linalg.solve_right expects matrix, got {}", args[0].type_name()));
    };
    let Value::Matrix(a_rows) = &args[1] else {
        return Err(format!("linalg.solve_right expects matrix, got {}", args[1].type_name()));
    };

    let b = numeric_matrix(&b_rows.borrow(), "solve_right")?;
    let inverse = invert_matrix(numeric_matrix(&a_rows.borrow(), "solve_right")?)?;
    Ok(float_matrix_value(multiply_matrices(&b, &inverse)?))
}

pub fn eig(args: &[Value]) -> Result<Value, String> {
    let rows = matrix_arg(args, "eig")?;
    let values = numeric_matrix(&rows, "eig")?;
    if !is_diagonal(&values) {
        return Err("linalg.eig currently supports diagonal matrices".to_string());
    }

    let eigenvalues = values
        .iter()
        .enumerate()
        .map(|(index, row)| vec![row[index]])
        .collect();
    let mut result = HashMap::new();
    result.insert("values".to_string(), float_matrix_value(eigenvalues));
    result.insert("vectors".to_string(), float_matrix_value(identity(values.len())));
    Ok(Value::Struct(Rc::new(RefCell::new(result))))
}

pub fn svd(args: &[Value]) -> Result<Value, String> {
    let rows = matrix_arg(args, "svd")?;
    let values = numeric_matrix(&rows, "svd")?;
    if !is_diagonal(&values) {
        return Err("linalg.svd currently supports diagonal matrices".to_string());
    }

    let singular_values = values
        .iter()
        .enumerate()
        .map(|(index, row)| {
            let mut out = vec![0.0; row.len()];
            out[index] = row[index].abs();
            out
        })
        .collect();
    let mut result = HashMap::new();
    result.insert("U".to_string(), float_matrix_value(identity(values.len())));
    result.insert("S".to_string(), float_matrix_value(singular_values));
    result.insert("V".to_string(), float_matrix_value(identity(values.first().map_or(0, Vec::len))));
    Ok(Value::Struct(Rc::new(RefCell::new(result))))
}

fn determinant(rows: &[Vec<Value>]) -> Result<f64, String> {
    match rows.len() {
        0 => Ok(1.0),
        1 => rows[0][0].to_float(),
        2 => Ok(rows[0][0].to_float()? * rows[1][1].to_float()?
            - rows[0][1].to_float()? * rows[1][0].to_float()?),
        size => {
            let mut total = 0.0;
            for col in 0..size {
                let mut minor = Vec::with_capacity(size - 1);
                for row in rows.iter().skip(1) {
                    let mut minor_row = Vec::with_capacity(size - 1);
                    for (index, value) in row.iter().enumerate() {
                        if index != col {
                            minor_row.push(value.clone());
                        }
                    }
                    minor.push(minor_row);
                }

                let sign = if col % 2 == 0 { 1.0 } else { -1.0 };
                total += sign * rows[0][col].to_float()? * determinant(&minor)?;
            }
            Ok(total)
        }
    }
}

fn transpose_values(rows: &[Vec<Value>]) -> Result<Vec<Vec<Value>>, String> {
    let row_count = rows.len();
    let col_count = rows.first().map_or(0, Vec::len);
    let mut result = vec![Vec::with_capacity(row_count); col_count];

    for row in rows.iter() {
        if row.len() != col_count {
            return Err("linalg.transpose expects rectangular matrix".to_string());
        }
        for (col, value) in row.iter().enumerate() {
            result[col].push(value.clone());
        }
    }

    Ok(result)
}

fn numeric_matrix(rows: &[Vec<Value>], name: &str) -> Result<Vec<Vec<f64>>, String> {
    let col_count = rows.first().map_or(0, Vec::len);
    let mut values = Vec::with_capacity(rows.len());
    for row in rows {
        if row.len() != col_count {
            return Err(format!("linalg.{} expects rectangular matrix", name));
        }
        values.push(row.iter().map(Value::to_float).collect::<Result<Vec<_>, _>>()?);
    }
    Ok(values)
}

fn invert_matrix(mut matrix: Vec<Vec<f64>>) -> Result<Vec<Vec<f64>>, String> {
    let size = matrix.len();
    if size == 0 || matrix.iter().any(|row| row.len() != size) {
        return Err("linalg.inv expects square matrix".to_string());
    }

    let mut inverse = identity(size);
    for pivot_col in 0..size {
        let mut pivot_row = pivot_col;
        for row in pivot_col + 1..size {
            if matrix[row][pivot_col].abs() > matrix[pivot_row][pivot_col].abs() {
                pivot_row = row;
            }
        }

        if matrix[pivot_row][pivot_col].abs() < EPSILON {
            return Err("linalg.inv expects non-singular matrix".to_string());
        }

        matrix.swap(pivot_col, pivot_row);
        inverse.swap(pivot_col, pivot_row);

        let pivot = matrix[pivot_col][pivot_col];
        for col in 0..size {
            matrix[pivot_col][col] /= pivot;
            inverse[pivot_col][col] /= pivot;
        }

        for row in 0..size {
            if row == pivot_col {
                continue;
            }
            let factor = matrix[row][pivot_col];
            for col in 0..size {
                matrix[row][col] -= factor * matrix[pivot_col][col];
                inverse[row][col] -= factor * inverse[pivot_col][col];
            }
        }
    }

    Ok(inverse)
}

fn identity(size: usize) -> Vec<Vec<f64>> {
    let mut matrix = vec![vec![0.0; size]; size];
    for (index, row) in matrix.iter_mut().enumerate() {
        row[index] = 1.0;
    }
    matrix
}

fn matrix_rank(values: &mut [Vec<f64>]) -> usize {
    if values.is_empty() {
        return 0;
    }

    let row_count = values.len();
    let col_count = values[0].len();
    let mut rank = 0;
    for col in 0..col_count {
        let pivot = (rank..row_count).find(|row| values[*row][col].abs() >= EPSILON);
        let Some(pivot) = pivot else {
            continue;
        };

        values.swap(rank, pivot);
        let pivot_value = values[rank][col];
        for c in col..col_count {
            values[rank][c] /= pivot_value;
        }
        for row in 0..row_count {
            if row == rank {
                continue;
            }
            let factor = values[row][col];
            for c in col..col_count {
                values[row][c] -= factor * values[rank][c];
            }
        }
        rank += 1;
    }
    rank
}

fn multiply_matrices(left: &[Vec<f64>], right: &[Vec<f64>]) -> Result<Vec<Vec<f64>>, String> {
    if left.is_empty() || right.is_empty() || left[0].len() != right.len() {
        return Err("linalg matrix dimensions do not align".to_string());
    }

    let rows = left.len();
    let inner = right.len();
    let cols = right[0].len();
    let mut result = vec![vec![0.0; cols]; rows];
    for row in 0..rows {
        for col in 0..cols {
            result[row][col] = (0..inner).map(|index| left[row][index] * right[index][col]).sum();
        }
    }
    Ok(result)
}

fn is_diagonal(values: &[Vec<f64>]) -> bool {
    values.iter().enumerate().all(|(row_index, row)| {
        row.iter()
            .enumerate()
            .all(|(col_index, value)| row_index == col_index || value.abs() < EPSILON)
    })
}

fn float_matrix_value(rows: Vec<Vec<f64>>) -> Value {
    let matrix = rows
        .into_iter()
        .map(|row| {
            row.into_iter()
                .map(|value| {
                    if value.fract() == 0.0 {
                        Value::Int(value as i64)
                    } else {
                        Value::Float(value)
                    }
                })
                .collect()
        })
        .collect();
    Value::Matrix(Rc::new(RefCell::new(matrix)))
}
