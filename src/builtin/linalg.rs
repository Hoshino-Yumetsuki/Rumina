use std::cell::RefCell;
use std::rc::Rc;

use crate::value::Value;

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

    Ok(Value::Matrix(Rc::new(RefCell::new(result))))
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
