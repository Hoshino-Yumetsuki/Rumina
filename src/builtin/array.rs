// 数组函数模块
use crate::value::Value;
use std::cell::RefCell;
use std::rc::Rc;

pub fn foreach(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("foreach expects 2 arguments (array, function)".to_string());
    }

    match (&args[0], &args[1]) {
        (Value::Array(_arr), _func) => {
            // foreach需要在interpreter中实现，因为需要调用回调函数
            Err("foreach not yet fully implemented - use in interpreter".to_string())
        }
        _ => Err(format!(
            "foreach expects array and function, got {} and {}",
            args[0].type_name(),
            args[1].type_name()
        )),
    }
}

pub fn map(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("map expects 2 arguments (array, function)".to_string());
    }

    Err("map not yet implemented".to_string())
}

pub fn push(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("push expects 2 arguments (array, value)".to_string());
    }

    match &args[0] {
        Value::Array(arr) => {
            arr.borrow_mut().push(args[1].clone());
            Ok(Value::Null)
        }
        _ => Err(format!("push expects array, got {}", args[0].type_name())),
    }
}

pub fn pop(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("pop expects 1 argument (array)".to_string());
    }

    match &args[0] {
        Value::Array(arr) => arr
            .borrow_mut()
            .pop()
            .ok_or_else(|| "Cannot pop from empty array".to_string()),
        _ => Err(format!("pop expects array, got {}", args[0].type_name())),
    }
}

pub fn range(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("range expects 1 argument (length)".to_string());
    }

    match &args[0] {
        Value::Int(n) => {
            if *n < 0 {
                return Err("range expects non-negative integer".to_string());
            }
            let arr: Vec<Value> = (0..*n).map(|i| Value::Int(i)).collect();
            Ok(Value::Array(Rc::new(RefCell::new(arr))))
        }
        _ => Err(format!(
            "range expects integer, got {}",
            args[0].type_name()
        )),
    }
}

pub fn concat(args: &[Value]) -> Result<Value, String> {
    if args.is_empty() {
        return Ok(Value::Array(Rc::new(RefCell::new(Vec::new()))));
    }

    let mut result = Vec::new();

    for arg in args {
        match arg {
            Value::Array(arr) => {
                result.extend(arr.borrow().iter().cloned());
            }
            _ => {
                return Err(format!(
                    "concat expects only arrays, got {}",
                    arg.type_name()
                ));
            }
        }
    }

    Ok(Value::Array(Rc::new(RefCell::new(result))))
}

pub fn dot(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("dot expects 2 arguments (vector1, vector2)".to_string());
    }

    match (&args[0], &args[1]) {
        (Value::Array(v1), Value::Array(v2)) => {
            let v1 = v1.borrow();
            let v2 = v2.borrow();

            if v1.len() != v2.len() {
                return Err("Vectors must have same length".to_string());
            }

            let mut result = 0.0;
            for (a, b) in v1.iter().zip(v2.iter()) {
                let a = a.to_float()?;
                let b = b.to_float()?;
                result += a * b;
            }

            Ok(Value::Float(result))
        }
        _ => Err("dot expects two arrays".to_string()),
    }
}

pub fn norm(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("norm expects 1 argument (vector)".to_string());
    }

    match &args[0] {
        Value::Array(v) => {
            let v = v.borrow();
            let mut sum = 0.0;
            for val in v.iter() {
                let f = val.to_float()?;
                sum += f * f;
            }
            Ok(Value::Float(sum.sqrt()))
        }
        _ => Err("norm expects array".to_string()),
    }
}

pub fn cross(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("cross expects 2 arguments (vector1, vector2)".to_string());
    }

    match (&args[0], &args[1]) {
        (Value::Array(v1), Value::Array(v2)) => {
            let v1 = v1.borrow();
            let v2 = v2.borrow();

            if v1.len() != 3 || v2.len() != 3 {
                return Err("cross expects 3D vectors".to_string());
            }

            let x1 = v1[0].to_float()?;
            let y1 = v1[1].to_float()?;
            let z1 = v1[2].to_float()?;

            let x2 = v2[0].to_float()?;
            let y2 = v2[1].to_float()?;
            let z2 = v2[2].to_float()?;

            // 叉积公式: (a×b) = (a_y*b_z - a_z*b_y, a_z*b_x - a_x*b_z, a_x*b_y - a_y*b_x)
            let result = vec![
                Value::Float(y1 * z2 - z1 * y2),
                Value::Float(z1 * x2 - x1 * z2),
                Value::Float(x1 * y2 - y1 * x2),
            ];

            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        _ => Err("cross expects two arrays".to_string()),
    }
}

// Lamina-compliant: det() - matrix determinant
pub fn det(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("det expects 1 argument (matrix)".to_string());
    }

    match &args[0] {
        Value::Array(matrix) => {
            let matrix = matrix.borrow();

            // Convert to 2D float matrix
            let n = matrix.len();
            if n == 0 {
                return Err("Cannot compute determinant of empty matrix".to_string());
            }

            let mut mat: Vec<Vec<f64>> = Vec::new();
            for row in matrix.iter() {
                match row {
                    Value::Array(row_arr) => {
                        let row_arr = row_arr.borrow();
                        if row_arr.len() != n {
                            return Err("Matrix must be square".to_string());
                        }
                        let mut float_row = Vec::new();
                        for val in row_arr.iter() {
                            float_row.push(val.to_float()?);
                        }
                        mat.push(float_row);
                    }
                    _ => return Err("det expects a matrix (2D array)".to_string()),
                }
            }

            // Calculate determinant using recursive Laplace expansion
            let result = calculate_determinant(&mat);
            Ok(Value::Float(result))
        }
        _ => Err(format!("det expects array, got {}", args[0].type_name())),
    }
}

fn calculate_determinant(matrix: &Vec<Vec<f64>>) -> f64 {
    let n = matrix.len();

    if n == 1 {
        return matrix[0][0];
    }

    if n == 2 {
        return matrix[0][0] * matrix[1][1] - matrix[0][1] * matrix[1][0];
    }

    let mut det = 0.0;
    for col in 0..n {
        // Create submatrix
        let mut submatrix = Vec::new();
        for row in 1..n {
            let mut subrow = Vec::new();
            for c in 0..n {
                if c != col {
                    subrow.push(matrix[row][c]);
                }
            }
            submatrix.push(subrow);
        }

        let sign = if col % 2 == 0 { 1.0 } else { -1.0 };
        det += sign * matrix[0][col] * calculate_determinant(&submatrix);
    }

    det
}
