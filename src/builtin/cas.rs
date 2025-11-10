// CAS (Computer Algebra System) 模块 - 使用 MathCore 实现
use crate::value::Value;
use mathcore::MathCore;
use std::collections::HashMap;

// 全局CAS存储
use std::sync::Mutex;

lazy_static::lazy_static! {
    static ref CAS_STORAGE: Mutex<HashMap<String, String>> = Mutex::new(HashMap::new());
}

// CAS内置函数接口
pub fn cas_parse(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("cas_parse expects 1 argument".to_string());
    }

    match &args[0] {
        Value::String(s) => {
            // 使用 MathCore 解析表达式
            match MathCore::parse(s) {
                Ok(expr) => Ok(Value::String(format!("{:?}", expr))),
                Err(e) => Err(format!("Parse error: {}", e)),
            }
        }
        _ => Err("cas_parse expects string".to_string()),
    }
}

pub fn cas_differentiate(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("cas_differentiate expects 2 arguments (expr, var)".to_string());
    }

    let expr_str = if let Value::String(s) = &args[0] {
        s.clone()
    } else {
        return Err("cas_differentiate expects string expression".to_string());
    };

    let var = if let Value::String(v) = &args[1] {
        v.clone()
    } else {
        return Err("cas_differentiate expects string variable".to_string());
    };

    // 使用 MathCore 求导
    match MathCore::differentiate(&expr_str, &var) {
        Ok(derivative) => Ok(Value::String(format!("{}", derivative))),
        Err(e) => Err(format!("Differentiation error: {}", e)),
    }
}

pub fn cas_solve_linear(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("cas_solve_linear expects 2 arguments (expr, var)".to_string());
    }

    let expr_str = if let Value::String(s) = &args[0] {
        s.clone()
    } else {
        return Err("cas_solve_linear expects string expression".to_string());
    };

    let var = if let Value::String(v) = &args[1] {
        v.clone()
    } else {
        return Err("cas_solve_linear expects string variable".to_string());
    };

    // 使用 MathCore 求解方程
    match MathCore::solve(&expr_str, &var) {
        Ok(roots) => {
            // 返回第一个根（对于线性方程通常只有一个根）
            if !roots.is_empty() {
                if let mathcore::Expr::Number(n) = &roots[0] {
                    Ok(Value::Float(*n))
                } else {
                    Err("Solution is not a number".to_string())
                }
            } else {
                Err("Cannot solve equation".to_string())
            }
        }
        Err(e) => Err(format!("Solve error: {}", e)),
    }
}

pub fn cas_evaluate_at(args: &[Value]) -> Result<Value, String> {
    if args.len() != 3 {
        return Err("cas_evaluate_at expects 3 arguments (expr, var, value)".to_string());
    }

    let expr_str = if let Value::String(s) = &args[0] {
        s.clone()
    } else {
        return Err("cas_evaluate_at expects string expression".to_string());
    };

    let var = if let Value::String(v) = &args[1] {
        v.clone()
    } else {
        return Err("cas_evaluate_at expects string variable".to_string());
    };

    let value = args[2].to_float()?;

    // 使用 MathCore 解析并求值
    let math = MathCore::new();
    let mut vars = std::collections::HashMap::new();
    vars.insert(var, value);

    match math.evaluate_with_vars(&expr_str, &vars) {
        Ok(result) => Ok(Value::Float(result)),
        Err(e) => Err(format!("Evaluation error: {}", e)),
    }
}

pub fn cas_store(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("cas_store expects 2 arguments (name, expr)".to_string());
    }

    let name = if let Value::String(n) = &args[0] {
        n.clone()
    } else {
        return Err("cas_store expects string name".to_string());
    };

    let expr = if let Value::String(s) = &args[1] {
        s.clone()
    } else {
        return Err("cas_store expects string expression".to_string());
    };

    // 验证表达式是否可以解析
    MathCore::parse(&expr).map_err(|e| format!("Invalid expression: {}", e))?;

    CAS_STORAGE.lock().unwrap().insert(name, expr);
    Ok(Value::Null)
}

pub fn cas_load(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("cas_load expects 1 argument (name)".to_string());
    }

    let name = if let Value::String(n) = &args[0] {
        n.clone()
    } else {
        return Err("cas_load expects string name".to_string());
    };

    if let Some(expr) = CAS_STORAGE.lock().unwrap().get(&name) {
        Ok(Value::String(expr.clone()))
    } else {
        Err(format!("Expression '{}' not found in storage", name))
    }
}

pub fn cas_numerical_derivative(args: &[Value]) -> Result<Value, String> {
    if args.len() != 3 {
        return Err("cas_numerical_derivative expects 3 arguments (expr, var, point)".to_string());
    }

    let expr_str = if let Value::String(s) = &args[0] {
        s.clone()
    } else {
        return Err("cas_numerical_derivative expects string expression".to_string());
    };

    let var = if let Value::String(v) = &args[1] {
        v.clone()
    } else {
        return Err("cas_numerical_derivative expects string variable".to_string());
    };

    let point = args[2].to_float()?;

    let math = MathCore::new();
    let h = 1e-8;

    let mut vars_plus = std::collections::HashMap::new();
    vars_plus.insert(var.clone(), point + h);

    let mut vars_minus = std::collections::HashMap::new();
    vars_minus.insert(var.clone(), point - h);

    let f_plus = math
        .evaluate_with_vars(&expr_str, &vars_plus)
        .map_err(|e| format!("Evaluation error: {}", e))?;
    let f_minus = math
        .evaluate_with_vars(&expr_str, &vars_minus)
        .map_err(|e| format!("Evaluation error: {}", e))?;

    let derivative = (f_plus - f_minus) / (2.0 * h);

    Ok(Value::Float(derivative))
}

pub fn cas_integrate(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("cas_integrate expects 2 arguments (expr, var)".to_string());
    }

    let expr_str = if let Value::String(s) = &args[0] {
        s.clone()
    } else {
        return Err("cas_integrate expects string expression".to_string());
    };

    let var = if let Value::String(v) = &args[1] {
        v.clone()
    } else {
        return Err("cas_integrate expects string variable".to_string());
    };

    // 使用 MathCore 积分
    match MathCore::integrate(&expr_str, &var) {
        Ok(integral) => Ok(Value::String(format!("{}", integral))),
        Err(e) => Err(format!("Integration error: {}", e)),
    }
}

pub fn cas_definite_integral(args: &[Value]) -> Result<Value, String> {
    if args.len() != 4 {
        return Err(
            "cas_definite_integral expects 4 arguments (expr, var, lower, upper)".to_string(),
        );
    }

    let expr_str = if let Value::String(s) = &args[0] {
        s.clone()
    } else {
        return Err("cas_definite_integral expects string expression".to_string());
    };

    let var = if let Value::String(v) = &args[1] {
        v.clone()
    } else {
        return Err("cas_definite_integral expects string variable".to_string());
    };

    let lower = args[2].to_float()?;
    let upper = args[3].to_float()?;

    // 使用 MathCore 数值积分
    match MathCore::numerical_integrate(&expr_str, &var, lower, upper) {
        Ok(result) => Ok(Value::Float(result)),
        Err(e) => Err(format!("Integration error: {}", e)),
    }
}
