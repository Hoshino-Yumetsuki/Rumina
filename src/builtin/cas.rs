// CAS (Computer Algebra System) 模块
use crate::value::Value;
use std::collections::HashMap;

// 符号表达式
#[derive(Debug, Clone, PartialEq)]
pub enum SymExpr {
    Constant(f64),
    Variable(String),
    Add(Box<SymExpr>, Box<SymExpr>),
    Sub(Box<SymExpr>, Box<SymExpr>),
    Mul(Box<SymExpr>, Box<SymExpr>),
    Div(Box<SymExpr>, Box<SymExpr>),
    Pow(Box<SymExpr>, Box<SymExpr>),
    Neg(Box<SymExpr>),
}

impl std::fmt::Display for SymExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SymExpr::Constant(n) => {
                if n.fract() == 0.0 {
                    write!(f, "{}", *n as i64)
                } else {
                    write!(f, "{}", n)
                }
            }
            SymExpr::Variable(s) => write!(f, "{}", s),
            SymExpr::Add(a, b) => write!(f, "({} + {})", a, b),
            SymExpr::Sub(a, b) => write!(f, "({} - {})", a, b),
            SymExpr::Mul(a, b) => {
                // 简化显示：系数在前
                match (&**a, &**b) {
                    (SymExpr::Constant(n), expr) => {
                        if *n == 1.0 {
                            write!(f, "{}", expr)
                        } else {
                            write!(f, "{}*{}", n, expr)
                        }
                    }
                    _ => write!(f, "{}*{}", a, b),
                }
            }
            SymExpr::Div(a, b) => write!(f, "{}/{}", a, b),
            SymExpr::Pow(a, b) => write!(f, "{}^{}", a, b),
            SymExpr::Neg(e) => write!(f, "-({})", e),
        }
    }
}

impl SymExpr {
    // 化简表达式
    pub fn simplify(&self) -> SymExpr {
        match self {
            SymExpr::Add(a, b) => {
                let a = a.simplify();
                let b = b.simplify();
                match (&a, &b) {
                    (SymExpr::Constant(0.0), _) => b,
                    (_, SymExpr::Constant(0.0)) => a,
                    (SymExpr::Constant(x), SymExpr::Constant(y)) => SymExpr::Constant(x + y),
                    _ => SymExpr::Add(Box::new(a), Box::new(b)),
                }
            }
            SymExpr::Sub(a, b) => {
                let a = a.simplify();
                let b = b.simplify();
                match (&a, &b) {
                    (_, SymExpr::Constant(0.0)) => a,
                    (SymExpr::Constant(x), SymExpr::Constant(y)) => SymExpr::Constant(x - y),
                    _ => SymExpr::Sub(Box::new(a), Box::new(b)),
                }
            }
            SymExpr::Mul(a, b) => {
                let a = a.simplify();
                let b = b.simplify();
                match (&a, &b) {
                    (SymExpr::Constant(0.0), _) | (_, SymExpr::Constant(0.0)) => {
                        SymExpr::Constant(0.0)
                    }
                    (SymExpr::Constant(1.0), _) => b,
                    (_, SymExpr::Constant(1.0)) => a,
                    (SymExpr::Constant(x), SymExpr::Constant(y)) => SymExpr::Constant(x * y),
                    _ => SymExpr::Mul(Box::new(a), Box::new(b)),
                }
            }
            SymExpr::Pow(a, b) => {
                let a = a.simplify();
                let b = b.simplify();
                match (&a, &b) {
                    (_, SymExpr::Constant(0.0)) => SymExpr::Constant(1.0),
                    (_, SymExpr::Constant(1.0)) => a,
                    (SymExpr::Constant(x), SymExpr::Constant(y)) => SymExpr::Constant(x.powf(*y)),
                    _ => SymExpr::Pow(Box::new(a), Box::new(b)),
                }
            }
            _ => self.clone(),
        }
    }

    // 求导
    pub fn differentiate(&self, var: &str) -> SymExpr {
        match self {
            SymExpr::Constant(_) => SymExpr::Constant(0.0),
            SymExpr::Variable(v) => {
                if v == var {
                    SymExpr::Constant(1.0)
                } else {
                    SymExpr::Constant(0.0)
                }
            }
            SymExpr::Add(a, b) => SymExpr::Add(
                Box::new(a.differentiate(var)),
                Box::new(b.differentiate(var)),
            )
            .simplify(),
            SymExpr::Sub(a, b) => SymExpr::Sub(
                Box::new(a.differentiate(var)),
                Box::new(b.differentiate(var)),
            )
            .simplify(),
            SymExpr::Mul(a, b) => {
                // 乘法法则: (uv)' = u'v + uv'
                SymExpr::Add(
                    Box::new(SymExpr::Mul(Box::new(a.differentiate(var)), b.clone())),
                    Box::new(SymExpr::Mul(a.clone(), Box::new(b.differentiate(var)))),
                )
                .simplify()
            }
            SymExpr::Pow(base, exp) => {
                // 幂法则: (x^n)' = n*x^(n-1)
                if let SymExpr::Constant(_) = **exp {
                    SymExpr::Mul(
                        exp.clone(),
                        Box::new(SymExpr::Mul(
                            Box::new(SymExpr::Pow(
                                base.clone(),
                                Box::new(SymExpr::Sub(
                                    exp.clone(),
                                    Box::new(SymExpr::Constant(1.0)),
                                )),
                            )),
                            Box::new(base.differentiate(var)),
                        )),
                    )
                    .simplify()
                } else {
                    // 一般情况暂不支持
                    SymExpr::Constant(0.0)
                }
            }
            SymExpr::Neg(e) => SymExpr::Neg(Box::new(e.differentiate(var))).simplify(),
            _ => SymExpr::Constant(0.0),
        }
    }

    // 在特定值处求值
    pub fn evaluate(&self, var: &str, value: f64) -> f64 {
        match self {
            SymExpr::Constant(n) => *n,
            SymExpr::Variable(v) => {
                if v == var {
                    value
                } else {
                    0.0
                }
            }
            SymExpr::Add(a, b) => a.evaluate(var, value) + b.evaluate(var, value),
            SymExpr::Sub(a, b) => a.evaluate(var, value) - b.evaluate(var, value),
            SymExpr::Mul(a, b) => a.evaluate(var, value) * b.evaluate(var, value),
            SymExpr::Div(a, b) => a.evaluate(var, value) / b.evaluate(var, value),
            SymExpr::Pow(a, b) => a.evaluate(var, value).powf(b.evaluate(var, value)),
            SymExpr::Neg(e) => -e.evaluate(var, value),
        }
    }

    // 符号积分（仅支持多项式）
    pub fn integrate(&self, var: &str) -> Result<SymExpr, String> {
        match self {
            SymExpr::Constant(c) => {
                // ∫c dx = c*x
                Ok(SymExpr::Mul(
                    Box::new(SymExpr::Constant(*c)),
                    Box::new(SymExpr::Variable(var.to_string())),
                ))
            }
            SymExpr::Variable(v) => {
                if v == var {
                    // ∫x dx = x²/2
                    Ok(SymExpr::Div(
                        Box::new(SymExpr::Pow(
                            Box::new(SymExpr::Variable(var.to_string())),
                            Box::new(SymExpr::Constant(2.0)),
                        )),
                        Box::new(SymExpr::Constant(2.0)),
                    ))
                } else {
                    // ∫y dx = y*x (y是常数)
                    Ok(SymExpr::Mul(
                        Box::new(SymExpr::Variable(v.clone())),
                        Box::new(SymExpr::Variable(var.to_string())),
                    ))
                }
            }
            SymExpr::Add(a, b) => {
                // ∫(f + g) dx = ∫f dx + ∫g dx
                let int_a = a.integrate(var)?;
                let int_b = b.integrate(var)?;
                Ok(SymExpr::Add(Box::new(int_a), Box::new(int_b)).simplify())
            }
            SymExpr::Sub(a, b) => {
                // ∫(f - g) dx = ∫f dx - ∫g dx
                let int_a = a.integrate(var)?;
                let int_b = b.integrate(var)?;
                Ok(SymExpr::Sub(Box::new(int_a), Box::new(int_b)).simplify())
            }
            SymExpr::Mul(a, b) => {
                // 检查是否是 常数*x^n 的形式
                match (&**a, &**b) {
                    (SymExpr::Constant(c), SymExpr::Pow(base, exp)) => {
                        if let (SymExpr::Variable(v), SymExpr::Constant(n)) = (&**base, &**exp) {
                            if v == var {
                                // ∫c*x^n dx = c*x^(n+1)/(n+1)
                                Ok(SymExpr::Div(
                                    Box::new(SymExpr::Mul(
                                        Box::new(SymExpr::Constant(*c)),
                                        Box::new(SymExpr::Pow(
                                            Box::new(SymExpr::Variable(var.to_string())),
                                            Box::new(SymExpr::Constant(n + 1.0)),
                                        )),
                                    )),
                                    Box::new(SymExpr::Constant(n + 1.0)),
                                )
                                .simplify())
                            } else {
                                Err(format!("Cannot integrate complex expression"))
                            }
                        } else {
                            Err(format!("Cannot integrate complex expression"))
                        }
                    }
                    (SymExpr::Constant(c), SymExpr::Variable(v)) => {
                        if v == var {
                            // ∫c*x dx = c*x²/2
                            Ok(SymExpr::Div(
                                Box::new(SymExpr::Mul(
                                    Box::new(SymExpr::Constant(*c)),
                                    Box::new(SymExpr::Pow(
                                        Box::new(SymExpr::Variable(var.to_string())),
                                        Box::new(SymExpr::Constant(2.0)),
                                    )),
                                )),
                                Box::new(SymExpr::Constant(2.0)),
                            )
                            .simplify())
                        } else {
                            Ok(SymExpr::Mul(
                                Box::new(SymExpr::Constant(*c)),
                                Box::new(SymExpr::Mul(
                                    Box::new(SymExpr::Variable(v.clone())),
                                    Box::new(SymExpr::Variable(var.to_string())),
                                )),
                            ))
                        }
                    }
                    (SymExpr::Pow(base, exp), SymExpr::Constant(c)) => {
                        if let (SymExpr::Variable(v), SymExpr::Constant(n)) = (&**base, &**exp) {
                            if v == var {
                                // ∫x^n*c dx = c*x^(n+1)/(n+1)
                                Ok(SymExpr::Div(
                                    Box::new(SymExpr::Mul(
                                        Box::new(SymExpr::Constant(*c)),
                                        Box::new(SymExpr::Pow(
                                            Box::new(SymExpr::Variable(var.to_string())),
                                            Box::new(SymExpr::Constant(n + 1.0)),
                                        )),
                                    )),
                                    Box::new(SymExpr::Constant(n + 1.0)),
                                )
                                .simplify())
                            } else {
                                Err(format!("Cannot integrate complex expression"))
                            }
                        } else {
                            Err(format!("Cannot integrate complex expression"))
                        }
                    }
                    _ => Err(format!("Cannot integrate complex multiplication")),
                }
            }
            SymExpr::Pow(base, exp) => {
                if let (SymExpr::Variable(v), SymExpr::Constant(n)) = (&**base, &**exp) {
                    if v == var {
                        // ∫x^n dx = x^(n+1)/(n+1)
                        Ok(SymExpr::Div(
                            Box::new(SymExpr::Pow(
                                Box::new(SymExpr::Variable(var.to_string())),
                                Box::new(SymExpr::Constant(n + 1.0)),
                            )),
                            Box::new(SymExpr::Constant(n + 1.0)),
                        )
                        .simplify())
                    } else {
                        Err(format!("Cannot integrate power with non-variable base"))
                    }
                } else {
                    Err(format!("Cannot integrate complex power"))
                }
            }
            SymExpr::Neg(e) => {
                // ∫-f dx = -∫f dx
                let int_e = e.integrate(var)?;
                Ok(SymExpr::Neg(Box::new(int_e)).simplify())
            }
            _ => Err(format!(
                "Integration not supported for this expression type"
            )),
        }
    }

    // 定积分
    pub fn definite_integral(&self, var: &str, lower: f64, upper: f64) -> Result<f64, String> {
        let antiderivative = self.integrate(var)?;
        let upper_val = antiderivative.evaluate(var, upper);
        let lower_val = antiderivative.evaluate(var, lower);
        Ok(upper_val - lower_val)
    }

    // 解一元一次方程 ax + b = 0
    pub fn solve_linear(&self, var: &str) -> Option<f64> {
        // 简化为 ax + b 的形式
        let simplified = self.simplify();

        // 尝试提取系数
        match &simplified {
            SymExpr::Add(a, b) => {
                // 形式: coef*x + constant
                if let (Some(coef), Some(constant)) =
                    (extract_coefficient(a, var), extract_constant(b))
                {
                    return Some(-constant / coef);
                }
                if let (Some(constant), Some(coef)) =
                    (extract_constant(a), extract_coefficient(b, var))
                {
                    return Some(-constant / coef);
                }
            }
            SymExpr::Sub(a, b) => {
                // 形式: coef*x - constant  =>  coef*x = constant  =>  x = constant/coef
                if let (Some(coef), Some(constant)) =
                    (extract_coefficient(a, var), extract_constant(b))
                {
                    return Some(constant / coef);
                }
                // 形式: constant - coef*x  =>  -coef*x = -constant  =>  x = constant/coef
                if let (Some(constant), Some(coef)) =
                    (extract_constant(a), extract_coefficient(b, var))
                {
                    return Some(constant / coef);
                }
            }
            SymExpr::Mul(_a, _b) => {
                // 形式: coef*x
                if let Some(coef) = extract_coefficient(&simplified, var) {
                    return Some(0.0 / coef);
                }
            }
            SymExpr::Variable(v) if v == var => return Some(0.0),
            _ => {}
        }

        None
    }
}

// 辅助函数：提取变量的系数
fn extract_coefficient(expr: &SymExpr, var: &str) -> Option<f64> {
    match expr {
        SymExpr::Variable(v) if v == var => Some(1.0),
        SymExpr::Mul(a, b) => {
            if let SymExpr::Constant(n) = **a {
                if let SymExpr::Variable(v) = &**b {
                    if v == var {
                        return Some(n);
                    }
                }
            }
            if let SymExpr::Constant(n) = **b {
                if let SymExpr::Variable(v) = &**a {
                    if v == var {
                        return Some(n);
                    }
                }
            }
            None
        }
        _ => None,
    }
}

// 辅助函数：提取常数项
fn extract_constant(expr: &SymExpr) -> Option<f64> {
    match expr {
        SymExpr::Constant(n) => Some(*n),
        _ => None,
    }
}

// 全局CAS存储
use std::sync::Mutex;

lazy_static::lazy_static! {
    static ref CAS_STORAGE: Mutex<HashMap<String, SymExpr>> = Mutex::new(HashMap::new());
}

// 解析简单表达式（仅支持基本形式）
pub fn parse_simple_expr(s: &str) -> Result<SymExpr, String> {
    let s = s.trim();

    // 去除外层括号
    let s = if s.starts_with('(') && s.ends_with(')') {
        // 检查是否是匹配的外层括号
        let mut depth = 0;
        let mut is_outer = true;
        for (i, c) in s.chars().enumerate() {
            if c == '(' {
                depth += 1;
            } else if c == ')' {
                depth -= 1;
                if depth == 0 && i < s.len() - 1 {
                    is_outer = false;
                    break;
                }
            }
        }
        if is_outer {
            &s[1..s.len() - 1]
        } else {
            s
        }
    } else {
        s
    };

    // 尝试解析为数字
    if let Ok(n) = s.parse::<f64>() {
        return Ok(SymExpr::Constant(n));
    }

    // 查找运算符（考虑括号深度）
    let mut depth = 0;
    let mut add_pos = None;
    let mut sub_pos = None;
    let mut mul_pos = None;
    let mut pow_pos = None;

    for (i, c) in s.chars().enumerate() {
        if c == '(' {
            depth += 1;
        } else if c == ')' {
            depth -= 1;
        } else if depth == 0 {
            if c == '+'
                && i > 0
                && s.as_bytes()[i - 1] == b' '
                && i < s.len() - 1
                && s.as_bytes()[i + 1] == b' '
            {
                add_pos = Some(i - 1);
            } else if c == '-'
                && i > 0
                && s.as_bytes()[i - 1] == b' '
                && i < s.len() - 1
                && s.as_bytes()[i + 1] == b' '
            {
                sub_pos = Some(i - 1);
            } else if c == '*' {
                mul_pos = Some(i);
            } else if c == '^' {
                pow_pos = Some(i);
            }
        }
    }

    // 按优先级处理运算符：+ - > * / > ^
    if let Some(pos) = add_pos {
        let left = parse_simple_expr(&s[..pos])?;
        let right = parse_simple_expr(&s[pos + 3..])?;
        return Ok(SymExpr::Add(Box::new(left), Box::new(right)));
    }

    if let Some(pos) = sub_pos {
        let left = parse_simple_expr(&s[..pos])?;
        let right = parse_simple_expr(&s[pos + 3..])?;
        return Ok(SymExpr::Sub(Box::new(left), Box::new(right)));
    }

    if let Some(pos) = mul_pos {
        let left = parse_simple_expr(&s[..pos])?;
        let right = parse_simple_expr(&s[pos + 1..])?;
        return Ok(SymExpr::Mul(Box::new(left), Box::new(right)));
    }

    if let Some(pos) = pow_pos {
        let left = parse_simple_expr(&s[..pos])?;
        let right = parse_simple_expr(&s[pos + 1..])?;
        return Ok(SymExpr::Pow(Box::new(left), Box::new(right)));
    }

    // 单个变量
    if s.chars().all(|c| c.is_alphabetic() || c == '_') {
        return Ok(SymExpr::Variable(s.to_string()));
    }

    Err(format!("Cannot parse expression: {}", s))
}

// CAS内置函数接口
pub fn cas_parse(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("cas_parse expects 1 argument".to_string());
    }

    match &args[0] {
        Value::String(s) => {
            let expr = parse_simple_expr(s)?;
            Ok(Value::String(format!("{}", expr)))
        }
        _ => Err("cas_parse expects string".to_string()),
    }
}

pub fn cas_differentiate(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("cas_differentiate expects 2 arguments (expr, var)".to_string());
    }

    let expr = if let Value::String(s) = &args[0] {
        parse_simple_expr(s)?
    } else {
        return Err("cas_differentiate expects string expression".to_string());
    };

    let var = if let Value::String(v) = &args[1] {
        v.clone()
    } else {
        return Err("cas_differentiate expects string variable".to_string());
    };

    let derivative = expr.differentiate(&var);
    Ok(Value::String(format!("{}", derivative)))
}

pub fn cas_solve_linear(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("cas_solve_linear expects 2 arguments (expr, var)".to_string());
    }

    let expr = if let Value::String(s) = &args[0] {
        parse_simple_expr(s)?
    } else {
        return Err("cas_solve_linear expects string expression".to_string());
    };

    let var = if let Value::String(v) = &args[1] {
        v.clone()
    } else {
        return Err("cas_solve_linear expects string variable".to_string());
    };

    if let Some(solution) = expr.solve_linear(&var) {
        Ok(Value::Float(solution))
    } else {
        Err("Cannot solve equation".to_string())
    }
}

pub fn cas_evaluate_at(args: &[Value]) -> Result<Value, String> {
    if args.len() != 3 {
        return Err("cas_evaluate_at expects 3 arguments (expr, var, value)".to_string());
    }

    let expr = if let Value::String(s) = &args[0] {
        parse_simple_expr(s)?
    } else {
        return Err("cas_evaluate_at expects string expression".to_string());
    };

    let var = if let Value::String(v) = &args[1] {
        v.clone()
    } else {
        return Err("cas_evaluate_at expects string variable".to_string());
    };

    let value = args[2].to_float()?;

    let result = expr.evaluate(&var, value);
    Ok(Value::Float(result))
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
        parse_simple_expr(s)?
    } else {
        return Err("cas_store expects string expression".to_string());
    };

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
        Ok(Value::String(format!("{}", expr)))
    } else {
        Err(format!("Expression '{}' not found in storage", name))
    }
}

pub fn cas_numerical_derivative(args: &[Value]) -> Result<Value, String> {
    if args.len() != 3 {
        return Err("cas_numerical_derivative expects 3 arguments (expr, var, point)".to_string());
    }

    let expr = if let Value::String(s) = &args[0] {
        parse_simple_expr(s)?
    } else {
        return Err("cas_numerical_derivative expects string expression".to_string());
    };

    let var = if let Value::String(v) = &args[1] {
        v.clone()
    } else {
        return Err("cas_numerical_derivative expects string variable".to_string());
    };

    let point = args[2].to_float()?;

    let h = 1e-8;
    let f_plus = expr.evaluate(&var, point + h);
    let f_minus = expr.evaluate(&var, point - h);
    let derivative = (f_plus - f_minus) / (2.0 * h);

    Ok(Value::Float(derivative))
}

pub fn cas_integrate(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("cas_integrate expects 2 arguments (expr, var)".to_string());
    }

    let expr = if let Value::String(s) = &args[0] {
        parse_simple_expr(s)?
    } else {
        return Err("cas_integrate expects string expression".to_string());
    };

    let var = if let Value::String(v) = &args[1] {
        v.clone()
    } else {
        return Err("cas_integrate expects string variable".to_string());
    };

    let integral = expr.integrate(&var)?;
    Ok(Value::String(format!("{}", integral)))
}

pub fn cas_definite_integral(args: &[Value]) -> Result<Value, String> {
    if args.len() != 4 {
        return Err(
            "cas_definite_integral expects 4 arguments (expr, var, lower, upper)".to_string(),
        );
    }

    let expr = if let Value::String(s) = &args[0] {
        parse_simple_expr(s)?
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

    let result = expr.definite_integral(&var, lower, upper)?;
    Ok(Value::Float(result))
}
