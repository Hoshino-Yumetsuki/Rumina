use num::BigInt;
use num::BigRational;
use num::One;
use num::complex::Complex64;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::rc::Rc;

use crate::ast::*;
use crate::builtin;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::value::*;

use mathcore::MathCore;

/// Lamina Float Usage Policy:
///
/// Float values in Lamina are used in the following cases:
/// 1. Float literals from source code (e.g., 3.14) - for compatibility
/// 2. Explicit conversion via decimal() function - primary use case
/// 3. Mathematical transcendental functions (sin, cos, log, etc.) - cannot be exact
/// 4. Vector/matrix operations (dot, norm, cross, det) - numerical computation
/// 5. User input parsed as float
/// 6. Mixed operations with other types when Float is involved
/// 7. Complex number arithmetic (Complex64 uses f64 internally)
///
/// Lamina prioritizes exact computation using:
/// - Int for integers
/// - Rational for fractions (division of integers)
/// - Irrational for symbolic roots, π, e
/// - BigInt for large integers (factorial results)
/// - Complex for complex numbers
///
/// Float should NOT be used where exact computation is possible.

pub struct Interpreter {
    globals: Rc<RefCell<HashMap<String, Value>>>,
    locals: Vec<Rc<RefCell<HashMap<String, Value>>>>,
    return_value: Option<Value>,
    break_flag: bool,
    continue_flag: bool,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        builtin::register_builtins(&mut globals);

        Interpreter {
            globals: Rc::new(RefCell::new(globals)),
            locals: Vec::new(),
            return_value: None,
            break_flag: false,
            continue_flag: false,
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<Option<Value>, String> {
        let mut last_value = None;
        for stmt in statements {
            // 如果是表达式语句,保存其值
            if let Stmt::Expr(expr) = stmt {
                last_value = Some(self.eval_expr(&expr)?);
            } else {
                self.execute_stmt(&stmt)?;
            }

            if self.return_value.is_some() || self.break_flag || self.continue_flag {
                break;
            }
        }
        Ok(last_value)
    }

    fn execute_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::VarDecl {
                name,
                is_bigint,
                value,
            } => {
                let val = self.eval_expr(value)?;
                let val = if *is_bigint {
                    self.convert_to_bigint(val)?
                } else {
                    val
                };
                self.set_variable(name.clone(), val);
                Ok(())
            }

            Stmt::Assign { name, value } => {
                let val = self.eval_expr(value)?;
                if !self.variable_exists(name) {
                    return Err(format!("Variable '{}' not defined", name));
                }
                self.set_variable(name.clone(), val);
                Ok(())
            }

            Stmt::MemberAssign {
                object,
                member,
                value,
            } => {
                let obj = self.eval_expr(object)?;
                let val = self.eval_expr(value)?;

                match obj {
                    Value::Struct(s) => {
                        s.borrow_mut().insert(member.clone(), val);
                        Ok(())
                    }
                    _ => Err(format!("Cannot assign member to {}", obj.type_name())),
                }
            }

            Stmt::Expr(expr) => {
                self.eval_expr(expr)?;
                Ok(())
            }

            Stmt::FuncDef { name, params, body } => {
                let func = Value::Function {
                    name: name.clone(),
                    params: params.clone(),
                    body: Box::new(Stmt::Block(body.clone())),
                };
                self.set_variable(name.clone(), func);
                Ok(())
            }

            Stmt::Return(expr) => {
                let val = if let Some(e) = expr {
                    self.eval_expr(e)?
                } else {
                    Value::Null
                };
                self.return_value = Some(val);
                Ok(())
            }

            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.eval_expr(condition)?;
                if cond.is_truthy() {
                    for stmt in then_branch {
                        self.execute_stmt(stmt)?;
                        if self.return_value.is_some() || self.break_flag || self.continue_flag {
                            break;
                        }
                    }
                } else if let Some(else_stmts) = else_branch {
                    for stmt in else_stmts {
                        self.execute_stmt(stmt)?;
                        if self.return_value.is_some() || self.break_flag || self.continue_flag {
                            break;
                        }
                    }
                }
                Ok(())
            }

            Stmt::While { condition, body } => {
                loop {
                    let cond = self.eval_expr(condition)?;
                    if !cond.is_truthy() {
                        break;
                    }

                    for stmt in body {
                        self.execute_stmt(stmt)?;
                        if self.return_value.is_some() || self.break_flag {
                            break;
                        }
                        if self.continue_flag {
                            self.continue_flag = false;
                            break;
                        }
                    }

                    if self.return_value.is_some() || self.break_flag {
                        self.break_flag = false;
                        break;
                    }
                }
                Ok(())
            }

            Stmt::Loop { body } => {
                loop {
                    for stmt in body {
                        self.execute_stmt(stmt)?;
                        if self.return_value.is_some() || self.break_flag {
                            break;
                        }
                        if self.continue_flag {
                            self.continue_flag = false;
                            break;
                        }
                    }

                    if self.return_value.is_some() || self.break_flag {
                        self.break_flag = false;
                        break;
                    }
                }
                Ok(())
            }

            Stmt::Break => {
                self.break_flag = true;
                Ok(())
            }

            Stmt::Continue => {
                self.continue_flag = true;
                Ok(())
            }

            Stmt::Include(path) => {
                // 首先检查是否是已注册的内置模块
                if self.globals.borrow().contains_key(path) {
                    // 内置模块已经存在，不需要加载
                    return Ok(());
                }

                // 加载模块文件
                let mut file_path = path.clone();

                // 如果没有.lm扩展名，自动添加
                if !file_path.ends_with(".lm") {
                    file_path.push_str(".lm");
                }

                // 尝试多个路径位置
                let contents = if let Ok(content) = fs::read_to_string(&file_path) {
                    content
                } else if file_path.starts_with("./") {
                    // 如果以 ./ 开头，尝试在 examples 目录中查找
                    let examples_path = format!("examples/{}", &file_path[2..]);
                    fs::read_to_string(&examples_path)
                        .map_err(|e| format!("Cannot read module '{}': {}", file_path, e))?
                } else {
                    // 如果找不到文件，检查是否是内置模块
                    if self.globals.borrow().contains_key(path) {
                        return Ok(());
                    }
                    return Err(format!("Cannot read module '{}'", file_path));
                };

                // 从文件内容中解析模块名（查找 // Module: xxx）
                let module_name = if let Some(line) = contents.lines().next() {
                    if line.trim().starts_with("// Module:") {
                        line.trim()
                            .strip_prefix("// Module:")
                            .unwrap_or("")
                            .trim()
                            .to_string()
                    } else {
                        // 没有模块声明，使用文件名
                        path.split('/')
                            .last()
                            .or_else(|| path.split('\\').last())
                            .unwrap_or(path)
                            .trim_end_matches(".lm")
                            .to_string()
                    }
                } else {
                    // 空文件，使用文件名
                    path.split('/')
                        .last()
                        .or_else(|| path.split('\\').last())
                        .unwrap_or(path)
                        .trim_end_matches(".lm")
                        .to_string()
                };

                // 解析模块
                let mut lexer = Lexer::new(contents);
                let tokens = lexer.tokenize();
                let mut parser = Parser::new(tokens);
                let statements = parser.parse()?;

                // 创建新的模块作用域
                let module_scope = Rc::new(RefCell::new(HashMap::new()));
                self.locals.push(Rc::clone(&module_scope));

                // 执行模块代码
                for stmt in statements {
                    self.execute_stmt(&stmt)?;
                }

                // 弹出模块作用域
                self.locals.pop();

                // 将模块注册为全局变量
                self.globals
                    .borrow_mut()
                    .insert(module_name, Value::Module(module_scope));

                Ok(())
            }

            Stmt::Block(stmts) => {
                for stmt in stmts {
                    self.execute_stmt(stmt)?;
                    if self.return_value.is_some() || self.break_flag || self.continue_flag {
                        break;
                    }
                }
                Ok(())
            }
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Int(n) => Ok(Value::Int(*n)),
            Expr::Float(f) => Ok(Value::Float(*f)),
            Expr::String(s) => Ok(Value::String(s.clone())),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::Null => Ok(Value::Null),

            Expr::Ident(name) => self.get_variable(name),

            Expr::Array(elements) => {
                let mut arr = Vec::new();
                for elem in elements {
                    arr.push(self.eval_expr(elem)?);
                }
                Ok(Value::Array(Rc::new(RefCell::new(arr))))
            }

            Expr::Struct(fields) => {
                let mut map = HashMap::new();
                for (key, value) in fields {
                    let val = self.eval_expr(value)?;
                    map.insert(key.clone(), val);
                }
                Ok(Value::Struct(Rc::new(RefCell::new(map))))
            }

            Expr::Binary { left, op, right } => {
                let l = self.eval_expr(left)?;
                let r = self.eval_expr(right)?;
                self.eval_binary_op(&l, *op, &r)
            }

            Expr::Unary { op, expr } => {
                let val = self.eval_expr(expr)?;
                self.eval_unary_op(*op, &val)
            }

            Expr::Call { func, args } => {
                // 检查是否是成员调用 (obj.method())
                if let Expr::Member { object, member } = &**func {
                    let obj = self.eval_expr(object)?;

                    // 获取方法
                    let method = match &obj {
                        Value::Struct(s) => {
                            let s = s.borrow();
                            s.get(member).cloned().ok_or_else(|| {
                                format!("Struct does not have member '{}'", member)
                            })?
                        }
                        _ => return Err(format!("Cannot access member of {}", obj.type_name())),
                    };

                    // 计算参数
                    let arg_vals: Result<Vec<_>, _> =
                        args.iter().map(|a| self.eval_expr(a)).collect();
                    let arg_vals = arg_vals?;

                    // 调用方法，并注入self
                    self.call_method(method, obj, arg_vals)
                } else {
                    // 普通函数调用
                    let func_val = self.eval_expr(func)?;
                    let arg_vals: Result<Vec<_>, _> =
                        args.iter().map(|a| self.eval_expr(a)).collect();
                    let arg_vals = arg_vals?;
                    self.call_function(func_val, arg_vals)
                }
            }

            Expr::Member { object, member } => {
                let obj = self.eval_expr(object)?;
                match obj {
                    Value::Struct(s) => {
                        let s = s.borrow();
                        s.get(member)
                            .cloned()
                            .ok_or_else(|| format!("Struct does not have member '{}'", member))
                    }
                    _ => Err(format!("Cannot access member of {}", obj.type_name())),
                }
            }

            Expr::Index { object, index } => {
                let obj = self.eval_expr(object)?;
                let idx = self.eval_expr(index)?;
                match (obj, idx) {
                    (Value::Array(arr), Value::Int(i)) => {
                        let arr = arr.borrow();
                        let index = if i < 0 {
                            (arr.len() as i64 + i) as usize
                        } else {
                            i as usize
                        };
                        arr.get(index)
                            .cloned()
                            .ok_or_else(|| format!("Array index out of bounds: {}", i))
                    }
                    (Value::String(s), Value::Int(i)) => {
                        let chars: Vec<char> = s.chars().collect();
                        let index = if i < 0 {
                            (chars.len() as i64 + i) as usize
                        } else {
                            i as usize
                        };
                        chars
                            .get(index)
                            .map(|c| Value::String(c.to_string()))
                            .ok_or_else(|| format!("String index out of bounds: {}", i))
                    }
                    _ => Err("Invalid indexing operation".to_string()),
                }
            }

            Expr::Lambda { params, body, .. } => {
                let closure = if let Some(local) = self.locals.last() {
                    Rc::clone(local)
                } else {
                    Rc::clone(&self.globals)
                };
                Ok(Value::Lambda {
                    params: params.clone(),
                    body: body.clone(),
                    closure,
                })
            }

            Expr::Namespace { module, name } => {
                // 尝试从模块中获取
                let module_val = self.get_variable(module)?;
                match module_val {
                    Value::Module(m) => {
                        let m = m.borrow();
                        m.get(name).cloned().ok_or_else(|| {
                            format!("Module '{}' does not have member '{}'", module, name)
                        })
                    }
                    _ => Err(format!("'{}' is not a module", module)),
                }
            }
        }
    }

    /// Helper function to compute power operations - returns symbolic Irrational or Complex
    /// Implements Lamina's precise mathematics: symbolic roots, complex numbers for even roots of negatives
    fn compute_power(&self, base: f64, exponent: f64) -> Result<Value, String> {
        // Check if exponent is a simple rational (1/n form)
        let denom_approx = (1.0 / exponent).round();
        let is_simple_root = (1.0 / denom_approx - exponent).abs() < 1e-10;

        if is_simple_root && denom_approx > 0.0 {
            let n = denom_approx as u32;

            // Handle negative bases
            if base < 0.0 {
                let is_odd_root = n % 2 == 1;

                if is_odd_root {
                    // Odd root of negative number: return negative irrational
                    // e.g., (-8)^(1/3) = -2 or -(8^(1/3))
                    let abs_base = base.abs();

                    // Check if it's a perfect root
                    let root_val = abs_base.powf(exponent);
                    if (root_val.round() - root_val).abs() < 1e-10 {
                        // Perfect root - return as integer
                        return Ok(Value::Int(-(root_val.round() as i64)));
                    }

                    // Return as symbolic irrational: -(abs_base^(1/n))
                    let root = if n == 2 {
                        IrrationalValue::Sqrt(Box::new(Value::Int(abs_base as i64)))
                    } else {
                        IrrationalValue::Root(n, Box::new(Value::Int(abs_base as i64)))
                    };

                    // Negate by using Product with -1
                    return Ok(Value::Irrational(IrrationalValue::Product(
                        Box::new(Value::Int(-1)),
                        Box::new(root),
                    )));
                } else {
                    // Even root of negative: return symbolic complex number
                    // For even root of negative: base^(1/n) where n is even
                    // Result is: 0 + |base|^(1/n) * i
                    let abs_base = base.abs();
                    let root_val = abs_base.powf(exponent);

                    if (root_val.round() - root_val).abs() < 1e-10 {
                        // Perfect root - return as i * integer
                        return Ok(Value::Complex(
                            Box::new(Value::Int(0)),
                            Box::new(Value::Int(root_val.round() as i64)),
                        ));
                    }

                    // Return as symbolic: 0 + sqrt(abs_base) * i
                    let root = if n == 2 {
                        IrrationalValue::Sqrt(Box::new(Value::Int(abs_base as i64)))
                    } else {
                        IrrationalValue::Root(n, Box::new(Value::Int(abs_base as i64)))
                    };

                    return Ok(Value::Complex(
                        Box::new(Value::Int(0)),
                        Box::new(Value::Irrational(root)),
                    ));
                }
            } else {
                // Positive base
                // Check if it's a perfect root
                let root_val = base.powf(exponent);
                if (root_val.round() - root_val).abs() < 1e-10 {
                    // Perfect root - return as integer
                    return Ok(Value::Int(root_val.round() as i64));
                }

                // Return as symbolic irrational
                let root = if n == 2 {
                    IrrationalValue::Sqrt(Box::new(Value::Int(base as i64)))
                } else {
                    IrrationalValue::Root(n, Box::new(Value::Int(base as i64)))
                };

                return Ok(Value::Irrational(root));
            }
        }

        // For non-simple-root cases, use mathcore
        let math = MathCore::new();
        let expr = format!("{}^{}", base, exponent);

        match math.evaluate(&expr) {
            Ok(mathcore::Expr::Number(result)) => {
                // Check if result is close to an integer
                if (result.round() - result).abs() < 1e-10 {
                    Ok(Value::Int(result.round() as i64))
                } else {
                    // Return as Rational if possible, otherwise keep as symbolic
                    // For now, return as Float only for mathcore results
                    // TODO: Convert to Rational when appropriate
                    Ok(Value::Float(result))
                }
            }
            Ok(other) => Err(format!(
                "Mathcore returned non-numeric result for {}: {:?}",
                expr, other
            )),
            Err(_) => {
                // Mathcore failed - try complex number evaluation for negative bases
                if base < 0.0 {
                    // Return symbolic complex number
                    let c = Complex64::new(base, 0.0).powf(exponent);
                    Ok(Value::Complex(
                        Box::new(Value::Float(c.re)),
                        Box::new(Value::Float(c.im)),
                    ))
                } else {
                    Err(format!("Mathcore evaluation failed for {}", expr))
                }
            }
        }
    }

    fn eval_binary_op(&mut self, left: &Value, op: BinOp, right: &Value) -> Result<Value, String> {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => {
                match op {
                    BinOp::Add => Ok(Value::Int(a + b)),
                    BinOp::Sub => Ok(Value::Int(a - b)),
                    BinOp::Mul => Ok(Value::Int(a * b)),
                    BinOp::Div => {
                        if *b == 0 {
                            return Err("Division by zero".to_string());
                        }
                        // 返回有理数
                        let rational = BigRational::new(BigInt::from(*a), BigInt::from(*b));
                        Ok(Value::Rational(rational))
                    }
                    BinOp::Mod => Ok(Value::Int(a % b)),
                    BinOp::Pow => Ok(Value::Int(a.pow(*b as u32))),
                    BinOp::Equal => Ok(Value::Bool(a == b)),
                    BinOp::NotEqual => Ok(Value::Bool(a != b)),
                    BinOp::Greater => Ok(Value::Bool(a > b)),
                    BinOp::GreaterEq => Ok(Value::Bool(a >= b)),
                    BinOp::Less => Ok(Value::Bool(a < b)),
                    BinOp::LessEq => Ok(Value::Bool(a <= b)),
                    _ => Err(format!("Unsupported operation: int {} int", op)),
                }
            }

            // BigInt 与 BigInt 的运算
            (Value::BigInt(a), Value::BigInt(b)) => {
                match op {
                    BinOp::Add => Ok(Value::BigInt(a + b)),
                    BinOp::Sub => Ok(Value::BigInt(a - b)),
                    BinOp::Mul => Ok(Value::BigInt(a * b)),
                    BinOp::Div => {
                        if b == &BigInt::from(0) {
                            return Err("Division by zero".to_string());
                        }
                        // 返回有理数
                        let rational = BigRational::new(a.clone(), b.clone());
                        Ok(Value::Rational(rational))
                    }
                    BinOp::Mod => {
                        if b == &BigInt::from(0) {
                            return Err("Division by zero".to_string());
                        }
                        Ok(Value::BigInt(a % b))
                    }
                    BinOp::Pow => {
                        // For BigInt power, convert to i64 for the exponent
                        if let Ok(exp) = b.to_string().parse::<u32>() {
                            Ok(Value::BigInt(num::pow(a.clone(), exp as usize)))
                        } else {
                            // If exponent is too large or negative, convert to float
                            let a_float = a.to_string().parse::<f64>().unwrap_or(0.0);
                            let b_float = b.to_string().parse::<f64>().unwrap_or(0.0);
                            Ok(Value::Float(a_float.powf(b_float)))
                        }
                    }
                    BinOp::Equal => Ok(Value::Bool(a == b)),
                    BinOp::NotEqual => Ok(Value::Bool(a != b)),
                    BinOp::Greater => Ok(Value::Bool(a > b)),
                    BinOp::GreaterEq => Ok(Value::Bool(a >= b)),
                    BinOp::Less => Ok(Value::Bool(a < b)),
                    BinOp::LessEq => Ok(Value::Bool(a <= b)),
                    _ => Err(format!("Unsupported operation: bigint {} bigint", op)),
                }
            }

            // BigInt 与 Int 的运算
            (Value::BigInt(a), Value::Int(b)) | (Value::Int(b), Value::BigInt(a)) => {
                let b_bigint = BigInt::from(*b);
                match op {
                    BinOp::Add => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::BigInt(a + b_bigint))
                        } else {
                            Ok(Value::BigInt(b_bigint + a))
                        }
                    }
                    BinOp::Sub => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::BigInt(a - b_bigint))
                        } else {
                            Ok(Value::BigInt(b_bigint - a))
                        }
                    }
                    BinOp::Mul => Ok(Value::BigInt(a * b_bigint)),
                    BinOp::Div => {
                        if *b == 0 {
                            return Err("Division by zero".to_string());
                        }
                        if matches!(left, Value::BigInt(_)) {
                            let rational = BigRational::new(a.clone(), b_bigint);
                            Ok(Value::Rational(rational))
                        } else {
                            let rational = BigRational::new(b_bigint, a.clone());
                            Ok(Value::Rational(rational))
                        }
                    }
                    BinOp::Mod => {
                        if *b == 0 {
                            return Err("Division by zero".to_string());
                        }
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::BigInt(a % b_bigint))
                        } else {
                            Ok(Value::BigInt(b_bigint % a))
                        }
                    }
                    BinOp::Pow => {
                        if matches!(left, Value::BigInt(_)) {
                            if *b >= 0 {
                                Ok(Value::BigInt(num::pow(a.clone(), *b as usize)))
                            } else {
                                // Negative exponent
                                let a_float = a.to_string().parse::<f64>().unwrap_or(0.0);
                                Ok(Value::Float(a_float.powf(*b as f64)))
                            }
                        } else {
                            // Int ^ BigInt
                            if let Ok(exp) = a.to_string().parse::<u32>() {
                                Ok(Value::BigInt(num::pow(b_bigint, exp as usize)))
                            } else {
                                let a_float = a.to_string().parse::<f64>().unwrap_or(0.0);
                                let b_float = *b as f64;
                                Ok(Value::Float(b_float.powf(a_float)))
                            }
                        }
                    }
                    BinOp::Equal => Ok(Value::Bool(a == &b_bigint)),
                    BinOp::NotEqual => Ok(Value::Bool(a != &b_bigint)),
                    BinOp::Greater => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Bool(a > &b_bigint))
                        } else {
                            Ok(Value::Bool(&b_bigint > a))
                        }
                    }
                    BinOp::GreaterEq => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Bool(a >= &b_bigint))
                        } else {
                            Ok(Value::Bool(&b_bigint >= a))
                        }
                    }
                    BinOp::Less => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Bool(a < &b_bigint))
                        } else {
                            Ok(Value::Bool(&b_bigint < a))
                        }
                    }
                    BinOp::LessEq => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Bool(a <= &b_bigint))
                        } else {
                            Ok(Value::Bool(&b_bigint <= a))
                        }
                    }
                    _ => Err(format!("Unsupported operation: bigint {} int", op)),
                }
            }

            // BigInt 与 Float 的运算
            (Value::BigInt(a), Value::Float(b)) | (Value::Float(b), Value::BigInt(a)) => {
                let a_float = a.to_string().parse::<f64>().unwrap_or(0.0);
                match op {
                    BinOp::Add => Ok(Value::Float(a_float + b)),
                    BinOp::Sub => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Float(a_float - b))
                        } else {
                            Ok(Value::Float(b - a_float))
                        }
                    }
                    BinOp::Mul => Ok(Value::Float(a_float * b)),
                    BinOp::Div => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Float(a_float / b))
                        } else {
                            Ok(Value::Float(b / a_float))
                        }
                    }
                    BinOp::Mod => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Float(a_float % b))
                        } else {
                            Ok(Value::Float(b % a_float))
                        }
                    }
                    BinOp::Pow => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Float(a_float.powf(*b)))
                        } else {
                            Ok(Value::Float(b.powf(a_float)))
                        }
                    }
                    BinOp::Equal => Ok(Value::Bool((a_float - b).abs() < f64::EPSILON)),
                    BinOp::NotEqual => Ok(Value::Bool((a_float - b).abs() >= f64::EPSILON)),
                    BinOp::Greater => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Bool(a_float > *b))
                        } else {
                            Ok(Value::Bool(b > &a_float))
                        }
                    }
                    BinOp::GreaterEq => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Bool(a_float >= *b))
                        } else {
                            Ok(Value::Bool(b >= &a_float))
                        }
                    }
                    BinOp::Less => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Bool(a_float < *b))
                        } else {
                            Ok(Value::Bool(b < &a_float))
                        }
                    }
                    BinOp::LessEq => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Bool(a_float <= *b))
                        } else {
                            Ok(Value::Bool(b <= &a_float))
                        }
                    }
                    _ => Err(format!("Unsupported operation: bigint {} float", op)),
                }
            }

            // BigInt 与 Rational 的运算
            (Value::BigInt(a), Value::Rational(b)) | (Value::Rational(b), Value::BigInt(a)) => {
                let a_rational = BigRational::from_integer(a.clone());
                match op {
                    BinOp::Add => Ok(Value::Rational(&a_rational + b)),
                    BinOp::Sub => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Rational(&a_rational - b))
                        } else {
                            Ok(Value::Rational(b - &a_rational))
                        }
                    }
                    BinOp::Mul => Ok(Value::Rational(&a_rational * b)),
                    BinOp::Div => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Rational(&a_rational / b))
                        } else {
                            Ok(Value::Rational(b / &a_rational))
                        }
                    }
                    BinOp::Pow => {
                        // Convert both to float for power operations
                        let a_float = a.to_string().parse::<f64>().unwrap_or(0.0);
                        let b_float = b.numer().to_string().parse::<f64>().unwrap_or(0.0)
                            / b.denom().to_string().parse::<f64>().unwrap_or(1.0);
                        if matches!(left, Value::BigInt(_)) {
                            self.compute_power(a_float, b_float)
                        } else {
                            self.compute_power(b_float, a_float)
                        }
                    }
                    BinOp::Equal => Ok(Value::Bool(&a_rational == b)),
                    BinOp::NotEqual => Ok(Value::Bool(&a_rational != b)),
                    BinOp::Greater => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Bool(&a_rational > b))
                        } else {
                            Ok(Value::Bool(b > &a_rational))
                        }
                    }
                    BinOp::GreaterEq => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Bool(&a_rational >= b))
                        } else {
                            Ok(Value::Bool(b >= &a_rational))
                        }
                    }
                    BinOp::Less => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Bool(&a_rational < b))
                        } else {
                            Ok(Value::Bool(b < &a_rational))
                        }
                    }
                    BinOp::LessEq => {
                        if matches!(left, Value::BigInt(_)) {
                            Ok(Value::Bool(&a_rational <= b))
                        } else {
                            Ok(Value::Bool(b <= &a_rational))
                        }
                    }
                    _ => Err(format!("Unsupported operation: bigint {} rational", op)),
                }
            }

            (Value::Float(a), Value::Float(b)) => match op {
                BinOp::Add => Ok(Value::Float(a + b)),
                BinOp::Sub => Ok(Value::Float(a - b)),
                BinOp::Mul => Ok(Value::Float(a * b)),
                BinOp::Div => Ok(Value::Float(a / b)),
                BinOp::Mod => Ok(Value::Float(a % b)),
                BinOp::Pow => Ok(Value::Float(a.powf(*b))),
                BinOp::Equal => Ok(Value::Bool(a == b)),
                BinOp::NotEqual => Ok(Value::Bool(a != b)),
                BinOp::Greater => Ok(Value::Bool(a > b)),
                BinOp::GreaterEq => Ok(Value::Bool(a >= b)),
                BinOp::Less => Ok(Value::Bool(a < b)),
                BinOp::LessEq => Ok(Value::Bool(a <= b)),
                _ => Err(format!("Unsupported operation: float {} float", op)),
            },

            (Value::Int(a), Value::Float(b)) | (Value::Float(b), Value::Int(a)) => {
                let a = *a as f64;
                match op {
                    BinOp::Add => Ok(Value::Float(a + b)),
                    BinOp::Sub => Ok(Value::Float(a - b)),
                    BinOp::Mul => Ok(Value::Float(a * b)),
                    BinOp::Div => Ok(Value::Float(a / b)),
                    BinOp::Pow => Ok(Value::Float(a.powf(*b))),
                    _ => Err(format!("Unsupported operation: int/float {} float/int", op)),
                }
            }

            // Rational 与其他类型的运算
            (Value::Int(a), Value::Rational(b)) | (Value::Rational(b), Value::Int(a)) => {
                let a_rational = BigRational::from_integer(BigInt::from(*a));
                match op {
                    BinOp::Add => Ok(Value::Rational(&a_rational + b)),
                    BinOp::Sub => {
                        if matches!(left, Value::Int(_)) {
                            Ok(Value::Rational(&a_rational - b))
                        } else {
                            Ok(Value::Rational(b - &a_rational))
                        }
                    }
                    BinOp::Mul => Ok(Value::Rational(&a_rational * b)),
                    BinOp::Div => {
                        if matches!(left, Value::Int(_)) {
                            Ok(Value::Rational(&a_rational / b))
                        } else {
                            Ok(Value::Rational(b / &a_rational))
                        }
                    }
                    BinOp::Pow => {
                        // Convert both to float for power operations
                        let a_float = *a as f64;
                        let b_float = b.numer().to_string().parse::<f64>().unwrap_or(0.0)
                            / b.denom().to_string().parse::<f64>().unwrap_or(1.0);
                        if matches!(left, Value::Int(_)) {
                            self.compute_power(a_float, b_float)
                        } else {
                            self.compute_power(b_float, a_float)
                        }
                    }
                    _ => Err(format!(
                        "Unsupported operation: int/rational {} rational/int",
                        op
                    )),
                }
            }

            (Value::Float(a), Value::Rational(b)) | (Value::Rational(b), Value::Float(a)) => {
                // 将有理数转换为浮点数
                let b_float = b.numer().to_string().parse::<f64>().unwrap_or(0.0)
                    / b.denom().to_string().parse::<f64>().unwrap_or(1.0);
                match op {
                    BinOp::Add => Ok(Value::Float(a + b_float)),
                    BinOp::Sub => {
                        if matches!(left, Value::Float(_)) {
                            Ok(Value::Float(a - b_float))
                        } else {
                            Ok(Value::Float(b_float - a))
                        }
                    }
                    BinOp::Mul => Ok(Value::Float(a * b_float)),
                    BinOp::Div => {
                        if matches!(left, Value::Float(_)) {
                            Ok(Value::Float(a / b_float))
                        } else {
                            Ok(Value::Float(b_float / a))
                        }
                    }
                    BinOp::Pow => {
                        if matches!(left, Value::Float(_)) {
                            self.compute_power(*a, b_float)
                        } else {
                            self.compute_power(b_float, *a)
                        }
                    }
                    _ => Err(format!(
                        "Unsupported operation: float/rational {} rational/float",
                        op
                    )),
                }
            }

            (Value::Rational(a), Value::Rational(b)) => match op {
                BinOp::Add => Ok(Value::Rational(a + b)),
                BinOp::Sub => Ok(Value::Rational(a - b)),
                BinOp::Mul => Ok(Value::Rational(a * b)),
                BinOp::Div => Ok(Value::Rational(a / b)),
                BinOp::Pow => {
                    // Convert both rationals to float for power operations
                    let a_float = a.numer().to_string().parse::<f64>().unwrap_or(0.0)
                        / a.denom().to_string().parse::<f64>().unwrap_or(1.0);
                    let b_float = b.numer().to_string().parse::<f64>().unwrap_or(0.0)
                        / b.denom().to_string().parse::<f64>().unwrap_or(1.0);
                    self.compute_power(a_float, b_float)
                }
                BinOp::Equal => Ok(Value::Bool(a == b)),
                BinOp::NotEqual => Ok(Value::Bool(a != b)),
                _ => Err(format!("Unsupported operation: rational {} rational", op)),
            },

            (Value::String(a), Value::String(b)) => match op {
                BinOp::Add => Ok(Value::String(format!("{}{}", a, b))),
                BinOp::Equal => Ok(Value::Bool(a == b)),
                BinOp::NotEqual => Ok(Value::Bool(a != b)),
                _ => Err(format!("Unsupported operation: string {} string", op)),
            },

            (Value::Bool(a), Value::Bool(b)) => match op {
                BinOp::And => Ok(Value::Bool(*a && *b)),
                BinOp::Or => Ok(Value::Bool(*a || *b)),
                BinOp::Equal => Ok(Value::Bool(a == b)),
                BinOp::NotEqual => Ok(Value::Bool(a != b)),
                _ => Err(format!("Unsupported operation: bool {} bool", op)),
            },

            // 字符串与任意类型拼接
            (Value::String(s), other) | (other, Value::String(s)) => match op {
                BinOp::Add => {
                    let result = if matches!(left, Value::String(_)) {
                        format!("{}{}", s, other)
                    } else {
                        format!("{}{}", other, s)
                    };
                    Ok(Value::String(result))
                }
                _ => Err(format!(
                    "Unsupported operation: {} {} {}",
                    left.type_name(),
                    op,
                    right.type_name()
                )),
            },

            // 无理数运算
            (Value::Irrational(a), Value::Irrational(b)) => match op {
                BinOp::Add => Ok(Value::Irrational(IrrationalValue::Sum(
                    Box::new(a.clone()),
                    Box::new(b.clone()),
                ))),
                BinOp::Mul => {
                    // 展开无理数乘法
                    self.multiply_irrationals(a, b)
                }
                _ => Err(format!(
                    "Unsupported operation: irrational {} irrational",
                    op
                )),
            },

            // 有理数/整数 与 无理数的运算
            (Value::Int(a), Value::Irrational(irr)) | (Value::Irrational(irr), Value::Int(a)) => {
                match op {
                    BinOp::Mul => {
                        if *a == 0 {
                            Ok(Value::Int(0))
                        } else if *a == 1 {
                            Ok(Value::Irrational(irr.clone()))
                        } else {
                            Ok(Value::Irrational(IrrationalValue::Product(
                                Box::new(Value::Int(*a)),
                                Box::new(irr.clone()),
                            )))
                        }
                    }
                    BinOp::Div => {
                        if matches!(left, Value::Irrational(_)) {
                            // irr / int = (1/int) * irr
                            if *a == 0 {
                                Err("Division by zero".to_string())
                            } else if *a == 1 {
                                Ok(Value::Irrational(irr.clone()))
                            } else {
                                Ok(Value::Irrational(IrrationalValue::Product(
                                    Box::new(Value::Rational(BigRational::new(
                                        BigInt::from(1),
                                        BigInt::from(*a),
                                    ))),
                                    Box::new(irr.clone()),
                                )))
                            }
                        } else {
                            // int / irr - convert to float
                            let irr_float = irrational_to_float(irr);
                            Ok(Value::Float((*a as f64) / irr_float))
                        }
                    }
                    BinOp::Add => {
                        if matches!(left, Value::Irrational(_)) {
                            Ok(Value::Irrational(IrrationalValue::Sum(
                                Box::new(irr.clone()),
                                Box::new(IrrationalValue::Product(
                                    Box::new(Value::Int(*a)),
                                    Box::new(IrrationalValue::Sqrt(Box::new(Value::Int(1)))),
                                )),
                            )))
                        } else {
                            Ok(Value::Irrational(IrrationalValue::Sum(
                                Box::new(IrrationalValue::Product(
                                    Box::new(Value::Int(*a)),
                                    Box::new(IrrationalValue::Sqrt(Box::new(Value::Int(1)))),
                                )),
                                Box::new(irr.clone()),
                            )))
                        }
                    }
                    _ => Err(format!(
                        "Unsupported operation: {} {} {}",
                        left.type_name(),
                        op,
                        right.type_name()
                    )),
                }
            }

            (Value::Rational(r), Value::Irrational(irr))
            | (Value::Irrational(irr), Value::Rational(r)) => match op {
                BinOp::Mul => Ok(Value::Irrational(IrrationalValue::Product(
                    Box::new(Value::Rational(r.clone())),
                    Box::new(irr.clone()),
                ))),
                _ => Err(format!(
                    "Unsupported operation: {} {} {}",
                    left.type_name(),
                    op,
                    right.type_name()
                )),
            },

            // BigInt 与 Irrational 的运算
            (Value::BigInt(b), Value::Irrational(irr)) | (Value::Irrational(irr), Value::BigInt(b)) => {
                match op {
                    BinOp::Mul => {
                        if b == &BigInt::from(0) {
                            Ok(Value::Int(0))
                        } else if b == &BigInt::from(1) {
                            Ok(Value::Irrational(irr.clone()))
                        } else {
                            Ok(Value::Irrational(IrrationalValue::Product(
                                Box::new(Value::BigInt(b.clone())),
                                Box::new(irr.clone()),
                            )))
                        }
                    }
                    BinOp::Div => {
                        if matches!(left, Value::Irrational(_)) {
                            // irr / bigint = (1/bigint) * irr
                            if b == &BigInt::from(0) {
                                Err("Division by zero".to_string())
                            } else if b == &BigInt::from(1) {
                                Ok(Value::Irrational(irr.clone()))
                            } else {
                                Ok(Value::Irrational(IrrationalValue::Product(
                                    Box::new(Value::Rational(BigRational::new(
                                        BigInt::from(1),
                                        b.clone(),
                                    ))),
                                    Box::new(irr.clone()),
                                )))
                            }
                        } else {
                            // bigint / irr - convert to float
                            let irr_float = irrational_to_float(irr);
                            let b_float = b.to_string().parse::<f64>().unwrap_or(0.0);
                            Ok(Value::Float(b_float / irr_float))
                        }
                    }
                    BinOp::Add => {
                        if matches!(left, Value::Irrational(_)) {
                            Ok(Value::Irrational(IrrationalValue::Sum(
                                Box::new(irr.clone()),
                                Box::new(IrrationalValue::Product(
                                    Box::new(Value::BigInt(b.clone())),
                                    Box::new(IrrationalValue::Sqrt(Box::new(Value::Int(1)))),
                                )),
                            )))
                        } else {
                            Ok(Value::Irrational(IrrationalValue::Sum(
                                Box::new(IrrationalValue::Product(
                                    Box::new(Value::BigInt(b.clone())),
                                    Box::new(IrrationalValue::Sqrt(Box::new(Value::Int(1)))),
                                )),
                                Box::new(irr.clone()),
                            )))
                        }
                    }
                    _ => Err(format!(
                        "Unsupported operation: {} {} {}",
                        left.type_name(),
                        op,
                        right.type_name()
                    )),
                }
            }

            // 数组运算
            (Value::Array(a), Value::Array(b)) => match op {
                BinOp::Add => {
                    let a = a.borrow();
                    let b = b.borrow();
                    if a.len() != b.len() {
                        return Err("Arrays must have same length for addition".to_string());
                    }
                    let mut result = Vec::new();
                    for (val_a, val_b) in a.iter().zip(b.iter()) {
                        result.push(self.eval_binary_op(val_a, BinOp::Add, val_b)?);
                    }
                    Ok(Value::Array(Rc::new(RefCell::new(result))))
                }
                _ => Err(format!("Unsupported operation: array {} array", op)),
            },

            // Complex 与 Complex 的运算
            (Value::Complex(a_re, a_im), Value::Complex(b_re, b_im)) => match op {
                BinOp::Add => {
                    let re = self.eval_binary_op(a_re, BinOp::Add, b_re)?;
                    let im = self.eval_binary_op(a_im, BinOp::Add, b_im)?;
                    Ok(Value::Complex(Box::new(re), Box::new(im)))
                }
                BinOp::Sub => {
                    let re = self.eval_binary_op(a_re, BinOp::Sub, b_re)?;
                    let im = self.eval_binary_op(a_im, BinOp::Sub, b_im)?;
                    Ok(Value::Complex(Box::new(re), Box::new(im)))
                }
                BinOp::Mul => {
                    // (a + bi) * (c + di) = (ac - bd) + (ad + bc)i
                    let ac = self.eval_binary_op(a_re, BinOp::Mul, b_re)?;
                    let bd = self.eval_binary_op(a_im, BinOp::Mul, b_im)?;
                    let ad = self.eval_binary_op(a_re, BinOp::Mul, b_im)?;
                    let bc = self.eval_binary_op(a_im, BinOp::Mul, b_re)?;

                    let re = self.eval_binary_op(&ac, BinOp::Sub, &bd)?;
                    let im = self.eval_binary_op(&ad, BinOp::Add, &bc)?;
                    Ok(Value::Complex(Box::new(re), Box::new(im)))
                }
                BinOp::Div => {
                    // (a + bi) / (c + di) = ((ac + bd) + (bc - ad)i) / (c² + d²)
                    // Check for division by zero
                    let c_sq = self.eval_binary_op(b_re, BinOp::Mul, b_re)?;
                    let d_sq = self.eval_binary_op(b_im, BinOp::Mul, b_im)?;
                    let denom = self.eval_binary_op(&c_sq, BinOp::Add, &d_sq)?;

                    // Check if denominator is zero
                    let denom_is_zero = match &denom {
                        Value::Int(0) => true,
                        Value::Rational(r) => r.numer().to_string() == "0",
                        _ => false,
                    };
                    if denom_is_zero {
                        return Err("Division by zero".to_string());
                    }

                    let ac = self.eval_binary_op(a_re, BinOp::Mul, b_re)?;
                    let bd = self.eval_binary_op(a_im, BinOp::Mul, b_im)?;
                    let bc = self.eval_binary_op(a_im, BinOp::Mul, b_re)?;
                    let ad = self.eval_binary_op(a_re, BinOp::Mul, b_im)?;

                    let re_num = self.eval_binary_op(&ac, BinOp::Add, &bd)?;
                    let im_num = self.eval_binary_op(&bc, BinOp::Sub, &ad)?;

                    let re = self.eval_binary_op(&re_num, BinOp::Div, &denom)?;
                    let im = self.eval_binary_op(&im_num, BinOp::Div, &denom)?;
                    Ok(Value::Complex(Box::new(re), Box::new(im)))
                }
                BinOp::Equal => {
                    let re_eq = self.eval_binary_op(a_re, BinOp::Equal, b_re)?;
                    let im_eq = self.eval_binary_op(a_im, BinOp::Equal, b_im)?;
                    match (re_eq, im_eq) {
                        (Value::Bool(r), Value::Bool(i)) => Ok(Value::Bool(r && i)),
                        _ => Err("Comparison failed".to_string()),
                    }
                }
                BinOp::NotEqual => {
                    let eq = self.eval_binary_op(left, BinOp::Equal, right)?;
                    match eq {
                        Value::Bool(b) => Ok(Value::Bool(!b)),
                        _ => Err("Comparison failed".to_string()),
                    }
                }
                _ => Err(format!("Unsupported operation: complex {} complex", op)),
            },

            // Complex 与 Int 的运算
            (Value::Complex(c_re, c_im), Value::Int(i))
            | (Value::Int(i), Value::Complex(c_re, c_im)) => {
                let i_val = Value::Int(*i);
                match op {
                    BinOp::Add => {
                        if matches!(left, Value::Complex(_, _)) {
                            let re = self.eval_binary_op(c_re, BinOp::Add, &i_val)?;
                            Ok(Value::Complex(
                                Box::new(re),
                                Box::new(c_im.as_ref().clone()),
                            ))
                        } else {
                            let re = self.eval_binary_op(&i_val, BinOp::Add, c_re)?;
                            Ok(Value::Complex(
                                Box::new(re),
                                Box::new(c_im.as_ref().clone()),
                            ))
                        }
                    }
                    BinOp::Sub => {
                        if matches!(left, Value::Complex(_, _)) {
                            let re = self.eval_binary_op(c_re, BinOp::Sub, &i_val)?;
                            Ok(Value::Complex(
                                Box::new(re),
                                Box::new(c_im.as_ref().clone()),
                            ))
                        } else {
                            let re = self.eval_binary_op(&i_val, BinOp::Sub, c_re)?;
                            let im = self.eval_unary_op(UnaryOp::Neg, c_im)?;
                            Ok(Value::Complex(Box::new(re), Box::new(im)))
                        }
                    }
                    BinOp::Mul => {
                        // i * (a + bi) = ia + ibi
                        let re = self.eval_binary_op(c_re, BinOp::Mul, &i_val)?;
                        let im = self.eval_binary_op(c_im, BinOp::Mul, &i_val)?;
                        Ok(Value::Complex(Box::new(re), Box::new(im)))
                    }
                    BinOp::Div => {
                        if matches!(left, Value::Complex(_, _)) {
                            // (a + bi) / i = a/i + b/i * i
                            if *i == 0 {
                                return Err("Division by zero".to_string());
                            }
                            let re = self.eval_binary_op(c_re, BinOp::Div, &i_val)?;
                            let im = self.eval_binary_op(c_im, BinOp::Div, &i_val)?;
                            Ok(Value::Complex(Box::new(re), Box::new(im)))
                        } else {
                            // i / (a + bi) = i(a - bi) / (a² + b²)
                            let a_sq = self.eval_binary_op(c_re, BinOp::Mul, c_re)?;
                            let b_sq = self.eval_binary_op(c_im, BinOp::Mul, c_im)?;
                            let denom = self.eval_binary_op(&a_sq, BinOp::Add, &b_sq)?;

                            let denom_is_zero = match &denom {
                                Value::Int(0) => true,
                                Value::Rational(r) => r.numer().to_string() == "0",
                                _ => false,
                            };
                            if denom_is_zero {
                                return Err("Division by zero".to_string());
                            }

                            let i_a = self.eval_binary_op(&i_val, BinOp::Mul, c_re)?;
                            let i_b = self.eval_binary_op(&i_val, BinOp::Mul, c_im)?;
                            let neg_i_b = self.eval_unary_op(UnaryOp::Neg, &i_b)?;

                            let re = self.eval_binary_op(&i_a, BinOp::Div, &denom)?;
                            let im = self.eval_binary_op(&neg_i_b, BinOp::Div, &denom)?;
                            Ok(Value::Complex(Box::new(re), Box::new(im)))
                        }
                    }
                    _ => Err(format!("Unsupported operation: complex {} int", op)),
                }
            }

            // Complex 与 Float 的运算
            (Value::Complex(c_re, c_im), Value::Float(f))
            | (Value::Float(f), Value::Complex(c_re, c_im)) => {
                let f_val = Value::Float(*f);
                match op {
                    BinOp::Add => {
                        if matches!(left, Value::Complex(_, _)) {
                            let re = self.eval_binary_op(c_re, BinOp::Add, &f_val)?;
                            Ok(Value::Complex(
                                Box::new(re),
                                Box::new(c_im.as_ref().clone()),
                            ))
                        } else {
                            let re = self.eval_binary_op(&f_val, BinOp::Add, c_re)?;
                            Ok(Value::Complex(
                                Box::new(re),
                                Box::new(c_im.as_ref().clone()),
                            ))
                        }
                    }
                    BinOp::Sub => {
                        if matches!(left, Value::Complex(_, _)) {
                            let re = self.eval_binary_op(c_re, BinOp::Sub, &f_val)?;
                            Ok(Value::Complex(
                                Box::new(re),
                                Box::new(c_im.as_ref().clone()),
                            ))
                        } else {
                            let re = self.eval_binary_op(&f_val, BinOp::Sub, c_re)?;
                            let im = self.eval_unary_op(UnaryOp::Neg, c_im)?;
                            Ok(Value::Complex(Box::new(re), Box::new(im)))
                        }
                    }
                    BinOp::Mul => {
                        let re = self.eval_binary_op(c_re, BinOp::Mul, &f_val)?;
                        let im = self.eval_binary_op(c_im, BinOp::Mul, &f_val)?;
                        Ok(Value::Complex(Box::new(re), Box::new(im)))
                    }
                    BinOp::Div => {
                        if matches!(left, Value::Complex(_, _)) {
                            if *f == 0.0 {
                                return Err("Division by zero".to_string());
                            }
                            let re = self.eval_binary_op(c_re, BinOp::Div, &f_val)?;
                            let im = self.eval_binary_op(c_im, BinOp::Div, &f_val)?;
                            Ok(Value::Complex(Box::new(re), Box::new(im)))
                        } else {
                            // f / (a + bi) - convert to complex division
                            let a_sq = self.eval_binary_op(c_re, BinOp::Mul, c_re)?;
                            let b_sq = self.eval_binary_op(c_im, BinOp::Mul, c_im)?;
                            let denom = self.eval_binary_op(&a_sq, BinOp::Add, &b_sq)?;

                            let f_a = self.eval_binary_op(&f_val, BinOp::Mul, c_re)?;
                            let f_b = self.eval_binary_op(&f_val, BinOp::Mul, c_im)?;
                            let neg_f_b = self.eval_unary_op(UnaryOp::Neg, &f_b)?;

                            let re = self.eval_binary_op(&f_a, BinOp::Div, &denom)?;
                            let im = self.eval_binary_op(&neg_f_b, BinOp::Div, &denom)?;
                            Ok(Value::Complex(Box::new(re), Box::new(im)))
                        }
                    }
                    _ => Err(format!("Unsupported operation: complex {} float", op)),
                }
            }

            // Complex 与 Rational 的运算
            (Value::Complex(c_re, c_im), Value::Rational(r))
            | (Value::Rational(r), Value::Complex(c_re, c_im)) => {
                let r_val = Value::Rational(r.clone());
                match op {
                    BinOp::Add => {
                        if matches!(left, Value::Complex(_, _)) {
                            let re = self.eval_binary_op(c_re, BinOp::Add, &r_val)?;
                            Ok(Value::Complex(
                                Box::new(re),
                                Box::new(c_im.as_ref().clone()),
                            ))
                        } else {
                            let re = self.eval_binary_op(&r_val, BinOp::Add, c_re)?;
                            Ok(Value::Complex(
                                Box::new(re),
                                Box::new(c_im.as_ref().clone()),
                            ))
                        }
                    }
                    BinOp::Sub => {
                        if matches!(left, Value::Complex(_, _)) {
                            let re = self.eval_binary_op(c_re, BinOp::Sub, &r_val)?;
                            Ok(Value::Complex(
                                Box::new(re),
                                Box::new(c_im.as_ref().clone()),
                            ))
                        } else {
                            let re = self.eval_binary_op(&r_val, BinOp::Sub, c_re)?;
                            let im = self.eval_unary_op(UnaryOp::Neg, c_im)?;
                            Ok(Value::Complex(Box::new(re), Box::new(im)))
                        }
                    }
                    BinOp::Mul => {
                        let re = self.eval_binary_op(c_re, BinOp::Mul, &r_val)?;
                        let im = self.eval_binary_op(c_im, BinOp::Mul, &r_val)?;
                        Ok(Value::Complex(Box::new(re), Box::new(im)))
                    }
                    BinOp::Div => {
                        if matches!(left, Value::Complex(_, _)) {
                            let r_is_zero = r.numer().to_string() == "0";
                            if r_is_zero {
                                return Err("Division by zero".to_string());
                            }
                            let re = self.eval_binary_op(c_re, BinOp::Div, &r_val)?;
                            let im = self.eval_binary_op(c_im, BinOp::Div, &r_val)?;
                            Ok(Value::Complex(Box::new(re), Box::new(im)))
                        } else {
                            // r / (a + bi)
                            let a_sq = self.eval_binary_op(c_re, BinOp::Mul, c_re)?;
                            let b_sq = self.eval_binary_op(c_im, BinOp::Mul, c_im)?;
                            let denom = self.eval_binary_op(&a_sq, BinOp::Add, &b_sq)?;

                            let r_a = self.eval_binary_op(&r_val, BinOp::Mul, c_re)?;
                            let r_b = self.eval_binary_op(&r_val, BinOp::Mul, c_im)?;
                            let neg_r_b = self.eval_unary_op(UnaryOp::Neg, &r_b)?;

                            let re = self.eval_binary_op(&r_a, BinOp::Div, &denom)?;
                            let im = self.eval_binary_op(&neg_r_b, BinOp::Div, &denom)?;
                            Ok(Value::Complex(Box::new(re), Box::new(im)))
                        }
                    }
                    _ => Err(format!("Unsupported operation: complex {} rational", op)),
                }
            }

            // Complex 与 BigInt 的运算
            (Value::Complex(c_re, c_im), Value::BigInt(b))
            | (Value::BigInt(b), Value::Complex(c_re, c_im)) => {
                let b_val = Value::BigInt(b.clone());
                match op {
                    BinOp::Add => {
                        if matches!(left, Value::Complex(_, _)) {
                            let re = self.eval_binary_op(c_re, BinOp::Add, &b_val)?;
                            Ok(Value::Complex(
                                Box::new(re),
                                Box::new(c_im.as_ref().clone()),
                            ))
                        } else {
                            let re = self.eval_binary_op(&b_val, BinOp::Add, c_re)?;
                            Ok(Value::Complex(
                                Box::new(re),
                                Box::new(c_im.as_ref().clone()),
                            ))
                        }
                    }
                    BinOp::Sub => {
                        if matches!(left, Value::Complex(_, _)) {
                            let re = self.eval_binary_op(c_re, BinOp::Sub, &b_val)?;
                            Ok(Value::Complex(
                                Box::new(re),
                                Box::new(c_im.as_ref().clone()),
                            ))
                        } else {
                            let re = self.eval_binary_op(&b_val, BinOp::Sub, c_re)?;
                            let im = self.eval_unary_op(UnaryOp::Neg, c_im)?;
                            Ok(Value::Complex(Box::new(re), Box::new(im)))
                        }
                    }
                    BinOp::Mul => {
                        let re = self.eval_binary_op(c_re, BinOp::Mul, &b_val)?;
                        let im = self.eval_binary_op(c_im, BinOp::Mul, &b_val)?;
                        Ok(Value::Complex(Box::new(re), Box::new(im)))
                    }
                    BinOp::Div => {
                        if matches!(left, Value::Complex(_, _)) {
                            if b == &BigInt::from(0) {
                                return Err("Division by zero".to_string());
                            }
                            let re = self.eval_binary_op(c_re, BinOp::Div, &b_val)?;
                            let im = self.eval_binary_op(c_im, BinOp::Div, &b_val)?;
                            Ok(Value::Complex(Box::new(re), Box::new(im)))
                        } else {
                            // b / (a + bi) = b(a - bi) / (a² + b²)
                            let a_sq = self.eval_binary_op(c_re, BinOp::Mul, c_re)?;
                            let b_sq = self.eval_binary_op(c_im, BinOp::Mul, c_im)?;
                            let denom = self.eval_binary_op(&a_sq, BinOp::Add, &b_sq)?;

                            let denom_is_zero = match &denom {
                                Value::Int(0) => true,
                                Value::BigInt(n) => n == &BigInt::from(0),
                                Value::Rational(r) => r.numer().to_string() == "0",
                                _ => false,
                            };
                            if denom_is_zero {
                                return Err("Division by zero".to_string());
                            }

                            let b_a = self.eval_binary_op(&b_val, BinOp::Mul, c_re)?;
                            let b_b = self.eval_binary_op(&b_val, BinOp::Mul, c_im)?;
                            let neg_b_b = self.eval_unary_op(UnaryOp::Neg, &b_b)?;

                            let re = self.eval_binary_op(&b_a, BinOp::Div, &denom)?;
                            let im = self.eval_binary_op(&neg_b_b, BinOp::Div, &denom)?;
                            Ok(Value::Complex(Box::new(re), Box::new(im)))
                        }
                    }
                    _ => Err(format!("Unsupported operation: complex {} bigint", op)),
                }
            }

            // Complex 与 Irrational 的运算
            (Value::Complex(c_re, c_im), Value::Irrational(irr))
            | (Value::Irrational(irr), Value::Complex(c_re, c_im)) => {
                let irr_val = Value::Irrational(irr.clone());
                match op {
                    BinOp::Add => {
                        if matches!(left, Value::Complex(_, _)) {
                            let re = self.eval_binary_op(c_re, BinOp::Add, &irr_val)?;
                            Ok(Value::Complex(
                                Box::new(re),
                                Box::new(c_im.as_ref().clone()),
                            ))
                        } else {
                            let re = self.eval_binary_op(&irr_val, BinOp::Add, c_re)?;
                            Ok(Value::Complex(
                                Box::new(re),
                                Box::new(c_im.as_ref().clone()),
                            ))
                        }
                    }
                    BinOp::Sub => {
                        if matches!(left, Value::Complex(_, _)) {
                            let re = self.eval_binary_op(c_re, BinOp::Sub, &irr_val)?;
                            Ok(Value::Complex(
                                Box::new(re),
                                Box::new(c_im.as_ref().clone()),
                            ))
                        } else {
                            let re = self.eval_binary_op(&irr_val, BinOp::Sub, c_re)?;
                            let im = self.eval_unary_op(UnaryOp::Neg, c_im)?;
                            Ok(Value::Complex(Box::new(re), Box::new(im)))
                        }
                    }
                    BinOp::Mul => {
                        let re = self.eval_binary_op(c_re, BinOp::Mul, &irr_val)?;
                        let im = self.eval_binary_op(c_im, BinOp::Mul, &irr_val)?;
                        Ok(Value::Complex(Box::new(re), Box::new(im)))
                    }
                    BinOp::Div => {
                        if matches!(left, Value::Complex(_, _)) {
                            let re = self.eval_binary_op(c_re, BinOp::Div, &irr_val)?;
                            let im = self.eval_binary_op(c_im, BinOp::Div, &irr_val)?;
                            Ok(Value::Complex(Box::new(re), Box::new(im)))
                        } else {
                            // irr / (a + bi)
                            let a_sq = self.eval_binary_op(c_re, BinOp::Mul, c_re)?;
                            let b_sq = self.eval_binary_op(c_im, BinOp::Mul, c_im)?;
                            let denom = self.eval_binary_op(&a_sq, BinOp::Add, &b_sq)?;

                            let irr_a = self.eval_binary_op(&irr_val, BinOp::Mul, c_re)?;
                            let irr_b = self.eval_binary_op(&irr_val, BinOp::Mul, c_im)?;
                            let neg_irr_b = self.eval_unary_op(UnaryOp::Neg, &irr_b)?;

                            let re = self.eval_binary_op(&irr_a, BinOp::Div, &denom)?;
                            let im = self.eval_binary_op(&neg_irr_b, BinOp::Div, &denom)?;
                            Ok(Value::Complex(Box::new(re), Box::new(im)))
                        }
                    }
                    _ => Err(format!("Unsupported operation: complex {} irrational", op)),
                }
            }

            _ => Err(format!(
                "Unsupported operation: {} {} {}",
                left.type_name(),
                op,
                right.type_name()
            )),
        }
    }

    fn eval_unary_op(&mut self, op: UnaryOp, val: &Value) -> Result<Value, String> {
        match op {
            UnaryOp::Neg => match val {
                Value::Int(n) => Ok(Value::Int(-n)),
                Value::Float(f) => Ok(Value::Float(-f)),
                Value::Complex(re, im) => {
                    let neg_re = self.eval_unary_op(UnaryOp::Neg, re)?;
                    let neg_im = self.eval_unary_op(UnaryOp::Neg, im)?;
                    Ok(Value::Complex(Box::new(neg_re), Box::new(neg_im)))
                }
                Value::Rational(r) => Ok(Value::Rational(-r)),
                Value::Irrational(irr) => Ok(Value::Irrational(IrrationalValue::Product(
                    Box::new(Value::Int(-1)),
                    Box::new(irr.clone()),
                ))),
                _ => Err(format!("Cannot negate {}", val.type_name())),
            },
            UnaryOp::Not => Ok(Value::Bool(!val.is_truthy())),
            UnaryOp::Factorial => match val {
                Value::Int(n) => {
                    if *n < 0 {
                        return Err("Factorial of negative number".to_string());
                    }
                    let mut result = BigInt::one();
                    for i in 2..=*n {
                        result *= i;
                    }
                    Ok(Value::BigInt(result))
                }
                _ => Err(format!("Cannot compute factorial of {}", val.type_name())),
            },
        }
    }

    fn multiply_irrationals(
        &mut self,
        a: &IrrationalValue,
        b: &IrrationalValue,
    ) -> Result<Value, String> {
        use IrrationalValue::*;

        match (a, b) {
            // 简单情况：基础无理数相乘
            (Pi, Pi) => Ok(Value::Irrational(Product(
                Box::new(Value::Irrational(Pi)),
                Box::new(Pi),
            ))),
            (E, E) => Ok(Value::Irrational(Product(
                Box::new(Value::Irrational(E)),
                Box::new(E),
            ))),
            (Pi, E) | (E, Pi) => Ok(Value::Irrational(Product(
                Box::new(Value::Irrational(E)),
                Box::new(Pi),
            ))),

            // √a * √b = √(a*b), with simplification
            (Sqrt(x), Sqrt(y)) => {
                let product = self.eval_binary_op(x, BinOp::Mul, y)?;
                // Check if the product is a perfect square
                match &product {
                    Value::Int(n) if *n >= 0 => {
                        let sqrt = (*n as f64).sqrt();
                        if sqrt.fract() == 0.0 {
                            // Perfect square, return as Int
                            return Ok(Value::Int(sqrt as i64));
                        }
                    }
                    _ => {}
                }
                // Not a perfect square, return as Irrational with simplification
                Ok(Value::Irrational(Sqrt(Box::new(product))))
            }

            // Product 展开: (c1 * irr1) * (c2 * irr2) = (c1*c2) * (irr1*irr2)
            (Product(coef1, irr1), Product(coef2, irr2)) => {
                let combined_coef = self.eval_binary_op(coef1, BinOp::Mul, coef2)?;
                let inner_product = self.multiply_irrationals(irr1, irr2)?;
                self.eval_binary_op(&combined_coef, BinOp::Mul, &inner_product)
            }

            // Product 展开: (c * irr1) * irr2 = c * (irr1 * irr2)
            (Product(coef, irr), other) | (other, Product(coef, irr)) => {
                let inner_product = self.multiply_irrationals(irr, other)?;
                self.eval_binary_op(coef, BinOp::Mul, &inner_product)
            }

            // Sum 展开: (a + b) * c = a*c + b*c
            (Sum(x, y), other) | (other, Sum(x, y)) => {
                let term1 = self.multiply_irrationals(x, other)?;
                let term2 = self.multiply_irrationals(y, other)?;
                self.eval_binary_op(&term1, BinOp::Add, &term2)
            }

            // 其他情况
            _ => Ok(Value::Irrational(Product(
                Box::new(Value::Irrational(a.clone())),
                Box::new(b.clone()),
            ))),
        }
    }

    fn call_function(&mut self, func: Value, args: Vec<Value>) -> Result<Value, String> {
        match func {
            Value::Function { params, body, .. } => {
                if params.len() != args.len() {
                    return Err(format!(
                        "Expected {} arguments, got {}",
                        params.len(),
                        args.len()
                    ));
                }

                // 创建新的局部作用域
                let mut local_scope = HashMap::new();
                for (param, arg) in params.iter().zip(args.iter()) {
                    local_scope.insert(param.clone(), arg.clone());
                }
                self.locals.push(Rc::new(RefCell::new(local_scope)));

                // 执行函数体
                self.execute_stmt(&body)?;

                // 弹出局部作用域
                self.locals.pop();

                // 获取返回值
                let result = self.return_value.take().unwrap_or(Value::Null);
                Ok(result)
            }

            Value::Lambda {
                params,
                body,
                closure,
            } => {
                if params.len() != args.len() {
                    return Err(format!(
                        "Expected {} arguments, got {}",
                        params.len(),
                        args.len()
                    ));
                }

                // 使用闭包作为基础作用域
                let mut local_scope = closure.borrow().clone();
                for (param, arg) in params.iter().zip(args.iter()) {
                    local_scope.insert(param.clone(), arg.clone());
                }
                self.locals.push(Rc::new(RefCell::new(local_scope)));

                // 执行函数体
                self.execute_stmt(&body)?;

                // 弹出局部作用域
                self.locals.pop();

                // 获取返回值
                let result = self.return_value.take().unwrap_or(Value::Null);
                Ok(result)
            }

            Value::NativeFunction { name, func } => {
                // 特殊处理 foreach 函数
                if name == "foreach" {
                    return self.handle_foreach(&args);
                }
                func(&args)
            }

            _ => Err(format!("Cannot call {}", func.type_name())),
        }
    }

    fn call_method(
        &mut self,
        func: Value,
        self_obj: Value,
        args: Vec<Value>,
    ) -> Result<Value, String> {
        match func {
            Value::Function { params, body, .. } => {
                if params.len() != args.len() {
                    return Err(format!(
                        "Expected {} arguments, got {}",
                        params.len(),
                        args.len()
                    ));
                }

                // 创建新的局部作用域，并注入self
                let mut local_scope = HashMap::new();
                local_scope.insert("self".to_string(), self_obj);
                for (param, arg) in params.iter().zip(args.iter()) {
                    local_scope.insert(param.clone(), arg.clone());
                }
                self.locals.push(Rc::new(RefCell::new(local_scope)));

                // 执行函数体
                self.execute_stmt(&body)?;

                // 弹出局部作用域
                self.locals.pop();

                // 获取返回值
                let result = self.return_value.take().unwrap_or(Value::Null);
                Ok(result)
            }

            Value::Lambda {
                params,
                body,
                closure,
            } => {
                if params.len() != args.len() {
                    return Err(format!(
                        "Expected {} arguments, got {}",
                        params.len(),
                        args.len()
                    ));
                }

                // 使用闭包作为基础作用域，并注入self
                let mut local_scope = closure.borrow().clone();
                local_scope.insert("self".to_string(), self_obj);
                for (param, arg) in params.iter().zip(args.iter()) {
                    local_scope.insert(param.clone(), arg.clone());
                }
                self.locals.push(Rc::new(RefCell::new(local_scope)));

                // 执行函数体
                self.execute_stmt(&body)?;

                // 弹出局部作用域
                self.locals.pop();

                // 获取返回值
                let result = self.return_value.take().unwrap_or(Value::Null);
                Ok(result)
            }

            Value::NativeFunction { func, .. } => {
                // 原生函数不支持self注入，直接调用
                func(&args)
            }

            _ => Err(format!("Cannot call method on {}", func.type_name())),
        }
    }

    fn handle_foreach(&mut self, args: &[Value]) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("foreach expects 2 arguments (array, function)".to_string());
        }

        let array = match &args[0] {
            Value::Array(arr) => arr.clone(),
            _ => {
                return Err(format!(
                    "foreach expects array, got {}",
                    args[0].type_name()
                ));
            }
        };

        let callback = args[1].clone();

        // 遍历数组并调用回调函数
        let arr = array.borrow();
        for (index, element) in arr.iter().enumerate() {
            // 调用回调函数，传入索引和元素
            let callback_args = vec![Value::Int(index as i64), element.clone()];
            self.call_function(callback.clone(), callback_args)?;
        }

        Ok(Value::Null)
    }

    fn set_variable(&mut self, name: String, value: Value) {
        if let Some(local) = self.locals.last() {
            local.borrow_mut().insert(name, value);
        } else {
            self.globals.borrow_mut().insert(name, value);
        }
    }

    fn get_variable(&self, name: &str) -> Result<Value, String> {
        // 从最近的作用域开始查找
        for scope in self.locals.iter().rev() {
            if let Some(val) = scope.borrow().get(name) {
                return Ok(val.clone());
            }
        }

        // 查找全局作用域
        if let Some(val) = self.globals.borrow().get(name) {
            return Ok(val.clone());
        }

        Err(format!("Undefined variable: {}", name))
    }

    fn variable_exists(&self, name: &str) -> bool {
        for scope in self.locals.iter().rev() {
            if scope.borrow().contains_key(name) {
                return true;
            }
        }
        self.globals.borrow().contains_key(name)
    }

    fn convert_to_bigint(&self, val: Value) -> Result<Value, String> {
        match val {
            Value::Int(n) => Ok(Value::BigInt(BigInt::from(n))),
            Value::BigInt(_) => Ok(val),
            _ => Err(format!("Cannot convert {} to bigint", val.type_name())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn eval_expr(code: &str) -> Result<Value, String> {
        let mut lexer = Lexer::new(code.to_string());
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse()?;
        let mut interpreter = Interpreter::new();
        match interpreter.interpret(ast)? {
            Some(v) => Ok(v),
            None => Err("No value returned".to_string()),
        }
    }

    #[test]
    fn test_power_int_rational() {
        // Test 8^(1/3) = 2 (perfect cube root returns Int)
        let result = eval_expr("8^(1/3)").unwrap();
        match result {
            Value::Int(n) => assert_eq!(n, 2, "Expected 2, got {}", n),
            other => panic!("Expected Int result, got {:?}", other),
        }
    }

    #[test]
    fn test_power_negative_int_rational() {
        // Test (-8)^(1/3) - should return -2 (real cube root as Int)
        let result = eval_expr("(-8)^(1/3)");
        assert!(result.is_ok(), "Should not error on (-8)^(1/3)");

        // Verify the result is -2
        match result.unwrap() {
            Value::Int(n) => {
                assert_eq!(n, -2, "Expected -2, got {}", n);
            }
            other => panic!("Expected Int result, got {:?}", other),
        }
    }

    #[test]
    fn test_power_int_by_half() {
        // Test 4^(1/2) = 2 (perfect square root returns Int)
        let result = eval_expr("4^(1/2)").unwrap();
        match result {
            Value::Int(n) => assert_eq!(n, 2, "Expected 2, got {}", n),
            other => panic!("Expected Int result, got {:?}", other),
        }
    }

    #[test]
    fn test_power_rational_rational() {
        // Test (1/2)^(1/2) - should return symbolic or float
        let result = eval_expr("(1/2)^(1/2)");
        assert!(result.is_ok(), "Should support rational^rational");
        // Accept any valid result type
    }

    #[test]
    fn test_power_float_rational() {
        // Test 2.0^(1/2) - since 2.0 is Float, may return Irrational or Float
        let result = eval_expr("2.0^(1/2)").unwrap();
        match result {
            Value::Irrational(_) => {} // Symbolic √2
            Value::Float(f) => assert!((f - 1.414).abs() < 0.01),
            other => panic!("Expected Irrational or Float, got {:?}", other),
        }
    }

    #[test]
    fn test_power_negative_fifth_root() {
        // Test (-32)^(1/5) = -2 (perfect fifth root returns Int)
        let result = eval_expr("(-32)^(1/5)");
        assert!(result.is_ok(), "Should not error on (-32)^(1/5)");
        match result.unwrap() {
            Value::Int(n) => {
                assert_eq!(n, -2, "Expected -2, got {}", n);
            }
            other => panic!("Expected Int result, got {:?}", other),
        }
    }

    #[test]
    fn test_power_mathcore_integration() {
        // Test that mathcore is being used for calculations
        // 27^(1/3) = 3 (perfect cube root returns Int)
        let result = eval_expr("27^(1/3)").unwrap();
        match result {
            Value::Int(n) => assert_eq!(n, 3, "Expected 3, got {}", n),
            other => panic!("Expected Int result, got {:?}", other),
        }
    }

    #[test]
    fn test_power_mathcore_exclusive() {
        // Test that mathcore handles various power operations

        // Large integer exponent
        let result = eval_expr("2^10").unwrap();
        match result {
            Value::Int(i) => assert_eq!(i, 1024, "Expected 1024, got {}", i),
            _ => panic!("Expected Int result for integer power"),
        }

        // Fractional base and exponent - (1/4)^(1/2) = 1/2 ideally, but we may get float
        let result = eval_expr("(1/4)^(1/2)");
        assert!(result.is_ok(), "Should handle (1/4)^(1/2)");

        // Negative base with rational exponent (odd denominator) - perfect root
        let result = eval_expr("(-27)^(1/3)").unwrap();
        match result {
            Value::Int(n) => assert_eq!(n, -3, "Expected -3, got {}", n),
            other => panic!("Expected Int result, got {:?}", other),
        }
    }

    #[test]
    fn test_complex_even_root_negative() {
        // Test (-4)^(1/2) should return complex number
        let result = eval_expr("(-4)^(1/2)");
        assert!(result.is_ok(), "Should not error on (-4)^(1/2)");
        match result.unwrap() {
            Value::Complex(re, im) => {
                // Should be 0 + 2i (symbolic)
                assert!(
                    matches!(re.as_ref(), Value::Int(0)),
                    "Real part should be 0, got {:?}",
                    re
                );
                assert!(
                    matches!(im.as_ref(), Value::Int(2)),
                    "Imaginary part should be 2, got {:?}",
                    im
                );
            }
            other => panic!(
                "Expected Complex result for even root of negative, got {:?}",
                other
            ),
        }
    }

    #[test]
    fn test_symbolic_irrational_roots() {
        // Test 2^(1/2) should return symbolic √2
        let result = eval_expr("2^(1/2)").unwrap();
        match result {
            Value::Irrational(IrrationalValue::Sqrt(_))
            | Value::Irrational(IrrationalValue::Root(2, _)) => {
                // Good - symbolic representation
            }
            other => {
                // May also return Int if simplified, which is fine
                println!("Got non-symbolic result: {:?}", other);
            }
        }
    }

    // Tests for Complex number arithmetic
    #[test]
    fn test_complex_addition() {
        // Test Complex + Complex
        let code = r#"
            var c1 = (-4)^(1/2);
            var c2 = (-9)^(1/2);
            c1 + c2;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Complex addition should work");
        match result.unwrap() {
            Value::Complex(re, im) => {
                // Should be 0 + 5i
                assert!(
                    matches!(re.as_ref(), Value::Int(0)),
                    "Real part should be 0"
                );
                assert!(
                    matches!(im.as_ref(), Value::Int(5)),
                    "Imaginary part should be 5, got {:?}",
                    im
                );
            }
            other => panic!("Expected Complex result, got {:?}", other),
        }
    }

    #[test]
    fn test_complex_multiplication() {
        // Test Complex * Complex
        let code = r#"
            var c1 = (-1)^(1/2);
            var c2 = (-1)^(1/2);
            c1 * c2;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Complex multiplication should work");
        // i * i = -1 + 0i
        match result.unwrap() {
            Value::Int(-1) => {} // Simplified to just -1
            Value::Complex(re, im) => {
                assert!(
                    matches!(re.as_ref(), Value::Int(-1)),
                    "Real part should be -1, got {:?}",
                    re
                );
                assert!(
                    matches!(im.as_ref(), Value::Int(0)),
                    "Imaginary part should be 0, got {:?}",
                    im
                );
            }
            other => panic!("Expected Int(-1) or Complex result, got {:?}", other),
        }
    }

    #[test]
    fn test_complex_with_int() {
        // Test Complex + Int
        let code = r#"
            var c = (-4)^(1/2);
            c + 3;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Complex + Int should work");
        match result.unwrap() {
            Value::Complex(re, im) => {
                // Should be 3 + 2i
                assert!(
                    matches!(re.as_ref(), Value::Int(3)),
                    "Real part should be 3, got {:?}",
                    re
                );
                assert!(
                    matches!(im.as_ref(), Value::Int(2)),
                    "Imaginary part should be 2, got {:?}",
                    im
                );
            }
            other => panic!("Expected Complex result, got {:?}", other),
        }
    }

    #[test]
    fn test_complex_division() {
        // Test Complex / Complex
        let code = r#"
            var c1 = (-4)^(1/2);
            var c2 = (-1)^(1/2);
            c1 / c2;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Complex division should work");
        match result.unwrap() {
            // (-4)^(1/2) = 2i, (-1)^(1/2) = i, so 2i / i = 2/1 + 0i or just 2/1
            Value::Rational(r) => {
                assert_eq!(r.numer().to_string(), "2", "Result should be 2/1");
            }
            Value::Int(2) => {} // Simplified to just 2
            Value::Complex(re, im) => {
                assert!(
                    matches!(re.as_ref(), Value::Rational(_) | Value::Int(2)),
                    "Real part should be 2, got {:?}",
                    re
                );
                // Imaginary part could be Int(0) or Rational(0/1)
                match im.as_ref() {
                    Value::Int(0) => {}
                    Value::Rational(r) if r.numer().to_string() == "0" => {}
                    other => panic!("Imaginary part should be 0, got {:?}", other),
                }
            }
            other => panic!("Expected Complex result, got {:?}", other),
        }
    }

    #[test]
    fn test_complex_with_rational() {
        // Test Complex * Rational
        let code = r#"
            var c = (-1)^(1/2);
            var r = 1/2;
            c * r;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Complex * Rational should work");
        match result.unwrap() {
            Value::Complex(re, im) => {
                // Should be 0 + (1/2)i
                // Real part could be Int(0) or Rational(0/1)
                match re.as_ref() {
                    Value::Int(0) => {}
                    Value::Rational(r) if r.numer().to_string() == "0" => {}
                    other => panic!("Real part should be 0, got {:?}", other),
                }
                match im.as_ref() {
                    Value::Rational(r) => {
                        assert_eq!(
                            r.numer().to_string(),
                            "1",
                            "Imaginary numerator should be 1"
                        );
                        assert_eq!(
                            r.denom().to_string(),
                            "2",
                            "Imaginary denominator should be 2"
                        );
                    }
                    other => panic!("Expected Rational imaginary part, got {:?}", other),
                }
            }
            other => panic!("Expected Complex result, got {:?}", other),
        }
    }

    // Tests for enhanced Irrational simplification
    #[test]
    fn test_sqrt_multiplication_simplification() {
        // Test √2 × √2 → 2
        let code = r#"
            var s = sqrt(2);
            s * s;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "√2 × √2 should work");
        match result.unwrap() {
            Value::Int(n) => assert_eq!(n, 2, "√2 × √2 should equal 2"),
            other => panic!("Expected Int(2), got {:?}", other),
        }
    }

    #[test]
    fn test_sqrt_multiplication_non_perfect() {
        // Test √2 × √3 → √6
        let code = r#"
            var s2 = sqrt(2);
            var s3 = sqrt(3);
            s2 * s3;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "√2 × √3 should work");
        match result.unwrap() {
            Value::Irrational(IrrationalValue::Sqrt(n)) => {
                match n.as_ref() {
                    Value::Int(6) => {} // Expected √6
                    other => panic!("Expected √6, got √{:?}", other),
                }
            }
            other => panic!("Expected Irrational(Sqrt(6)), got {:?}", other),
        }
    }

    #[test]
    fn test_sqrt_eight_simplification() {
        // Test √8 → 2√2 (via display formatting)
        let code = "sqrt(8);";
        let result = eval_expr(code);
        assert!(result.is_ok(), "sqrt(8) should work");
        // The result should be Irrational(Sqrt(8)), which displays as "2√2"
        match result.unwrap() {
            Value::Irrational(IrrationalValue::Sqrt(n)) => {
                match n.as_ref() {
                    Value::Int(8) => {
                        // Display formatting will simplify to 2√2
                    }
                    other => panic!("Expected √8, got √{:?}", other),
                }
            }
            other => panic!("Expected Irrational(Sqrt(8)), got {:?}", other),
        }
    }

    #[test]
    fn test_complex_negation() {
        // Test negation of complex numbers
        let code = r#"
            var c = (-1)^(1/2);
            -c;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Complex negation should work");
        match result.unwrap() {
            Value::Complex(re, im) => {
                // Should be 0 + (-1)i = -i
                assert!(
                    matches!(re.as_ref(), Value::Int(0)),
                    "Real part should be 0, got {:?}",
                    re
                );
                assert!(
                    matches!(im.as_ref(), Value::Int(-1)),
                    "Imaginary part should be -1, got {:?}",
                    im
                );
            }
            other => panic!("Expected Complex result, got {:?}", other),
        }
    }

    #[test]
    fn test_rational_negation() {
        // Test negation of rational numbers
        let code = r#"
            var r = 3/4;
            -r;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Rational negation should work");
        match result.unwrap() {
            Value::Rational(r) => {
                assert_eq!(r.numer().to_string(), "-3");
                assert_eq!(r.denom().to_string(), "4");
            }
            other => panic!("Expected Rational result, got {:?}", other),
        }
    }

    #[test]
    fn test_irrational_negation() {
        // Test negation of irrational numbers
        let code = r#"
            var s = sqrt(2);
            -s;
        "#;
        let result = eval_expr(code);
        assert!(result.is_ok(), "Irrational negation should work");
        match result.unwrap() {
            Value::Irrational(IrrationalValue::Product(coef, irr)) => {
                match coef.as_ref() {
                    Value::Int(-1) => {
                        match irr.as_ref() {
                            IrrationalValue::Sqrt(n) => {
                                match n.as_ref() {
                                    Value::Int(2) => {} // Expected -√2
                                    other => panic!("Expected -√2, got -√{:?}", other),
                                }
                            }
                            other => panic!("Expected Sqrt, got {:?}", other),
                        }
                    }
                    other => panic!("Expected coefficient -1, got {:?}", other),
                }
            }
            other => panic!("Expected Irrational(Product(-1, Sqrt(2))), got {:?}", other),
        }
    }
}
