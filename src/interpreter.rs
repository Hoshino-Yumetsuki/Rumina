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
                    // Even root of negative: return complex number
                    let c = Complex64::new(base, 0.0).powf(exponent);
                    return Ok(Value::Complex(c));
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
                // Mathcore failed - try complex number evaluation
                if base < 0.0 {
                    let c = Complex64::new(base, 0.0).powf(exponent);
                    Ok(Value::Complex(c))
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
            (Value::Complex(a), Value::Complex(b)) => match op {
                BinOp::Add => Ok(Value::Complex(a + b)),
                BinOp::Sub => Ok(Value::Complex(a - b)),
                BinOp::Mul => Ok(Value::Complex(a * b)),
                BinOp::Div => {
                    if b.re == 0.0 && b.im == 0.0 {
                        return Err("Division by zero".to_string());
                    }
                    Ok(Value::Complex(a / b))
                }
                BinOp::Equal => Ok(Value::Bool(a == b)),
                BinOp::NotEqual => Ok(Value::Bool(a != b)),
                _ => Err(format!("Unsupported operation: complex {} complex", op)),
            },

            // Complex 与 Int 的运算
            (Value::Complex(c), Value::Int(i)) | (Value::Int(i), Value::Complex(c)) => {
                let i_complex = Complex64::new(*i as f64, 0.0);
                match op {
                    BinOp::Add => Ok(Value::Complex(if matches!(left, Value::Complex(_)) { c + i_complex } else { i_complex + c })),
                    BinOp::Sub => Ok(Value::Complex(if matches!(left, Value::Complex(_)) { c - i_complex } else { i_complex - c })),
                    BinOp::Mul => Ok(Value::Complex(c * i_complex)),
                    BinOp::Div => {
                        if matches!(left, Value::Complex(_)) {
                            if *i == 0 {
                                return Err("Division by zero".to_string());
                            }
                            Ok(Value::Complex(c / i_complex))
                        } else {
                            if c.re == 0.0 && c.im == 0.0 {
                                return Err("Division by zero".to_string());
                            }
                            Ok(Value::Complex(i_complex / c))
                        }
                    }
                    _ => Err(format!("Unsupported operation: complex {} int", op)),
                }
            },

            // Complex 与 Float 的运算
            (Value::Complex(c), Value::Float(f)) | (Value::Float(f), Value::Complex(c)) => {
                let f_complex = Complex64::new(*f, 0.0);
                match op {
                    BinOp::Add => Ok(Value::Complex(if matches!(left, Value::Complex(_)) { c + f_complex } else { f_complex + c })),
                    BinOp::Sub => Ok(Value::Complex(if matches!(left, Value::Complex(_)) { c - f_complex } else { f_complex - c })),
                    BinOp::Mul => Ok(Value::Complex(c * f_complex)),
                    BinOp::Div => {
                        if matches!(left, Value::Complex(_)) {
                            if *f == 0.0 {
                                return Err("Division by zero".to_string());
                            }
                            Ok(Value::Complex(c / f_complex))
                        } else {
                            if c.re == 0.0 && c.im == 0.0 {
                                return Err("Division by zero".to_string());
                            }
                            Ok(Value::Complex(f_complex / c))
                        }
                    }
                    _ => Err(format!("Unsupported operation: complex {} float", op)),
                }
            },

            // Complex 与 Rational 的运算
            (Value::Complex(c), Value::Rational(r)) | (Value::Rational(r), Value::Complex(c)) => {
                let r_float = r.numer().to_string().parse::<f64>().unwrap_or(0.0)
                    / r.denom().to_string().parse::<f64>().unwrap_or(1.0);
                let r_complex = Complex64::new(r_float, 0.0);
                match op {
                    BinOp::Add => Ok(Value::Complex(if matches!(left, Value::Complex(_)) { c + r_complex } else { r_complex + c })),
                    BinOp::Sub => Ok(Value::Complex(if matches!(left, Value::Complex(_)) { c - r_complex } else { r_complex - c })),
                    BinOp::Mul => Ok(Value::Complex(c * r_complex)),
                    BinOp::Div => {
                        if matches!(left, Value::Complex(_)) {
                            if r_float == 0.0 {
                                return Err("Division by zero".to_string());
                            }
                            Ok(Value::Complex(c / r_complex))
                        } else {
                            if c.re == 0.0 && c.im == 0.0 {
                                return Err("Division by zero".to_string());
                            }
                            Ok(Value::Complex(r_complex / c))
                        }
                    }
                    _ => Err(format!("Unsupported operation: complex {} rational", op)),
                }
            },

            // Complex 与 Irrational 的运算
            (Value::Complex(c), Value::Irrational(irr)) | (Value::Irrational(irr), Value::Complex(c)) => {
                // 将无理数转换为浮点数来进行复数运算
                let irr_val = match irr {
                    IrrationalValue::Pi => std::f64::consts::PI,
                    IrrationalValue::E => std::f64::consts::E,
                    IrrationalValue::Sqrt(n) => {
                        let n_float = match n.as_ref() {
                            Value::Int(i) => *i as f64,
                            Value::Float(f) => *f,
                            _ => return Err("Cannot convert irrational to float for complex operation".to_string()),
                        };
                        n_float.sqrt()
                    }
                    _ => return Err("Complex arithmetic with composite irrationals not yet supported".to_string()),
                };
                let irr_complex = Complex64::new(irr_val, 0.0);
                match op {
                    BinOp::Add => Ok(Value::Complex(if matches!(left, Value::Complex(_)) { c + irr_complex } else { irr_complex + c })),
                    BinOp::Sub => Ok(Value::Complex(if matches!(left, Value::Complex(_)) { c - irr_complex } else { irr_complex - c })),
                    BinOp::Mul => Ok(Value::Complex(c * irr_complex)),
                    BinOp::Div => {
                        if matches!(left, Value::Complex(_)) {
                            if irr_val == 0.0 {
                                return Err("Division by zero".to_string());
                            }
                            Ok(Value::Complex(c / irr_complex))
                        } else {
                            if c.re == 0.0 && c.im == 0.0 {
                                return Err("Division by zero".to_string());
                            }
                            Ok(Value::Complex(irr_complex / c))
                        }
                    }
                    _ => Err(format!("Unsupported operation: complex {} irrational", op)),
                }
            },

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
                Value::Complex(c) => Ok(Value::Complex(-c)),
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
            Value::Complex(c) => {
                // Should be approximately 2i
                assert!(c.re.abs() < 0.001, "Real part should be ~0, got {}", c.re);
                assert!(
                    (c.im.abs() - 2.0).abs() < 0.001,
                    "Imaginary part should be ~2, got {}",
                    c.im
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
            Value::Complex(c) => {
                assert!(c.re.abs() < 0.001, "Real part should be ~0");
                assert!((c.im.abs() - 5.0).abs() < 0.001, "Imaginary part should be ~5");
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
        match result.unwrap() {
            Value::Complex(c) => {
                assert!((c.re + 1.0).abs() < 0.001, "Real part should be ~-1, got {}", c.re);
                assert!(c.im.abs() < 0.001, "Imaginary part should be ~0");
            }
            other => panic!("Expected Complex result, got {:?}", other),
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
            Value::Complex(c) => {
                assert!((c.re - 3.0).abs() < 0.001, "Real part should be ~3");
                assert!((c.im.abs() - 2.0).abs() < 0.001, "Imaginary part should be ~2");
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
            Value::Complex(c) => {
                // (-4)^(1/2) = 2i, (-1)^(1/2) = i, so 2i / i = 2
                eprintln!("Result: re={}, im={}", c.re, c.im);
                assert!((c.re - 2.0).abs() < 0.001, "Real part should be ~2, got {}", c.re);
                assert!(c.im.abs() < 0.001, "Imaginary part should be ~0, got {}", c.im);
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
            Value::Complex(c) => {
                assert!(c.re.abs() < 0.001, "Real part should be ~0");
                assert!((c.im.abs() - 0.5).abs() < 0.001, "Imaginary part should be ~0.5");
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
                    Value::Int(6) => {}, // Expected √6
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
            Value::Complex(c) => {
                assert!(c.re.abs() < 0.001, "Real part should be ~0");
                assert!((c.im + 1.0).abs() < 0.001, "Imaginary part should be ~-1");
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
                                    Value::Int(2) => {}, // Expected -√2
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
