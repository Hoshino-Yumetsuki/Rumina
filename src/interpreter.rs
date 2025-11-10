use num::BigInt;
use num::BigRational;
use num::One;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::rc::Rc;

use crate::ast::*;
use crate::builtin;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::value::*;

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
                _ => Err(format!("Unsupported operation: irrational {} irrational", op)),
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

            // √a * √b = √(a*b)
            (Sqrt(x), Sqrt(y)) => {
                let product = self.eval_binary_op(x, BinOp::Mul, y)?;
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
