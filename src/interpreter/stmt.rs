/// Statement execution implementation
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::rc::Rc;

use crate::ast::{Expr, Stmt};
use crate::builtin;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::value::{IrrationalValue, Value};

use super::Interpreter;
use super::convert;

impl Interpreter {
    fn lsr002_constants_module() -> HashMap<String, Value> {
        HashMap::from([
            ("EARTH_GRAVITY".to_string(), Value::Float(9.80665)),
            ("MOON_GRAVITY".to_string(), Value::Float(1.625)),
            ("MARS_GRAVITY".to_string(), Value::Float(3.72076)),
            ("WATER_DENSITY".to_string(), Value::Float(1000.0)),
            ("STANDARD_PRESSURE".to_string(), Value::Float(101325.0)),
            ("STANDARD_TEMPERATURE".to_string(), Value::Float(273.15)),
            ("AIR_DENSITY".to_string(), Value::Float(1.225)),
            ("C".to_string(), Value::Float(2.99792458e8)),
            ("G".to_string(), Value::Float(6.67430e-11)),
            ("H".to_string(), Value::Float(6.62607015e-34)),
            ("KB".to_string(), Value::Float(1.380649e-23)),
            ("EPSILON_0".to_string(), Value::Float(8.8541878128e-12)),
            ("MU_0".to_string(), Value::Float(1.25663706212e-6)),
            ("AVOGADRO".to_string(), Value::Float(6.02214076e23)),
            ("R".to_string(), Value::Float(8.314462618)),
            ("FARADAY".to_string(), Value::Float(9.648533212e4)),
            ("AMU".to_string(), Value::Float(1.66053906660e-27)),
            ("MOLAR_VOLUME_IDEAL".to_string(), Value::Float(0.024465)),
            ("ROOM_PRESSURE".to_string(), Value::Float(1.0e5)),
            ("ROOM_TEMPERATURE".to_string(), Value::Float(297.15)),
        ])
    }

    fn native_fn(name: &str, func: fn(&[Value]) -> Result<Value, String>) -> Value {
        Value::NativeFunction {
            name: name.to_string(),
            func,
        }
    }

    fn lsr004_math_module() -> HashMap<String, Value> {
        HashMap::from([
            ("abs".to_string(), Self::native_fn("std.math.abs", builtin::math::abs_fn)),
            ("sqrt".to_string(), Self::native_fn("std.math.sqrt", builtin::math::sqrt)),
            ("pow".to_string(), Self::native_fn("std.math.pow", builtin::math::pow)),
            ("exp".to_string(), Self::native_fn("std.math.exp", builtin::math::exp)),
            ("log".to_string(), Self::native_fn("std.math.log", builtin::math::std_log)),
            ("log10".to_string(), Self::native_fn("std.math.log10", builtin::math::log10)),
            ("sin".to_string(), Self::native_fn("std.math.sin", builtin::math::sin)),
            ("cos".to_string(), Self::native_fn("std.math.cos", builtin::math::cos)),
            ("tan".to_string(), Self::native_fn("std.math.tan", builtin::math::tan)),
            ("asin".to_string(), Self::native_fn("std.math.asin", builtin::math::asin)),
            ("acos".to_string(), Self::native_fn("std.math.acos", builtin::math::acos)),
            ("atan".to_string(), Self::native_fn("std.math.atan", builtin::math::atan)),
            ("floor".to_string(), Self::native_fn("std.math.floor", builtin::math::floor)),
            ("ceil".to_string(), Self::native_fn("std.math.ceil", builtin::math::ceil)),
            ("round".to_string(), Self::native_fn("std.math.round", builtin::math::round)),
            ("clamp".to_string(), Self::native_fn("std.math.clamp", builtin::math::clamp)),
            ("pi".to_string(), Value::Irrational(IrrationalValue::Pi)),
            ("e".to_string(), Value::Irrational(IrrationalValue::E)),
            ("phi".to_string(), Value::Float(1.618033988749895)),
        ])
    }

    fn lsr004_random_module() -> HashMap<String, Value> {
        HashMap::from([
            ("seed".to_string(), Self::native_fn("std.random.seed", builtin::random::seed)),
            ("rand".to_string(), Self::native_fn("std.random.rand", builtin::random::rand)),
            ("randint".to_string(), Self::native_fn("std.random.randint", builtin::random::randint)),
            ("normal".to_string(), Self::native_fn("std.random.normal", builtin::random::normal)),
            ("choice".to_string(), Self::native_fn("std.random.choice", builtin::random::choice)),
        ])
    }

    fn lsr004_stats_module() -> HashMap<String, Value> {
        HashMap::from([
            ("mean".to_string(), Self::native_fn("std.stats.mean", builtin::stats::mean)),
            ("median".to_string(), Self::native_fn("std.stats.median", builtin::stats::median)),
            ("var".to_string(), Self::native_fn("std.stats.var", builtin::stats::var)),
            ("std".to_string(), Self::native_fn("std.stats.std", builtin::stats::std)),
            ("quantile".to_string(), Self::native_fn("std.stats.quantile", builtin::stats::quantile)),
            ("cov".to_string(), Self::native_fn("std.stats.cov", builtin::stats::cov)),
            ("corr".to_string(), Self::native_fn("std.stats.corr", builtin::stats::corr)),
        ])
    }

    fn lsr004_units_module() -> HashMap<String, Value> {
        HashMap::from([
            ("convert".to_string(), Self::native_fn("std.units.convert", builtin::units::convert)),
            ("strip".to_string(), Self::native_fn("std.units.strip", builtin::units::strip)),
            (
                "is_dimensionless".to_string(),
                Self::native_fn("std.units.is_dimensionless", builtin::units::is_dimensionless),
            ),
        ])
    }

    fn lsr_standard_module(path: &[String]) -> Result<Value, String> {
        match path.join(".").as_str() {
            "std.constants" => Ok(Value::Module(Rc::new(RefCell::new(
                Self::lsr002_constants_module(),
            )))),
            "std.math" => Ok(Value::Module(Rc::new(RefCell::new(Self::lsr004_math_module())))),
            "std.random" => Ok(Value::Module(Rc::new(RefCell::new(
                Self::lsr004_random_module(),
            )))),
            "std.stats" => Ok(Value::Module(Rc::new(RefCell::new(
                Self::lsr004_stats_module(),
            )))),
            "std.units" => Ok(Value::Module(Rc::new(RefCell::new(
                Self::lsr004_units_module(),
            )))),
            module => Err(format!("Unknown module: {}", module)),
        }
    }

    pub(super) fn execute_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::VarDecl {
                name,
                is_bigint,
                declared_type,
                value,
            } => {
                let val = self.eval_expr(value)?;

                // LSR-005: Apply type conversion if declared_type is specified
                let val = if let Some(dtype) = declared_type {
                    convert::convert_to_declared_type(val, dtype)?
                } else if *is_bigint {
                    // Backward compatibility
                    convert::convert_to_bigint(val)?
                } else {
                    val
                };

                self.set_variable(name.clone(), val, false);
                Ok(())
            }

            Stmt::LetDecl {
                name,
                is_bigint,
                declared_type,
                value,
            } => {
                let val = self.eval_expr(value)?;

                let val = if let Some(dtype) = declared_type {
                    convert::convert_to_declared_type(val, dtype)?
                } else if *is_bigint {
                    convert::convert_to_bigint(val)?
                } else {
                    val
                };

                self.set_variable(name.clone(), val, true);
                Ok(())
            }

            Stmt::Assign { name, value } => {
                let val = self.eval_expr(value)?;
                self.assign_variable(name.clone(), val)
            }

            Stmt::MemberAssign {
                object,
                member,
                value,
            } => {
                let val = self.eval_expr(value)?;

                // Special handling when object is an identifier (variable)
                if let Expr::Ident(var_name) = object {
                    if self.is_immutable_binding(var_name) {
                        return Err(format!(
                            "Cannot assign to immutable variable '{}'",
                            var_name
                        ));
                    }

                    let obj = self.eval_expr(object)?;

                    match obj {
                        Value::Struct(s) => {
                            s.borrow_mut().insert(member.clone(), val);
                            Ok(())
                        }
                        Value::Null => {
                            // Auto-vivify: Convert null to empty struct
                            if self.is_immutable_binding(var_name) {
                                return Err(format!(
                                    "Cannot assign to immutable variable '{}'",
                                    var_name
                                ));
                            }
                            let new_struct = Rc::new(RefCell::new(HashMap::default()));
                            new_struct.borrow_mut().insert(member.clone(), val);
                            self.assign_variable(var_name.clone(), Value::Struct(new_struct))?;
                            Ok(())
                        }
                        _ => Err(format!("Cannot assign member to {}", obj.type_name())),
                    }
                } else {
                    // For non-identifier expressions, evaluate and check
                    let obj = self.eval_expr(object)?;
                    match obj {
                        Value::Struct(s) => {
                            s.borrow_mut().insert(member.clone(), val);
                            Ok(())
                        }
                        _ => Err(format!("Cannot assign member to {}", obj.type_name())),
                    }
                }
            }

            Stmt::Expr(expr) => {
                self.eval_expr(expr)?;
                Ok(())
            }

            Stmt::FuncDef {
                name,
                params,
                body,
                decorators,
            } => {
                let mut func = Value::Function {
                    name: name.clone(),
                    params: params.clone(),
                    body: Box::new(Stmt::Block(body.clone())),
                    decorators: decorators.clone(),
                };

                // LSR-011: Apply decorators
                for decorator in decorators {
                    func = self.apply_decorator(decorator, func)?;
                }

                self.set_variable(name.clone(), func, false);
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

            Stmt::For {
                init,
                condition,
                update,
                body,
            } => {
                // 执行初始化语句（如果有）
                if let Some(init_stmt) = init {
                    self.execute_stmt(init_stmt)?;
                }

                // 循环
                loop {
                    // 检查条件（如果有）
                    if let Some(cond) = condition {
                        let cond_val = self.eval_expr(cond)?;
                        if !cond_val.is_truthy() {
                            break;
                        }
                    }

                    // 执行循环体
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

                    // 如果有return或break，退出循环
                    if self.return_value.is_some() || self.break_flag {
                        self.break_flag = false;
                        break;
                    }

                    // 执行更新语句（如果有）
                    if let Some(upd) = update {
                        self.execute_stmt(upd)?;
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
                // Handle built-in virtual modules
                if path == "rumina:fs" {
                    let module = self.globals.borrow().get("rumina:fs").cloned();
                    if let Some(module) = module {
                        self.globals.borrow_mut().insert("fs".to_string(), module);
                        return Ok(());
                    }
                    return Err("Built-in module 'rumina:fs' is not registered".to_string());
                }

                if path == "rumina:path" {
                    let module = self.globals.borrow().get("rumina:path").cloned();
                    if let Some(module) = module {
                        self.globals.borrow_mut().insert("path".to_string(), module);
                        return Ok(());
                    }
                    return Err("Built-in module 'rumina:path' is not registered".to_string());
                }

                if path == "rumina:env" {
                    let module = self.globals.borrow().get("rumina:env").cloned();
                    if let Some(module) = module {
                        self.globals.borrow_mut().insert("env".to_string(), module);
                        return Ok(());
                    }
                    return Err("Built-in module 'rumina:env' is not registered".to_string());
                }

                if path == "rumina:process" {
                    let module = self.globals.borrow().get("rumina:process").cloned();
                    if let Some(module) = module {
                        self.globals
                            .borrow_mut()
                            .insert("process".to_string(), module);
                        return Ok(());
                    }
                    return Err("Built-in module 'rumina:process' is not registered".to_string());
                }

                if path == "rumina:time" {
                    let module = self.globals.borrow().get("rumina:time").cloned();
                    if let Some(module) = module {
                        self.globals.borrow_mut().insert("time".to_string(), module);
                        return Ok(());
                    }
                    return Err("Built-in module 'rumina:time' is not registered".to_string());
                }

                if path == "rumina:stream" {
                    let module = self.globals.borrow().get("rumina:stream").cloned();
                    if let Some(module) = module {
                        self.globals
                            .borrow_mut()
                            .insert("stream".to_string(), module);
                        return Ok(());
                    }
                    return Err("Built-in module 'rumina:stream' is not registered".to_string());
                }

                if path == "rumina:buffer" {
                    let module = self.globals.borrow().get("rumina:buffer").cloned();
                    if let Some(module) = module {
                        self.globals
                            .borrow_mut()
                            .insert("Buffer".to_string(), module);
                        return Ok(());
                    }
                    return Err("Built-in module 'rumina:buffer' is not registered".to_string());
                }

                if path.starts_with("rumina:") {
                    return Err(format!("Unknown built-in module '{}'", path));
                }

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
                } else if let Some(stripped_path) = file_path.strip_prefix("./") {
                    // 如果以 ./ 开头，尝试在 examples 目录中查找
                    let examples_path = format!("examples/{}", stripped_path);
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
                            .next_back()
                            .or_else(|| path.split('\\').next_back())
                            .unwrap_or(path)
                            .trim_end_matches(".lm")
                            .to_string()
                    }
                } else {
                    // 空文件，使用文件名
                    path.split('/')
                        .next_back()
                        .or_else(|| path.split('\\').next_back())
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
                self.immutable_locals.push(std::collections::HashSet::new());

                // 执行模块代码
                for stmt in statements {
                    self.execute_stmt(&stmt)?;
                }

                // 弹出模块作用域
                self.locals.pop();
                if self.immutable_locals.pop().is_none() {
                    return Err("Internal error: immutable scope stack underflow".to_string());
                }

                // 将模块注册为全局变量
                self.globals
                    .borrow_mut()
                    .insert(module_name, Value::Module(module_scope));

                Ok(())
            }

            Stmt::Import { path, alias } => {
                let module = Self::lsr_standard_module(path)?;
                let binding = alias
                    .clone()
                    .or_else(|| path.last().cloned())
                    .ok_or_else(|| "Import path cannot be empty".to_string())?;
                self.set_variable(binding, module, true);
                Ok(())
            }

            Stmt::Use { path, items } => {
                let module = Self::lsr_standard_module(path)?;
                let Value::Module(module_values) = module else {
                    return Err(format!("{} is not a module", path.join(".")));
                };
                let module_values = module_values.borrow();

                for (name, alias) in items {
                    let value = module_values.get(name).cloned().ok_or_else(|| {
                        format!("Module {} has no symbol {}", path.join("."), name)
                    })?;
                    self.set_variable(alias.clone().unwrap_or_else(|| name.clone()), value, true);
                }

                Ok(())
            }

            Stmt::TryCatch(try_block, error_name, catch_block) => {
                match self.execute_stmt(try_block) {
                    Ok(()) => Ok(()),
                    Err(err) => {
                        let catch_value = if Self::is_result_propagation(&err) {
                            self.take_propagated_result()
                                .unwrap_or(Value::String("Unknown propagated error".to_string()))
                        } else {
                            Value::String(err)
                        };

                        let mut scope = HashMap::new();
                        scope.insert(error_name.clone(), catch_value);
                        self.locals.push(Rc::new(RefCell::new(scope)));
                        self.immutable_locals.push(std::collections::HashSet::new());

                        let result = self.execute_stmt(catch_block);

                        self.locals.pop();
                        if self.immutable_locals.pop().is_none() {
                            return Err(
                                "Internal error: immutable scope stack underflow".to_string()
                            );
                        }

                        result
                    }
                }
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
            Stmt::Empty => Ok(()),
        }
    }
}
