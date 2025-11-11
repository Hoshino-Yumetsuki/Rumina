/// Statement execution implementation
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::rc::Rc;

use crate::ast::Stmt;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::value::Value;

use super::Interpreter;
use super::convert;

impl Interpreter {
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
}
