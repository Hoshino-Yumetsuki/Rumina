/// Expression evaluation implementation
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::Expr;
use crate::value::Value;

use super::Interpreter;

impl Interpreter {
    fn try_call_special_method(
        &mut self,
        obj: Value,
        member: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, String>> {
        if let Value::Array(_) = &obj {
            return Some(self.call_array_method(obj, member, args));
        }

        if member == "curried" {
            if !args.is_empty() {
                return Some(Err("curried() does not take arguments".to_string()));
            }

            let curried = match &obj {
                Value::Function { params, .. } | Value::Lambda { params, .. } => {
                    Value::CurriedFunction {
                        original: Box::new(obj.clone()),
                        collected_args: Vec::new(),
                        total_params: params.len(),
                    }
                }
                Value::NativeFunction { .. } => {
                    return Some(Err("Cannot curry native functions".to_string()));
                }
                _ => {
                    return Some(Err(format!(
                        "Type {} does not have method 'curried'",
                        obj.type_name()
                    )));
                }
            };

            return Some(Ok(curried));
        }

        None
    }

    fn eval_pipe_expr(&mut self, left: &Expr, right: &Expr) -> Result<Value, String> {
        let left_value = self.eval_expr(left)?;

        match right {
            Expr::Call { func, args } => {
                if let Expr::Member { object, member } = &**func {
                    let obj = self.eval_expr(object)?;

                    let mut arg_vals = Vec::with_capacity(args.len() + 1);
                    arg_vals.push(left_value.clone());
                    for arg in args {
                        arg_vals.push(self.eval_expr(arg)?);
                    }

                    if let Value::Array(_) = &obj {
                        return self.call_array_method(obj, member, arg_vals);
                    }

                    if member == "curried" {
                        let curried =
                            match self.try_call_special_method(obj.clone(), member, Vec::new()) {
                                Some(Ok(value)) => value,
                                Some(Err(err)) => return Err(err),
                                None => {
                                    unreachable!("curried should be handled as a special method")
                                }
                            };

                        return self.call_function(curried, vec![left_value]);
                    }

                    let method = match &obj {
                        Value::Struct(s) | Value::Module(s) => {
                            let s = s.borrow();
                            s.get(member).cloned().ok_or_else(|| {
                                format!("{} does not have member '{}'", obj.type_name(), member)
                            })?
                        }
                        _ => return Err(format!("Cannot access member of {}", obj.type_name())),
                    };

                    self.call_method(method, obj, arg_vals)
                } else {
                    let func_val = self.eval_expr(func)?;
                    let mut arg_vals = Vec::with_capacity(args.len() + 1);
                    arg_vals.push(left_value);
                    for arg in args {
                        arg_vals.push(self.eval_expr(arg)?);
                    }
                    self.call_function(func_val, arg_vals)
                }
            }
            _ => {
                let func_val = self.eval_expr(right)?;
                self.call_function(func_val, vec![left_value])
            }
        }
    }

    pub(super) fn eval_expr(&mut self, expr: &Expr) -> Result<Value, String> {
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
                if *op == crate::ast::BinOp::Pipe {
                    self.eval_pipe_expr(left, right)
                } else {
                    let l = self.eval_expr(left)?;
                    let r = self.eval_expr(right)?;
                    self.eval_binary_op(&l, *op, &r)
                }
            }

            Expr::Multi(values) => {
                let mut evaluated = Vec::with_capacity(values.len());
                for value in values {
                    evaluated.push(self.eval_expr(value)?);
                }
                Ok(Value::normalize_multi(evaluated))
            }

            Expr::Unary { op, expr } => {
                let val = self.eval_expr(expr)?;
                self.eval_unary_op(*op, &val)
            }

            Expr::Call { func, args } => {
                // 检查是否是成员调用 (obj.method())
                if let Expr::Member { object, member } = &**func {
                    let obj = self.eval_expr(object)?;

                    let arg_vals: Result<Vec<_>, _> =
                        args.iter().map(|a| self.eval_expr(a)).collect();
                    let arg_vals = arg_vals?;

                    if let Some(result) =
                        self.try_call_special_method(obj.clone(), member, arg_vals.clone())
                    {
                        return result;
                    }

                    // 获取方法
                    let method = match &obj {
                        Value::Struct(s) | Value::Module(s) => {
                            let s = s.borrow();
                            s.get(member).cloned().ok_or_else(|| {
                                format!("{} does not have member '{}'", obj.type_name(), member)
                            })?
                        }
                        _ => return Err(format!("Cannot access member of {}", obj.type_name())),
                    };

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
                match &obj {
                    Value::Struct(s) | Value::Module(s) => {
                        let s = s.borrow();
                        s.get(member).cloned().ok_or_else(|| {
                            format!("{} does not have member '{}'", obj.type_name(), member)
                        })
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

            Expr::Try(expr) => {
                let value = self.eval_expr(expr)?;
                match value {
                    Value::Result { ok: true, value } => Ok(*value),
                    Value::Result { ok: false, value } => self.propagate_result(*value),
                    other => Err(format!(
                        "? operator expects result, got {}",
                        other.type_name()
                    )),
                }
            }
        }
    }
}
