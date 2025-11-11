/// Function call implementations
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::value::Value;

use super::Interpreter;

impl Interpreter {
    pub(super) fn call_function(&mut self, func: Value, args: Vec<Value>) -> Result<Value, String> {
        match func {
            Value::MemoizedFunction { original, cache } => {
                // Create cache key from arguments
                let cache_key = args
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(",");

                // Check cache
                if let Some(cached_result) = cache.borrow().get(&cache_key) {
                    return Ok(cached_result.clone());
                }

                // Call original function
                let result = self.call_function(*original, args)?;

                // Store in cache
                cache.borrow_mut().insert(cache_key, result.clone());

                Ok(result)
            }

            Value::CurriedFunction {
                original,
                mut collected_args,
                total_params,
            } => {
                // 收集新的参数
                collected_args.extend(args);

                // 检查是否已收集到所有参数
                if collected_args.len() >= total_params {
                    // 如果参数过多,报错
                    if collected_args.len() > total_params {
                        return Err(format!(
                            "Too many arguments: expected {}, got {}",
                            total_params,
                            collected_args.len()
                        ));
                    }
                    // 调用原始函数
                    self.call_function(*original, collected_args)
                } else {
                    // 还需要更多参数,返回新的柯里化函数
                    Ok(Value::CurriedFunction {
                        original,
                        collected_args,
                        total_params,
                    })
                }
            }

            Value::Function {
                name, params, body, ..
            } => {
                if params.len() != args.len() {
                    return Err(format!(
                        "Expected {} arguments, got {}",
                        params.len(),
                        args.len()
                    ));
                }

                // Check recursion depth to prevent stack overflow
                self.recursion_depth += 1;
                if self.recursion_depth > self.max_recursion_depth {
                    self.recursion_depth -= 1;
                    return Err(format!(
                        "Maximum recursion depth exceeded ({}). Consider using memoization with @memoize decorator or iterative approach.",
                        self.max_recursion_depth
                    ));
                }

                // Push function name onto call stack
                self.call_stack.push(name.clone());

                // 创建新的局部作用域
                let mut local_scope = HashMap::new();
                for (param, arg) in params.iter().zip(args.iter()) {
                    local_scope.insert(param.clone(), arg.clone());
                }
                self.locals.push(Rc::new(RefCell::new(local_scope)));

                // 执行函数体
                let exec_result = self.execute_stmt(&body);

                // 弹出局部作用域
                self.locals.pop();

                // Pop function name from call stack
                self.call_stack.pop();

                // Decrement recursion depth
                self.recursion_depth -= 1;

                // Check execution result
                exec_result?;

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

                // Check recursion depth to prevent stack overflow
                self.recursion_depth += 1;
                if self.recursion_depth > self.max_recursion_depth {
                    self.recursion_depth -= 1;
                    return Err(format!(
                        "Maximum recursion depth exceeded ({}). Consider using memoization or iterative approach.",
                        self.max_recursion_depth
                    ));
                }

                // Push lambda marker onto call stack
                self.call_stack.push("<lambda>".to_string());

                // 使用闭包作为基础作用域
                let mut local_scope = closure.borrow().clone();
                for (param, arg) in params.iter().zip(args.iter()) {
                    local_scope.insert(param.clone(), arg.clone());
                }
                self.locals.push(Rc::new(RefCell::new(local_scope)));

                // 执行函数体
                let exec_result = self.execute_stmt(&body);

                // 弹出局部作用域
                self.locals.pop();

                // Pop lambda from call stack
                self.call_stack.pop();

                // Decrement recursion depth
                self.recursion_depth -= 1;

                // Check execution result
                exec_result?;

                // 获取返回值
                let result = self.return_value.take().unwrap_or(Value::Null);
                Ok(result)
            }

            Value::NativeFunction { name, func } => {
                // 特殊处理需要调用lambda的高阶函数
                match name.as_str() {
                    "foreach" => return self.handle_foreach(&args),
                    "map" => return self.handle_map(&args),
                    "filter" => return self.handle_filter(&args),
                    "reduce" => return self.handle_reduce(&args),
                    _ => {}
                }
                func(&args)
            }

            _ => Err(format!("Cannot call {}", func.type_name())),
        }
    }

    pub(super) fn call_method(
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

    pub(super) fn handle_foreach(&mut self, args: &[Value]) -> Result<Value, String> {
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

    pub(super) fn handle_map(&mut self, args: &[Value]) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("map expects 2 arguments (array, function)".to_string());
        }

        let array = match &args[0] {
            Value::Array(arr) => arr.clone(),
            _ => {
                return Err(format!("map expects array, got {}", args[0].type_name()));
            }
        };

        let callback = args[1].clone();

        // 映射数组元素
        let arr = array.borrow();
        let mut result = Vec::new();
        for element in arr.iter() {
            let callback_args = vec![element.clone()];
            let mapped_value = self.call_function(callback.clone(), callback_args)?;
            result.push(mapped_value);
        }

        Ok(Value::Array(Rc::new(RefCell::new(result))))
    }

    pub(super) fn handle_filter(&mut self, args: &[Value]) -> Result<Value, String> {
        if args.len() != 2 {
            return Err("filter expects 2 arguments (array, function)".to_string());
        }

        let array = match &args[0] {
            Value::Array(arr) => arr.clone(),
            _ => {
                return Err(format!("filter expects array, got {}", args[0].type_name()));
            }
        };

        let callback = args[1].clone();

        // 过滤数组元素
        let arr = array.borrow();
        let mut result = Vec::new();
        for element in arr.iter() {
            let callback_args = vec![element.clone()];
            let filter_result = self.call_function(callback.clone(), callback_args)?;
            if filter_result.is_truthy() {
                result.push(element.clone());
            }
        }

        Ok(Value::Array(Rc::new(RefCell::new(result))))
    }

    pub(super) fn handle_reduce(&mut self, args: &[Value]) -> Result<Value, String> {
        if args.len() < 2 || args.len() > 3 {
            return Err("reduce expects 2 or 3 arguments (array, function, [initial])".to_string());
        }

        let array = match &args[0] {
            Value::Array(arr) => arr.clone(),
            _ => {
                return Err(format!("reduce expects array, got {}", args[0].type_name()));
            }
        };

        let callback = args[1].clone();

        let arr = array.borrow();

        // 检查数组是否为空
        if arr.is_empty() {
            if args.len() == 3 {
                return Ok(args[2].clone());
            } else {
                return Err("reduce of empty array with no initial value".to_string());
            }
        }

        // 初始化累加器
        let (mut accumulator, start_index) = if args.len() == 3 {
            // 提供了初始值
            (args[2].clone(), 0)
        } else {
            // 使用第一个元素作为初始值
            (arr[0].clone(), 1)
        };

        // 执行reduce操作
        for i in start_index..arr.len() {
            let callback_args = vec![accumulator.clone(), arr[i].clone()];
            accumulator = self.call_function(callback.clone(), callback_args)?;
        }

        Ok(accumulator)
    }
}
