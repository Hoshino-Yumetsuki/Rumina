/// Expression evaluation implementation
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{Expr, MatchPattern};
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

                    // LSR-006: Set 方法调用
                    if let Value::Set(_) = &obj {
                        let method_name = format!("set_{}", member);
                        if let Ok(func) = self.get_variable(&method_name) {
                            return self.call_function(func, arg_vals);
                        }

                        let alt_method_name =
                            format!("set_to_{}", member.strip_prefix("to").unwrap_or(member));
                        if let Ok(func) = self.get_variable(&alt_method_name) {
                            return self.call_function(func, arg_vals);
                        }

                        return Err(format!("Set does not have method '{}'", member));
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
            Expr::BigInt(n) => Ok(Value::BigInt(n.clone())),
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

            Expr::Vector(elements) => {
                let mut values = Vec::new();
                for elem in elements {
                    values.push(self.eval_expr(elem)?);
                }
                Ok(Value::Vector(Rc::new(RefCell::new(values))))
            }

            Expr::Matrix(rows) => {
                let mut matrix = Vec::new();
                for row in rows {
                    let mut values = Vec::new();
                    for elem in row {
                        values.push(self.eval_expr(elem)?);
                    }
                    matrix.push(values);
                }
                Ok(Value::Matrix(Rc::new(RefCell::new(matrix))))
            }

            Expr::Set(elements) => {
                let mut values = Vec::new();
                for elem in elements {
                    let value = self.eval_expr(elem)?;
                    if !values.contains(&value) {
                        values.push(value);
                    }
                }
                Ok(Value::Set(values))
            }

            Expr::Struct(fields) => {
                let mut map = HashMap::new();
                for (key, value) in fields {
                    let val = self.eval_expr(value)?;
                    map.insert(key.clone(), val);
                }
                Ok(Value::Struct(Rc::new(RefCell::new(map))))
            }

            Expr::Table(entries) => {
                let mut map = HashMap::new();
                for (key, value) in entries {
                    let key = self.eval_expr(key)?.to_string();
                    let value = self.eval_expr(value)?;
                    map.insert(key, value);
                }
                Ok(Value::Struct(Rc::new(RefCell::new(map))))
            }

            Expr::Binary { left, op, right } => {
                if *op == crate::ast::BinOp::Pipe {
                    self.eval_pipe_expr(left, right)
                } else if *op == crate::ast::BinOp::Equivalent {
                    Ok(Value::Bool(self.expr_equivalent(left, right)))
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

            Expr::UnitStrip { expr, .. } => {
                let val = self.eval_expr(expr)?;
                match val {
                    Value::Int(_)
                    | Value::BigInt(_)
                    | Value::Float(_)
                    | Value::Rational(_)
                    | Value::Irrational(_)
                    | Value::Complex(_, _) => Ok(val),
                    other => Err(format!("Cannot strip units from {}", other.type_name())),
                }
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

                    // LSR-006: Set 方法调用
                    if let Value::Set(_) = &obj {
                        let mut method_args = vec![obj.clone()];
                        method_args.extend(arg_vals);

                        let method_name = format!("set_{}", member);
                        if let Ok(func) = self.get_variable(&method_name) {
                            return self.call_function(func, method_args);
                        }

                        // 尝试 set_to_* 形式
                        let alt_method_name =
                            format!("set_to_{}", member.strip_prefix("to").unwrap_or(member));
                        if let Ok(func) = self.get_variable(&alt_method_name) {
                            return self.call_function(func, method_args);
                        }

                        return Err(format!("Set does not have method '{}'", member));
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

                    if matches!(
                        &method,
                        Value::NativeFunction { name, .. } if name.starts_with("std.")
                    ) {
                        return self.call_function(method, arg_vals);
                    }

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
                    (Value::Vector(values), Value::Int(i)) => {
                        if i <= 0 {
                            return Err(format!("Vector index out of bounds: {}", i));
                        }
                        values
                            .borrow()
                            .get((i - 1) as usize)
                            .cloned()
                            .ok_or_else(|| format!("Vector index out of bounds: {}", i))
                    }
                    (Value::Matrix(rows), Value::Int(i)) => {
                        if i <= 0 {
                            return Err(format!("Matrix row index out of bounds: {}", i));
                        }
                        let row = rows
                            .borrow()
                            .get((i - 1) as usize)
                            .cloned()
                            .ok_or_else(|| format!("Matrix row index out of bounds: {}", i))?;
                        Ok(Value::Vector(Rc::new(RefCell::new(row))))
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
                    (Value::Struct(map) | Value::Module(map), key) => {
                        let key = key.to_string();
                        Ok(map.borrow().get(&key).cloned().unwrap_or(Value::Null))
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

            Expr::Match { target, arms } => {
                let target = self.eval_expr(target)?;
                for arm in arms {
                    let binding = match &arm.pattern {
                        MatchPattern::Wildcard => Some(None),
                        MatchPattern::Binding(name) => Some(Some(name.clone())),
                        MatchPattern::Literal(pattern) => {
                            let pattern = self.eval_expr(pattern)?;
                            if pattern == target { Some(None) } else { None }
                        }
                    };

                    let Some(binding_name) = binding else {
                        continue;
                    };

                    let has_binding = binding_name.is_some();
                    if let Some(name) = binding_name {
                        self.locals.push(Rc::new(RefCell::new(HashMap::from([(
                            name,
                            target.clone(),
                        )]))));
                        self.immutable_locals.push(Default::default());
                    }

                    let guard_matches = if let Some(guard) = &arm.guard {
                        self.eval_expr(guard)?.is_truthy()
                    } else {
                        true
                    };

                    if guard_matches {
                        let result = self.eval_expr(&arm.expr);
                        if has_binding {
                            self.locals.pop();
                            self.immutable_locals.pop();
                        }
                        return result;
                    }

                    if has_binding {
                        self.locals.pop();
                        self.immutable_locals.pop();
                    }
                }

                Err("Match expression did not match any arm".to_string())
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

    fn expr_equivalent(&self, left: &Expr, right: &Expr) -> bool {
        Self::normalize_equivalence_expr(left) == Self::normalize_equivalence_expr(right)
    }

    fn normalize_equivalence_expr(expr: &Expr) -> Expr {
        match expr {
            Expr::Binary { left, op, right } => {
                let left = Self::normalize_equivalence_expr(left);
                let right = Self::normalize_equivalence_expr(right);

                match (left, *op, right) {
                    (Expr::Int(a), crate::ast::BinOp::Add, Expr::Int(b)) => Expr::Int(a + b),
                    (Expr::Int(a), crate::ast::BinOp::Sub, Expr::Int(b)) => Expr::Int(a - b),
                    (Expr::Int(a), crate::ast::BinOp::Mul, Expr::Int(b)) => Expr::Int(a * b),
                    (expr, crate::ast::BinOp::Add, Expr::Int(0))
                    | (Expr::Int(0), crate::ast::BinOp::Add, expr) => expr,
                    (expr, crate::ast::BinOp::Mul, Expr::Int(1))
                    | (Expr::Int(1), crate::ast::BinOp::Mul, expr) => expr,
                    (_, crate::ast::BinOp::Mul, Expr::Int(0))
                    | (Expr::Int(0), crate::ast::BinOp::Mul, _) => Expr::Int(0),
                    (left, crate::ast::BinOp::Sub, right) if left == right => Expr::Int(0),
                    (left, op, right) => Expr::Binary {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    },
                }
            }
            Expr::Unary { op, expr } => Expr::Unary {
                op: *op,
                expr: Box::new(Self::normalize_equivalence_expr(expr)),
            },
            other => other.clone(),
        }
    }
}

#[cfg(test)]
#[allow(clippy::approx_constant)]
mod tests {
    use super::*;
    use crate::ast::{Stmt, UnaryOp};

    fn new_interpreter() -> Interpreter {
        Interpreter::new()
    }

    #[test]
    fn test_literals() {
        let mut interp = new_interpreter();
        assert_eq!(interp.eval_expr(&Expr::Int(42)).unwrap(), Value::Int(42));
        assert_eq!(
            interp.eval_expr(&Expr::Float(3.14)).unwrap(),
            Value::Float(3.14)
        );
        assert_eq!(
            interp.eval_expr(&Expr::String("test".into())).unwrap(),
            Value::String("test".into())
        );
        assert_eq!(
            interp.eval_expr(&Expr::Bool(true)).unwrap(),
            Value::Bool(true)
        );
        assert_eq!(interp.eval_expr(&Expr::Null).unwrap(), Value::Null);
    }

    #[test]
    fn test_array() {
        let mut interp = new_interpreter();
        let arr = Expr::Array(vec![Expr::Int(1), Expr::Int(2), Expr::Int(3)]);
        let result = interp.eval_expr(&arr).unwrap();
        if let Value::Array(a) = result {
            assert_eq!(a.borrow().len(), 3);
        } else {
            panic!("Expected array");
        }
    }

    #[test]
    fn test_struct() {
        let mut interp = new_interpreter();
        let fields = vec![("x".into(), Expr::Int(10))];
        let s = Expr::Struct(fields);
        let result = interp.eval_expr(&s).unwrap();
        if let Value::Struct(m) = result {
            assert_eq!(m.borrow().get("x"), Some(&Value::Int(10)));
        } else {
            panic!("Expected struct");
        }
    }

    #[test]
    fn test_unary_neg() {
        let mut interp = new_interpreter();
        let expr = Expr::Unary {
            op: UnaryOp::Neg,
            expr: Box::new(Expr::Int(5)),
        };
        assert_eq!(interp.eval_expr(&expr).unwrap(), Value::Int(-5));
    }

    #[test]
    fn test_unary_not() {
        let mut interp = new_interpreter();
        let expr = Expr::Unary {
            op: UnaryOp::Not,
            expr: Box::new(Expr::Bool(true)),
        };
        assert_eq!(interp.eval_expr(&expr).unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_index_array_positive() {
        let mut interp = new_interpreter();
        let arr = Expr::Array(vec![Expr::Int(10), Expr::Int(20)]);
        let idx = Expr::Index {
            object: Box::new(arr),
            index: Box::new(Expr::Int(0)),
        };
        assert_eq!(interp.eval_expr(&idx).unwrap(), Value::Int(10));
    }

    #[test]
    fn test_index_array_negative() {
        let mut interp = new_interpreter();
        let arr = Expr::Array(vec![Expr::Int(10), Expr::Int(20)]);
        let idx = Expr::Index {
            object: Box::new(arr),
            index: Box::new(Expr::Int(-1)),
        };
        assert_eq!(interp.eval_expr(&idx).unwrap(), Value::Int(20));
    }

    #[test]
    fn test_index_array_out_of_bounds() {
        let mut interp = new_interpreter();
        let arr = Expr::Array(vec![Expr::Int(10)]);
        let idx = Expr::Index {
            object: Box::new(arr),
            index: Box::new(Expr::Int(5)),
        };
        assert!(interp.eval_expr(&idx).is_err());
    }

    #[test]
    fn test_index_string() {
        let mut interp = new_interpreter();
        let s = Expr::String("abc".into());
        let idx = Expr::Index {
            object: Box::new(s),
            index: Box::new(Expr::Int(1)),
        };
        assert_eq!(interp.eval_expr(&idx).unwrap(), Value::String("b".into()));
    }

    #[test]
    fn test_index_string_negative() {
        let mut interp = new_interpreter();
        let s = Expr::String("abc".into());
        let idx = Expr::Index {
            object: Box::new(s),
            index: Box::new(Expr::Int(-1)),
        };
        assert_eq!(interp.eval_expr(&idx).unwrap(), Value::String("c".into()));
    }

    #[test]
    fn test_index_invalid() {
        let mut interp = new_interpreter();
        let idx = Expr::Index {
            object: Box::new(Expr::Int(5)),
            index: Box::new(Expr::Int(0)),
        };
        assert!(interp.eval_expr(&idx).is_err());
    }

    #[test]
    fn test_member_invalid_type() {
        let mut interp = new_interpreter();
        let member = Expr::Member {
            object: Box::new(Expr::Int(5)),
            member: "x".into(),
        };
        assert!(interp.eval_expr(&member).is_err());
    }

    #[test]
    fn test_lambda() {
        let mut interp = new_interpreter();
        let lambda = Expr::Lambda {
            params: vec!["x".into()],
            body: Box::new(Stmt::Expr(Expr::Ident("x".into()))),
            is_simple: true,
        };
        let result = interp.eval_expr(&lambda).unwrap();
        assert!(matches!(result, Value::Lambda { .. }));
    }

    #[test]
    fn test_multi() {
        let mut interp = new_interpreter();
        let multi = Expr::Multi(vec![Expr::Int(1), Expr::Int(2)]);
        let result = interp.eval_expr(&multi).unwrap();
        if let Value::MultiValue(v) = result {
            assert_eq!(v.len(), 2);
        } else {
            panic!("Expected multi");
        }
    }

    #[test]
    fn test_try_invalid_type() {
        let mut interp = new_interpreter();
        let try_expr = Expr::Try(Box::new(Expr::Int(5)));
        assert!(interp.eval_expr(&try_expr).is_err());
    }
}
