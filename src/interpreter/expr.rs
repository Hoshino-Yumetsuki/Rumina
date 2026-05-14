/// Expression evaluation implementation
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{BinOp, Expr, MatchPattern};
use crate::builtin::math;
use crate::value::Value;

use super::Interpreter;

impl Interpreter {
    fn structural_expr_bindings(
        pattern: &Expr,
        target: &Expr,
        bindings: &mut HashMap<String, Value>,
    ) -> bool {
        match pattern {
            Expr::Ident(name) => {
                if let Some(Value::Expr(previous)) = bindings.get(name) {
                    previous == target
                } else {
                    bindings.insert(name.clone(), Value::Expr(target.clone()));
                    true
                }
            }
            Expr::Int(a) => matches!(target, Expr::Int(b) if a == b),
            Expr::BigInt(a) => matches!(target, Expr::BigInt(b) if a == b),
            Expr::Float(a) => matches!(target, Expr::Float(b) if a == b),
            Expr::String(a) => matches!(target, Expr::String(b) if a == b),
            Expr::Bool(a) => matches!(target, Expr::Bool(b) if a == b),
            Expr::Null => matches!(target, Expr::Null),
            Expr::Unary { op, expr } => match target {
                Expr::Unary {
                    op: target_op,
                    expr: target_expr,
                } if op == target_op => Self::structural_expr_bindings(expr, target_expr, bindings),
                _ => false,
            },
            Expr::Binary { left, op, right } => {
                let Expr::Binary {
                    left: target_left,
                    op: target_op,
                    right: target_right,
                } = target
                else {
                    return false;
                };

                if op != target_op {
                    return false;
                }

                let original = bindings.clone();
                if Self::structural_expr_bindings(left, target_left, bindings)
                    && Self::structural_expr_bindings(right, target_right, bindings)
                {
                    return true;
                }

                if matches!(op, BinOp::Add | BinOp::Mul) {
                    *bindings = original.clone();
                    if Self::structural_expr_bindings(left, target_right, bindings)
                        && Self::structural_expr_bindings(right, target_left, bindings)
                    {
                        return true;
                    }
                }

                *bindings = original;
                false
            }
            Expr::Call { func, args } => {
                let Expr::Call {
                    func: target_func,
                    args: target_args,
                } = target
                else {
                    return false;
                };
                if args.len() != target_args.len() {
                    return false;
                }

                match (func.as_ref(), target_func.as_ref()) {
                    (Expr::Ident(expected), Expr::Ident(actual)) if expected == actual => {}
                    _ => return false,
                }

                let original = bindings.clone();
                for (arg, target_arg) in args.iter().zip(target_args) {
                    if !Self::structural_expr_bindings(arg, target_arg, bindings) {
                        *bindings = original;
                        return false;
                    }
                }
                true
            }
            _ => pattern == target,
        }
    }

    fn substitute_expr_bindings(&self, expr: &Expr) -> Expr {
        match expr {
            Expr::Ident(name) => match self.get_variable(name) {
                Ok(Value::Expr(bound)) => bound,
                _ => expr.clone(),
            },
            Expr::Binary { left, op, right } => Expr::Binary {
                left: Box::new(self.substitute_expr_bindings(left)),
                op: *op,
                right: Box::new(self.substitute_expr_bindings(right)),
            },
            Expr::Unary { op, expr } => Expr::Unary {
                op: *op,
                expr: Box::new(self.substitute_expr_bindings(expr)),
            },
            Expr::Call { func, args } => Expr::Call {
                func: Box::new(self.substitute_expr_bindings(func)),
                args: args
                    .iter()
                    .map(|arg| self.substitute_expr_bindings(arg))
                    .collect(),
            },
            _ => expr.clone(),
        }
    }

    fn unit_scale(&self, unit: &str) -> Result<i64, String> {
        match unit {
            "m" => Ok(1),
            "km" => Ok(1000),
            "s" => Ok(1),
            "h" => Ok(3600),
            "m/s" => Ok(1),
            "km/h" => Ok(1),
            _ => match self.get_variable(&format!("__unit_{}", unit))? {
                Value::Int(scale) => Ok(scale),
                Value::Bool(true) => Ok(1),
                other => Err(format!(
                    "Invalid unit declaration for {}: {}",
                    unit,
                    other.type_name()
                )),
            },
        }
    }

    pub(super) fn scale_unit_value(&self, value: Value, scale: i64) -> Result<Value, String> {
        match value {
            Value::Int(n) => n
                .checked_mul(scale)
                .map(Value::Int)
                .ok_or_else(|| "UnitStripOverflow: unit strip scaling overflowed i64".to_string()),
            Value::BigInt(n) => Ok(Value::BigInt(n * crate::numeric::BigInt::from(scale))),
            Value::Float(n) => Ok(Value::Float(n * scale as f64)),
            Value::Rational(n) => Ok(Value::Rational(
                n * crate::numeric::rational_from_integer(crate::numeric::BigInt::from(scale)),
            )),
            other => Err(format!("Cannot strip units from {}", other.type_name())),
        }
    }

    fn divide_unit_value(&self, value: Value, scale: i64) -> Result<Value, String> {
        match value {
            Value::Int(n) if n % scale == 0 => Ok(Value::Int(n / scale)),
            Value::Int(n) => Ok(Value::Rational(crate::numeric::rational_new(
                crate::numeric::BigInt::from(n),
                crate::numeric::BigInt::from(scale),
            ))),
            Value::BigInt(n) => Ok(Value::Rational(crate::numeric::rational_new(
                n,
                crate::numeric::BigInt::from(scale),
            ))),
            Value::Float(n) => Ok(Value::Float(n / scale as f64)),
            Value::Rational(n) => Ok(Value::Rational(
                n / crate::numeric::rational_from_integer(crate::numeric::BigInt::from(scale)),
            )),
            other => Err(format!("Cannot convert units for {}", other.type_name())),
        }
    }

    fn multiply_unit_value_by_ratio(
        &self,
        value: Value,
        numerator: i64,
        denominator: i64,
    ) -> Result<Value, String> {
        match value {
            Value::Int(n) if (n * numerator) % denominator == 0 => {
                Ok(Value::Int((n * numerator) / denominator))
            }
            Value::Int(n) => Ok(Value::Rational(crate::numeric::rational_new(
                crate::numeric::BigInt::from(n * numerator),
                crate::numeric::BigInt::from(denominator),
            ))),
            Value::BigInt(n) => Ok(Value::Rational(crate::numeric::rational_new(
                n * crate::numeric::BigInt::from(numerator),
                crate::numeric::BigInt::from(denominator),
            ))),
            Value::Float(n) => Ok(Value::Float(n * numerator as f64 / denominator as f64)),
            Value::Rational(n) => Ok(Value::Rational(
                n * crate::numeric::rational_new(
                    crate::numeric::BigInt::from(numerator),
                    crate::numeric::BigInt::from(denominator),
                ),
            )),
            other => Err(format!("Cannot convert units for {}", other.type_name())),
        }
    }

    fn units_are_compatible(&self, source: &str, target: &str) -> bool {
        source == target
            || (matches!(source, "m" | "km") && matches!(target, "m" | "km"))
            || (matches!(source, "m/s" | "km/h") && matches!(target, "m/s" | "km/h"))
    }

    fn try_call_special_method(
        &mut self,
        obj: Value,
        member: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, String>> {
        if let Value::Array(_) = &obj {
            return Some(self.call_array_method(obj, member, args));
        }

        if let Value::Struct(map) = &obj {
            return match member {
                "has" => {
                    if args.len() != 1 {
                        Some(Err(format!("has() expects 1 argument, got {}", args.len())))
                    } else {
                        let key = args[0].to_string();
                        Some(Ok(Value::Bool(map.borrow().contains_key(&key))))
                    }
                }
                "keys" => {
                    if !args.is_empty() {
                        Some(Err(format!(
                            "keys() expects 0 arguments, got {}",
                            args.len()
                        )))
                    } else {
                        let keys = map.borrow().keys().cloned().map(Value::String).collect();
                        Some(Ok(Value::Vector(Rc::new(RefCell::new(keys)))))
                    }
                }
                "values" => {
                    if !args.is_empty() {
                        Some(Err(format!(
                            "values() expects 0 arguments, got {}",
                            args.len()
                        )))
                    } else {
                        let values = map.borrow().values().cloned().collect();
                        Some(Ok(Value::Vector(Rc::new(RefCell::new(values)))))
                    }
                }
                "items" => {
                    if !args.is_empty() {
                        Some(Err(format!(
                            "items() expects 0 arguments, got {}",
                            args.len()
                        )))
                    } else {
                        let items = map
                            .borrow()
                            .iter()
                            .map(|(key, value)| {
                                Value::Vector(Rc::new(RefCell::new(vec![
                                    Value::String(key.clone()),
                                    value.clone(),
                                ])))
                            })
                            .collect();
                        Some(Ok(Value::Vector(Rc::new(RefCell::new(items)))))
                    }
                }
                _ => None,
            };
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

            Expr::Wildcard => Err("Wildcard can only be used inside a matrix slice".to_string()),
            Expr::Range { .. } => Err("Range can only be used inside a matrix slice".to_string()),

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
                    if !Self::is_equivalence_operand(left) || !Self::is_equivalence_operand(right) {
                        return Err(
                            "EqvTypeMismatch: === expects mathematical Expr operands".to_string()
                        );
                    }
                    let left = self.substitute_expr_bindings(left);
                    let right = self.substitute_expr_bindings(right);
                    Ok(Value::Bool(self.expr_equivalent(&left, &right)?))
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

            Expr::UnitStrip { expr, mode } => {
                let val = self.eval_expr(expr)?;
                match val {
                    Value::UnitNumber { value, scale, .. } => match mode {
                        crate::ast::UnitStripMode::Num => self.scale_unit_value(*value, scale),
                        crate::ast::UnitStripMode::Scalar => Ok(*value),
                    },
                    Value::Int(_)
                    | Value::BigInt(_)
                    | Value::Float(_)
                    | Value::Rational(_)
                    | Value::Irrational(_)
                    | Value::Complex(_, _) => Ok(val),
                    other => Err(format!(
                        "UnitStripTypeMismatch: cannot strip units from {}",
                        other.type_name()
                    )),
                }
            }

            Expr::UnitConvert { expr, unit } => {
                let target_scale = self.unit_scale(unit).map_err(|err| {
                    format!("UnitStripInvalid: invalid target unit '{}': {}", unit, err)
                })?;
                let val = self.eval_expr(expr)?;
                match val {
                    Value::UnitNumber {
                        value,
                        unit: source_unit,
                        scale: source_scale,
                    } => {
                        if !self.units_are_compatible(&source_unit, unit) {
                            return Err(format!(
                                "UnitStripInvalid: cannot convert from '{}' to '{}'",
                                source_unit, unit
                            ));
                        }
                        if source_unit == "m/s" && unit == "km/h" {
                            let scaled = self.scale_unit_value(*value, source_scale)?;
                            let converted = self.multiply_unit_value_by_ratio(scaled, 18, 5)?;
                            return Ok(Value::UnitNumber {
                                value: Box::new(converted),
                                unit: unit.clone(),
                                scale: 1,
                            });
                        }
                        if source_unit == "km/h" && unit == "m/s" {
                            let converted = self.multiply_unit_value_by_ratio(*value, 5, 18)?;
                            return Ok(Value::UnitNumber {
                                value: Box::new(converted),
                                unit: unit.clone(),
                                scale: 1,
                            });
                        }
                        let scaled = self.scale_unit_value(*value, source_scale)?;
                        let converted = self.divide_unit_value(scaled, target_scale)?;
                        Ok(Value::UnitNumber {
                            value: Box::new(converted),
                            unit: unit.clone(),
                            scale: 1,
                        })
                    }
                    other => Err(format!(
                        "UnitStripInvalid: cannot convert {} to unit '{}'",
                        other.type_name(),
                        unit
                    )),
                }
            }

            Expr::UnitAttach { expr, unit } => {
                let value = self.eval_expr(expr)?;
                match value {
                    Value::Int(_)
                    | Value::BigInt(_)
                    | Value::Float(_)
                    | Value::Rational(_)
                    | Value::Irrational(_)
                    | Value::Complex(_, _) => Ok(Value::UnitNumber {
                        value: Box::new(value),
                        unit: unit.clone(),
                        scale: self.unit_scale(unit)?,
                    }),
                    other => Err(format!("Cannot attach unit to {}", other.type_name())),
                }
            }

            Expr::MatrixTranspose { expr, conjugate } => {
                let value = self.eval_expr(expr)?;
                let Value::Matrix(rows) = value else {
                    return Err(format!("Cannot transpose {}", value.type_name()));
                };
                let rows = rows.borrow();
                let row_count = rows.len();
                let col_count = rows.first().map_or(0, Vec::len);
                let mut transposed = vec![Vec::with_capacity(row_count); col_count];
                for row in rows.iter() {
                    if row.len() != col_count {
                        return Err("Matrix transpose expects rectangular matrix".to_string());
                    }
                    for (col, item) in row.iter().enumerate() {
                        let value = if *conjugate {
                            math::conj(std::slice::from_ref(item))?
                        } else {
                            item.clone()
                        };
                        transposed[col].push(value);
                    }
                }
                Ok(Value::Matrix(Rc::new(RefCell::new(transposed))))
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
                if let (Value::Matrix(rows), Expr::Multi(indices)) = (&obj, index.as_ref())
                    && indices.len() == 2
                {
                    if let Expr::Range { start, end } = &indices[0] {
                        let start = self.eval_expr(start)?.to_int()?;
                        let end = self.eval_expr(end)?.to_int()?;
                        if start <= 0 {
                            return Err(format!("Matrix row index out of bounds: {}", start));
                        }
                        if end < start {
                            return Err(format!("Invalid matrix row range: {}..{}", start, end));
                        }

                        if let Expr::Range {
                            start: col_start,
                            end: col_end,
                        } = &indices[1]
                        {
                            let col_start = self.eval_expr(col_start)?.to_int()?;
                            let col_end = self.eval_expr(col_end)?.to_int()?;
                            if col_start <= 0 {
                                return Err(format!(
                                    "Matrix column index out of bounds: {}",
                                    col_start
                                ));
                            }
                            if col_end < col_start {
                                return Err(format!(
                                    "Invalid matrix column range: {}..{}",
                                    col_start, col_end
                                ));
                            }

                            let rows = rows.borrow();
                            let mut sliced_rows = Vec::new();
                            for row_number in start..=end {
                                let row = rows.get((row_number - 1) as usize).ok_or_else(|| {
                                    format!("Matrix row index out of bounds: {}", row_number)
                                })?;

                                let mut sliced_row = Vec::new();
                                for col_number in col_start..=col_end {
                                    let value = row
                                        .get((col_number - 1) as usize)
                                        .cloned()
                                        .ok_or_else(|| {
                                            format!(
                                                "Matrix column index out of bounds: {}",
                                                col_number
                                            )
                                        })?;
                                    sliced_row.push(value);
                                }
                                sliced_rows.push(sliced_row);
                            }
                            return Ok(Value::Matrix(Rc::new(RefCell::new(sliced_rows))));
                        }

                        let col = self.eval_expr(&indices[1])?.to_int()?;
                        if col <= 0 {
                            return Err(format!("Matrix column index out of bounds: {}", col));
                        }

                        let rows = rows.borrow();
                        let mut values = Vec::new();
                        for row_number in start..=end {
                            let row = rows.get((row_number - 1) as usize).ok_or_else(|| {
                                format!("Matrix row index out of bounds: {}", row_number)
                            })?;
                            let value = row.get((col - 1) as usize).cloned().ok_or_else(|| {
                                format!("Matrix column index out of bounds: {}", col)
                            })?;
                            values.push(value);
                        }
                        return Ok(Value::Vector(Rc::new(RefCell::new(values))));
                    }

                    if matches!(indices[0], Expr::Wildcard) {
                        let col = self.eval_expr(&indices[1])?.to_int()?;
                        if col <= 0 {
                            return Err(format!("Matrix column index out of bounds: {}", col));
                        }

                        let rows = rows.borrow();
                        let mut values = Vec::with_capacity(rows.len());
                        for row in rows.iter() {
                            let value = row.get((col - 1) as usize).cloned().ok_or_else(|| {
                                format!("Matrix column index out of bounds: {}", col)
                            })?;
                            values.push(value);
                        }
                        return Ok(Value::Vector(Rc::new(RefCell::new(values))));
                    }

                    if matches!(indices[1], Expr::Wildcard) {
                        let row = self.eval_expr(&indices[0])?.to_int()?;
                        if row <= 0 {
                            return Err(format!("Matrix row index out of bounds: {}", row));
                        }

                        let values = rows
                            .borrow()
                            .get((row - 1) as usize)
                            .cloned()
                            .ok_or_else(|| format!("Matrix row index out of bounds: {}", row))?;
                        return Ok(Value::Vector(Rc::new(RefCell::new(values))));
                    }

                    let row = self.eval_expr(&indices[0])?.to_int()?;
                    let col = self.eval_expr(&indices[1])?.to_int()?;
                    if row <= 0 {
                        return Err(format!("Matrix row index out of bounds: {}", row));
                    }
                    if col <= 0 {
                        return Err(format!("Matrix column index out of bounds: {}", col));
                    }
                    return rows
                        .borrow()
                        .get((row - 1) as usize)
                        .and_then(|values| values.get((col - 1) as usize))
                        .cloned()
                        .ok_or_else(|| format!("Matrix index out of bounds: {},{}", row, col));
                }
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
                let has_expr_pattern = arms
                    .iter()
                    .any(|arm| matches!(arm.pattern, MatchPattern::Expr(_)));
                let target_value = if has_expr_pattern {
                    None
                } else {
                    Some(self.eval_expr(target)?)
                };
                for arm in arms {
                    let bindings = match &arm.pattern {
                        MatchPattern::Wildcard => Some(HashMap::new()),
                        MatchPattern::Binding(name) => Some(HashMap::from([(
                            name.clone(),
                            target_value
                                .as_ref()
                                .cloned()
                                .unwrap_or_else(|| Value::Expr((**target).clone())),
                        )])),
                        MatchPattern::Literal(pattern) => {
                            let pattern = self.eval_expr(pattern)?;
                            if target_value
                                .as_ref()
                                .is_some_and(|target| &pattern == target)
                            {
                                Some(HashMap::new())
                            } else {
                                None
                            }
                        }
                        MatchPattern::Expr(pattern) => {
                            let mut bindings = HashMap::new();
                            if Self::structural_expr_bindings(pattern, target, &mut bindings) {
                                Some(bindings)
                            } else {
                                None
                            }
                        }
                        MatchPattern::Vector(names) => {
                            let Some(Value::Vector(values)) = target_value.as_ref() else {
                                continue;
                            };
                            let values = values.borrow();
                            if values.len() != names.len() {
                                continue;
                            }
                            Some(names.iter().cloned().zip(values.iter().cloned()).collect())
                        }
                    };

                    let Some(bindings) = bindings else {
                        continue;
                    };

                    let has_binding = !bindings.is_empty();
                    if has_binding {
                        self.locals.push(Rc::new(RefCell::new(bindings)));
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

            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = self.eval_expr(condition)?;
                let branch = if condition.is_truthy() {
                    then_branch
                } else {
                    else_branch
                };
                self.execute_stmts_for_last_value(branch)
                    .map_err(|err| err.to_string())?
                    .ok_or_else(|| "If expression branch did not produce a value".to_string())
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

    fn expr_equivalent(&self, left: &Expr, right: &Expr) -> Result<bool, String> {
        let profile = crate::builtin::utils::current_eqv_profile();
        let budget = crate::builtin::utils::current_eqv_budget();
        let original_node_count =
            Self::equivalence_node_count(left) + Self::equivalence_node_count(right);
        let mut budget_state = EqvBudgetState::new(budget);
        let left = Self::normalize_equivalence_expr(left, &profile, &mut budget_state, 0)?;
        let right = Self::normalize_equivalence_expr(right, &profile, &mut budget_state, 0)?;
        budget_state.check_growth(
            original_node_count,
            Self::equivalence_node_count(&left) + Self::equivalence_node_count(&right),
        )?;
        Ok(left == right)
    }

    fn is_equivalence_operand(expr: &Expr) -> bool {
        match expr {
            Expr::Bool(_)
            | Expr::String(_)
            | Expr::Null
            | Expr::Array(_)
            | Expr::Vector(_)
            | Expr::Matrix(_)
            | Expr::Set(_)
            | Expr::Struct(_)
            | Expr::Table(_)
            | Expr::Multi(_)
            | Expr::UnitStrip { .. }
            | Expr::UnitConvert { .. }
            | Expr::UnitAttach { .. }
            | Expr::MatrixTranspose { .. }
            | Expr::Lambda { .. }
            | Expr::Try(_)
            | Expr::Range { .. }
            | Expr::Match { .. }
            | Expr::If { .. } => false,
            Expr::Binary { left, op, right } => {
                matches!(
                    op,
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod | BinOp::Pow
                ) && Self::is_equivalence_operand(left)
                    && Self::is_equivalence_operand(right)
            }
            Expr::Unary { expr, .. } => Self::is_equivalence_operand(expr),
            Expr::Call { args, .. } => args.iter().all(Self::is_equivalence_operand),
            Expr::Member { object, .. } => Self::is_equivalence_operand(object),
            Expr::Index { object, index } => {
                Self::is_equivalence_operand(object) && Self::is_equivalence_operand(index)
            }
            Expr::Namespace { .. }
            | Expr::Int(_)
            | Expr::BigInt(_)
            | Expr::Float(_)
            | Expr::Ident(_)
            | Expr::Wildcard => true,
        }
    }

    fn normalize_equivalence_expr(
        expr: &Expr,
        profile: &str,
        budget: &mut EqvBudgetState,
        depth: usize,
    ) -> Result<Expr, String> {
        budget.enter(depth)?;

        match expr {
            Expr::Binary { left, op, right } => {
                let left = Self::normalize_equivalence_expr(left, profile, budget, depth + 1)?;
                let right = Self::normalize_equivalence_expr(right, profile, budget, depth + 1)?;

                Ok(match (left, *op, right) {
                    (Expr::Int(a), crate::ast::BinOp::Add, Expr::Int(b)) => {
                        budget.rewrite()?;
                        Expr::Int(a + b)
                    }
                    (Expr::Int(a), crate::ast::BinOp::Sub, Expr::Int(b)) => {
                        budget.rewrite()?;
                        Expr::Int(a - b)
                    }
                    (Expr::Int(a), crate::ast::BinOp::Mul, Expr::Int(b)) => {
                        budget.rewrite()?;
                        Expr::Int(a * b)
                    }
                    (Expr::Int(a), crate::ast::BinOp::Div, Expr::Int(b))
                        if b != 0 && a % b == 0 =>
                    {
                        budget.rewrite()?;
                        Expr::Int(a / b)
                    }
                    (Expr::Int(a), crate::ast::BinOp::Pow, Expr::Int(b))
                        if b >= 0 && b <= u32::MAX as i64 =>
                    {
                        if let Some(value) = a.checked_pow(b as u32) {
                            budget.rewrite()?;
                            Expr::Int(value)
                        } else {
                            Expr::Binary {
                                left: Box::new(Expr::Int(a)),
                                op: crate::ast::BinOp::Pow,
                                right: Box::new(Expr::Int(b)),
                            }
                        }
                    }
                    (expr, crate::ast::BinOp::Add, Expr::Int(0))
                    | (Expr::Int(0), crate::ast::BinOp::Add, expr) => {
                        budget.rewrite()?;
                        expr
                    }
                    (expr, crate::ast::BinOp::Mul, Expr::Int(1))
                    | (Expr::Int(1), crate::ast::BinOp::Mul, expr) => {
                        budget.rewrite()?;
                        expr
                    }
                    (_, crate::ast::BinOp::Mul, Expr::Int(0))
                    | (Expr::Int(0), crate::ast::BinOp::Mul, _) => {
                        budget.rewrite()?;
                        Expr::Int(0)
                    }
                    (expr, crate::ast::BinOp::Sub, Expr::Int(0)) => {
                        budget.rewrite()?;
                        expr
                    }
                    (expr, crate::ast::BinOp::Div, Expr::Int(1)) => {
                        budget.rewrite()?;
                        expr
                    }
                    (expr, crate::ast::BinOp::Pow, Expr::Int(1)) => {
                        budget.rewrite()?;
                        expr
                    }
                    (_, crate::ast::BinOp::Pow, Expr::Int(0)) => {
                        budget.rewrite()?;
                        Expr::Int(1)
                    }
                    (left, crate::ast::BinOp::Sub, right) if left == right => {
                        budget.rewrite()?;
                        Expr::Int(0)
                    }
                    (left, crate::ast::BinOp::Add, right)
                        if profile == "Trig-Basic"
                            && Self::is_trig_pythagorean_identity(&left, &right) =>
                    {
                        budget.rewrite()?;
                        Expr::Int(1)
                    }
                    (left, crate::ast::BinOp::Add, right) => Self::normalize_commutative_expr(
                        crate::ast::BinOp::Add,
                        left,
                        right,
                        budget,
                    )?,
                    (left, crate::ast::BinOp::Mul, right) => Self::normalize_commutative_expr(
                        crate::ast::BinOp::Mul,
                        left,
                        right,
                        budget,
                    )?,
                    (left, op, right) => Expr::Binary {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    },
                })
            }
            Expr::Unary { op, expr } => Ok(Expr::Unary {
                op: *op,
                expr: Box::new(Self::normalize_equivalence_expr(
                    expr,
                    profile,
                    budget,
                    depth + 1,
                )?),
            }),
            Expr::Call { func, args } => {
                let func = Box::new(Self::normalize_equivalence_expr(
                    func,
                    profile,
                    budget,
                    depth + 1,
                )?);
                let args: Vec<Expr> = args
                    .iter()
                    .map(|arg| Self::normalize_equivalence_expr(arg, profile, budget, depth + 1))
                    .collect::<Result<_, _>>()?;

                if profile == "ExpLog-Basic"
                    && let Expr::Ident(func_name) = func.as_ref()
                {
                    if func_name == "exp"
                        && args.len() == 1
                        && let Expr::Call {
                            func: inner_func,
                            args: inner_args,
                        } = &args[0]
                        && let Expr::Ident(inner_func_name) = inner_func.as_ref()
                        && inner_func_name == "log"
                        && inner_args.len() == 1
                    {
                        budget.rewrite()?;
                        return Ok(inner_args[0].clone());
                    }

                    if func_name == "log" && args == [Expr::Int(1)] {
                        budget.rewrite()?;
                        return Ok(Expr::Int(0));
                    }

                    if func_name == "exp" && args == [Expr::Int(0)] {
                        budget.rewrite()?;
                        return Ok(Expr::Int(1));
                    }
                }

                Ok(Expr::Call { func, args })
            }
            other => Ok(other.clone()),
        }
    }

    fn is_trig_pythagorean_identity(left: &Expr, right: &Expr) -> bool {
        let sin_left = Self::trig_square_arg(left, "sin");
        let cos_right = Self::trig_square_arg(right, "cos");
        if matches!((sin_left, cos_right), (Some(a), Some(b)) if a == b) {
            return true;
        }

        let cos_left = Self::trig_square_arg(left, "cos");
        let sin_right = Self::trig_square_arg(right, "sin");
        matches!((cos_left, sin_right), (Some(a), Some(b)) if a == b)
    }

    fn trig_square_arg<'a>(expr: &'a Expr, name: &str) -> Option<&'a Expr> {
        let Expr::Binary { left, op, right } = expr else {
            return None;
        };
        if *op != crate::ast::BinOp::Pow || right.as_ref() != &Expr::Int(2) {
            return None;
        }

        let Expr::Call { func, args } = left.as_ref() else {
            return None;
        };
        if args.len() != 1 {
            return None;
        }

        match func.as_ref() {
            Expr::Ident(func_name) if func_name == name => Some(&args[0]),
            _ => None,
        }
    }

    fn normalize_commutative_expr(
        op: crate::ast::BinOp,
        left: Expr,
        right: Expr,
        budget: &mut EqvBudgetState,
    ) -> Result<Expr, String> {
        let mut terms = Vec::new();
        Self::collect_commutative_terms(op, left, &mut terms);
        Self::collect_commutative_terms(op, right, &mut terms);

        if terms.len() > 2 {
            budget.rewrite()?;
        }

        match op {
            crate::ast::BinOp::Add => Self::combine_add_terms(&mut terms, budget)?,
            crate::ast::BinOp::Mul => Self::combine_mul_terms(&mut terms, budget)?,
            _ => {}
        }

        terms.sort_by_key(|expr| format!("{:?}", expr));
        let mut terms = terms.into_iter();
        let Some(first) = terms.next() else {
            return Ok(Expr::Int(0));
        };

        Ok(terms.fold(first, |left, right| Expr::Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }))
    }

    fn combine_add_terms(terms: &mut Vec<Expr>, budget: &mut EqvBudgetState) -> Result<(), String> {
        let constant: i64 = terms
            .iter()
            .filter_map(|term| match term {
                Expr::Int(value) => Some(*value),
                _ => None,
            })
            .sum();
        let original_len = terms.len();
        terms.retain(|term| !matches!(term, Expr::Int(_)));
        if constant != 0 {
            terms.push(Expr::Int(constant));
        }
        if terms.len() != original_len {
            budget.rewrite()?;
        }
        Ok(())
    }

    fn combine_mul_terms(terms: &mut Vec<Expr>, budget: &mut EqvBudgetState) -> Result<(), String> {
        if terms.iter().any(|term| matches!(term, Expr::Int(0))) {
            terms.clear();
            terms.push(Expr::Int(0));
            budget.rewrite()?;
            return Ok(());
        }

        let constant: i64 = terms
            .iter()
            .filter_map(|term| match term {
                Expr::Int(value) => Some(*value),
                _ => None,
            })
            .product();
        let had_constant = terms.iter().any(|term| matches!(term, Expr::Int(_)));
        let original_len = terms.len();
        terms.retain(|term| !matches!(term, Expr::Int(_)));
        if constant != 1 || terms.is_empty() {
            terms.push(Expr::Int(constant));
        }
        if had_constant && terms.len() != original_len {
            budget.rewrite()?;
        }
        Ok(())
    }

    fn collect_commutative_terms(op: crate::ast::BinOp, expr: Expr, terms: &mut Vec<Expr>) {
        match expr {
            Expr::Binary {
                left,
                op: expr_op,
                right,
            } if expr_op == op => {
                Self::collect_commutative_terms(op, *left, terms);
                Self::collect_commutative_terms(op, *right, terms);
            }
            other => terms.push(other),
        }
    }

    fn equivalence_node_count(expr: &Expr) -> usize {
        match expr {
            Expr::Binary { left, right, .. } => {
                1 + Self::equivalence_node_count(left) + Self::equivalence_node_count(right)
            }
            Expr::Unary { expr, .. }
            | Expr::UnitStrip { expr, .. }
            | Expr::UnitConvert { expr, .. }
            | Expr::UnitAttach { expr, .. }
            | Expr::MatrixTranspose { expr, .. }
            | Expr::Try(expr) => 1 + Self::equivalence_node_count(expr),
            Expr::Call { func, args } => {
                1 + Self::equivalence_node_count(func)
                    + args.iter().map(Self::equivalence_node_count).sum::<usize>()
            }
            Expr::Member { object, .. } => 1 + Self::equivalence_node_count(object),
            Expr::Index { object, index } => {
                1 + Self::equivalence_node_count(object) + Self::equivalence_node_count(index)
            }
            Expr::Array(values)
            | Expr::Vector(values)
            | Expr::Set(values)
            | Expr::Multi(values) => {
                1 + values
                    .iter()
                    .map(Self::equivalence_node_count)
                    .sum::<usize>()
            }
            Expr::Matrix(rows) => {
                1 + rows
                    .iter()
                    .flatten()
                    .map(Self::equivalence_node_count)
                    .sum::<usize>()
            }
            Expr::Struct(entries) => {
                1 + entries
                    .iter()
                    .map(|(_, value)| Self::equivalence_node_count(value))
                    .sum::<usize>()
            }
            Expr::Table(entries) => {
                1 + entries
                    .iter()
                    .map(|(key, value)| {
                        Self::equivalence_node_count(key) + Self::equivalence_node_count(value)
                    })
                    .sum::<usize>()
            }
            Expr::Range { start, end } => {
                1 + Self::equivalence_node_count(start) + Self::equivalence_node_count(end)
            }
            Expr::Lambda { .. } | Expr::Match { .. } | Expr::If { .. } => 1,
            Expr::Namespace { .. }
            | Expr::Int(_)
            | Expr::BigInt(_)
            | Expr::Float(_)
            | Expr::String(_)
            | Expr::Bool(_)
            | Expr::Null
            | Expr::Ident(_)
            | Expr::Wildcard => 1,
        }
    }
}

struct EqvBudgetState {
    budget: crate::builtin::utils::EqvBudget,
    rewrite_steps: usize,
}

impl EqvBudgetState {
    fn new(budget: crate::builtin::utils::EqvBudget) -> Self {
        Self {
            budget,
            rewrite_steps: 0,
        }
    }

    fn enter(&self, depth: usize) -> Result<(), String> {
        if depth > self.budget.max_rewrite_depth {
            return Err(
                "EqvBudgetExceeded: equivalence normalization depth budget exceeded".to_string(),
            );
        }
        Ok(())
    }

    fn rewrite(&mut self) -> Result<(), String> {
        self.rewrite_steps += 1;
        if self.rewrite_steps > self.budget.max_rewrite_steps {
            return Err(
                "EqvBudgetExceeded: equivalence normalization rewrite budget exceeded".to_string(),
            );
        }
        Ok(())
    }

    fn check_growth(&self, original_nodes: usize, normalized_nodes: usize) -> Result<(), String> {
        if normalized_nodes > original_nodes.saturating_mul(self.budget.max_node_growth_factor) {
            return Err(
                "EqvBudgetExceeded: equivalence normalization node growth budget exceeded"
                    .to_string(),
            );
        }
        Ok(())
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

    fn complex(real: i64, imaginary: i64) -> Expr {
        Expr::Binary {
            left: Box::new(Expr::Int(real)),
            op: BinOp::Add,
            right: Box::new(Expr::Binary {
                left: Box::new(Expr::Int(imaginary)),
                op: BinOp::Mul,
                right: Box::new(Expr::Ident("i".to_string())),
            }),
        }
    }

    #[test]
    fn conjugate_transpose_transposes_and_conjugates_complex_entries() {
        let expr = Expr::MatrixTranspose {
            expr: Box::new(Expr::Matrix(vec![
                vec![complex(1, 2), Expr::Int(3)],
                vec![complex(4, -5), complex(0, 6)],
            ])),
            conjugate: true,
        };
        let mut interpreter = new_interpreter();

        let result = interpreter.eval_expr(&expr).unwrap();

        let Value::Matrix(rows) = result else {
            panic!("Expected matrix result, got {result:?}");
        };
        assert_eq!(
            rows.borrow().as_slice(),
            &[
                vec![
                    Value::Complex(Box::new(Value::Int(1)), Box::new(Value::Int(-2))),
                    Value::Complex(Box::new(Value::Int(4)), Box::new(Value::Int(5))),
                ],
                vec![
                    Value::Int(3),
                    Value::Complex(Box::new(Value::Int(0)), Box::new(Value::Int(-6))),
                ],
            ]
        );
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
            param_types: vec![None],
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
