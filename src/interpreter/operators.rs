/// Operator implementations for binary and unary operations
use std::cell::RefCell;
use std::rc::Rc;

use num::BigInt;
use num::BigRational;
use num::One;
use num::complex::Complex64;

use mathcore::MathCore;

use crate::ast::BinOp;
use crate::ast::UnaryOp;
use crate::value::{IrrationalValue, Value, irrational_to_float};

use super::Interpreter;

impl Interpreter {
    pub(super) fn compute_power(&self, base: f64, exponent: f64) -> Result<Value, String> {
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

    pub fn eval_binary_op(
        &mut self,
        left: &Value,
        op: BinOp,
        right: &Value,
    ) -> Result<Value, String> {
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
                    BinOp::Pow => {
                        // Try as int first, promote to BigInt on overflow
                        match a.checked_pow(*b as u32) {
                            Some(result) => Ok(Value::Int(result)),
                            None => {
                                // Overflow: promote to BigInt
                                let a_big = BigInt::from(*a);
                                Ok(Value::BigInt(a_big.pow(*b as u32)))
                            }
                        }
                    }
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
                            Ok(Value::BigInt(a.pow(exp)))
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
                                Ok(Value::BigInt(a.pow(*b as u32)))
                            } else {
                                // Negative exponent
                                let a_float = a.to_string().parse::<f64>().unwrap_or(0.0);
                                Ok(Value::Float(a_float.powf(*b as f64)))
                            }
                        } else {
                            // Int ^ BigInt
                            if let Ok(exp) = a.to_string().parse::<u32>() {
                                Ok(Value::BigInt(b_bigint.pow(exp)))
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
            (Value::BigInt(b), Value::Irrational(irr))
            | (Value::Irrational(irr), Value::BigInt(b)) => {
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

    pub fn eval_unary_op(&mut self, op: UnaryOp, val: &Value) -> Result<Value, String> {
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

    pub(super) fn multiply_irrationals(
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
}
