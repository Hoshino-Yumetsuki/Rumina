// 数学函数模块
use crate::value::{IrrationalValue, Value};
use mathcore::MathCore;
use num::BigInt;
use num::{One, Zero};

pub fn sqrt(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("sqrt expects 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => {
            if *n < 0 {
                // LSR-010: sqrt of negative returns imaginary number
                // sqrt(-n) = sqrt(n) * i
                let abs_n = n.abs();
                let sqrt_val = (abs_n as f64).sqrt();
                if sqrt_val.fract() == 0.0 {
                    // Perfect square: sqrt(-4) = 2i
                    Ok(Value::Complex(
                        Box::new(Value::Int(0)),
                        Box::new(Value::Int(sqrt_val as i64)),
                    ))
                } else {
                    // Irrational: sqrt(-2) = 0 + sqrt(2)*i
                    Ok(Value::Complex(
                        Box::new(Value::Int(0)),
                        Box::new(Value::Irrational(IrrationalValue::Sqrt(Box::new(
                            Value::Int(abs_n),
                        )))),
                    ))
                }
            } else {
                let sqrt_val = (*n as f64).sqrt();
                if sqrt_val.fract() == 0.0 {
                    Ok(Value::Int(sqrt_val as i64))
                } else {
                    Ok(Value::Irrational(IrrationalValue::Sqrt(Box::new(
                        Value::Int(*n),
                    ))))
                }
            }
        }
        Value::Float(f) => {
            if *f < 0.0 {
                // sqrt of negative float: return complex
                let abs_f = f.abs();
                Ok(Value::Complex(
                    Box::new(Value::Int(0)),
                    Box::new(Value::Float(abs_f.sqrt())),
                ))
            } else {
                Ok(Value::Float(f.sqrt()))
            }
        }
        Value::Irrational(irr) => {
            // Keep sqrt of irrational in symbolic form
            // sqrt(irrational) = sqrt(irrational)
            Ok(Value::Irrational(IrrationalValue::Sqrt(Box::new(
                Value::Irrational(irr.clone()),
            ))))
        }
        _ => Err(format!("sqrt expects number, got {}", args[0].type_name())),
    }
}

pub fn pi(_args: &[Value]) -> Result<Value, String> {
    Ok(Value::Irrational(IrrationalValue::Pi))
}

pub fn e(_args: &[Value]) -> Result<Value, String> {
    Ok(Value::Irrational(IrrationalValue::E))
}

pub fn sin(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("sin expects 1 argument".to_string());
    }
    let val = args[0].to_float()?;
    Ok(Value::Float(val.sin()))
}

pub fn cos(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("cos expects 1 argument".to_string());
    }
    let val = args[0].to_float()?;
    Ok(Value::Float(val.cos()))
}

pub fn tan(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("tan expects 1 argument".to_string());
    }
    let val = args[0].to_float()?;
    let math = MathCore::new();
    let expr = format!("tan({})", val);
    match math.calculate(&expr) {
        Ok(result) => Ok(Value::Float(result)),
        Err(e) => Err(format!("tan calculation error: {}", e)),
    }
}

pub fn exp(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("exp expects 1 argument".to_string());
    }
    let val = args[0].to_float()?;
    let math = MathCore::new();
    let expr = format!("exp({})", val);
    match math.calculate(&expr) {
        Ok(result) => Ok(Value::Float(result)),
        Err(e) => Err(format!("exp calculation error: {}", e)),
    }
}

pub fn abs_fn(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("abs expects 1 argument".to_string());
    }

    match &args[0] {
        Value::Int(n) => Ok(Value::Int(n.abs())),
        Value::Float(f) => Ok(Value::Float(f.abs())),
        Value::Irrational(irr) => {
            // abs of irrational: keep symbolic for positive values
            // For now, we'll convert to float since we can't determine sign symbolically
            use crate::value::irrational_to_float;
            let val = irrational_to_float(irr);
            if val < 0.0 {
                Ok(Value::Float(-val))
            } else {
                // Keep as irrational if positive
                Ok(Value::Irrational(irr.clone()))
            }
        }
        Value::Complex(re, im) => {
            // |a + bi| = sqrt(a^2 + b^2)
            let re_val = re.to_float().unwrap_or(0.0);
            let im_val = im.to_float().unwrap_or(0.0);
            let magnitude = (re_val * re_val + im_val * im_val).sqrt();

            // Check if result is integer
            if magnitude.fract() == 0.0 {
                Ok(Value::Int(magnitude as i64))
            } else {
                Ok(Value::Float(magnitude))
            }
        }
        _ => Err(format!("abs expects number, got {}", args[0].type_name())),
    }
}

// LSR-010: Complex number functions

// arg(z) - 返回复数的幅角（弧度）
pub fn arg(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("arg expects 1 argument".to_string());
    }

    match &args[0] {
        Value::Complex(re, im) => {
            let re_val = re.to_float().unwrap_or(0.0);
            let im_val = im.to_float().unwrap_or(0.0);
            Ok(Value::Float(im_val.atan2(re_val)))
        }
        Value::Int(n) => {
            // 实数的幅角是0（正数）或π（负数）
            if *n < 0 {
                Ok(Value::Irrational(IrrationalValue::Pi))
            } else {
                Ok(Value::Int(0))
            }
        }
        Value::Float(f) => {
            // 浮点数实数的幅角
            if *f < 0.0 {
                Ok(Value::Irrational(IrrationalValue::Pi))
            } else {
                Ok(Value::Int(0))
            }
        }
        _ => Err(format!(
            "arg expects complex or real number, got {}",
            args[0].type_name()
        )),
    }
}

// conj(z) - 返回复数的共轭
pub fn conj(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("conj expects 1 argument".to_string());
    }

    match &args[0] {
        Value::Complex(re, im) => {
            // conj(a + bi) = a - bi
            // 取虚部的负数
            match im.as_ref() {
                Value::Int(i) => Ok(Value::Complex(re.clone(), Box::new(Value::Int(-i)))),
                Value::Float(f) => Ok(Value::Complex(re.clone(), Box::new(Value::Float(-f)))),
                Value::Rational(r) => Ok(Value::Complex(re.clone(), Box::new(Value::Rational(-r)))),
                _ => {
                    // 对于其他类型，转换为float再取负
                    let im_val = im.to_float().unwrap_or(0.0);
                    Ok(Value::Complex(re.clone(), Box::new(Value::Float(-im_val))))
                }
            }
        }
        // 实数的共轭是它自己
        other => Ok(other.clone()),
    }
}

// re(z) - 返回复数的实部
pub fn re(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("re expects 1 argument".to_string());
    }

    match &args[0] {
        Value::Complex(re, _) => Ok(re.as_ref().clone()),
        // 实数的实部是它自己
        other => Ok(other.clone()),
    }
}

// im(z) - 返回复数的虚部
pub fn im(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("im expects 1 argument".to_string());
    }

    match &args[0] {
        Value::Complex(_, im) => Ok(im.as_ref().clone()),
        // 实数的虚部是0
        _ => Ok(Value::Int(0)),
    }
}

pub fn log(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("log expects 1 argument".to_string());
    }
    let val = args[0].to_float()?;
    Ok(Value::Float(val.ln()))
}

pub fn factorial(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("factorial expects 1 argument".to_string());
    }

    match &args[0] {
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
        Value::BigInt(n) => {
            if n < &BigInt::zero() {
                return Err("Factorial of negative number".to_string());
            }
            let mut result = BigInt::one();
            let mut i = BigInt::from(2);
            while &i <= n {
                result *= &i;
                i += 1;
            }
            Ok(Value::BigInt(result))
        }
        _ => Err(format!(
            "factorial expects integer, got {}",
            args[0].type_name()
        )),
    }
}
