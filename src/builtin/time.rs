// 时间函数模块
use crate::value::Value;

pub fn time(_args: &[Value]) -> Result<Value, String> {
    use chrono::Local;
    let now = Local::now();
    Ok(Value::String(now.format("%H:%M:%S").to_string()))
}

pub fn date(_args: &[Value]) -> Result<Value, String> {
    use chrono::Local;
    let now = Local::now();
    Ok(Value::String(now.format("%Y-%m-%d").to_string()))
}
