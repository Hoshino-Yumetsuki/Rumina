// WASM 接口模块
use crate::{Interpreter, Lexer, Parser};
use wasm_bindgen::prelude::*;

/// Rumina - 执行 Lamina 代码并返回结果
///
/// # 参数
/// - `code`: Lamina 源代码字符串
///
/// # 返回
/// - 成功：返回最后一个表达式的计算结果字符串，如果没有表达式则返回空字符串
/// - 失败：返回错误信息字符串（以 "Error: " 开头）
///
/// # 示例
/// ```javascript
/// import init, { rumina } from 'rumina';
/// await init();
///
/// const result = rumina('10 + 20');
/// console.log(result); // "30"
/// ```
#[wasm_bindgen]
pub fn rumina(code: &str) -> String {
    // 词法分析
    let mut lexer = Lexer::new(code.to_string());
    let tokens = lexer.tokenize();

    // 语法分析
    let mut parser = Parser::new(tokens);
    let statements = match parser.parse() {
        Ok(stmts) => stmts,
        Err(e) => return format!("Error: Parse error: {}", e),
    };

    // 创建解释器
    let mut interpreter = Interpreter::new();

    // 执行代码
    match interpreter.interpret(statements) {
        Ok(result) => {
            // 如果有返回值，转换为字符串；否则返回空字符串
            result.map(|v| v.to_string()).unwrap_or_default()
        }
        Err(e) => format!("Error: Runtime error: {}", e),
    }
}
