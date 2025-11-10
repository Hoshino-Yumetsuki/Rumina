pub mod ast;
pub mod builtin;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod value;

pub use interpreter::Interpreter;
pub use lexer::Lexer;
pub use parser::Parser;

pub fn run(source: &str) -> Result<(), String> {
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    let mut interpreter = Interpreter::new();
    interpreter.interpret(ast)?;

    Ok(())
}
