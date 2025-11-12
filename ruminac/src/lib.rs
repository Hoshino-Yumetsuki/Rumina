// Re-export compiler-related types from rumina for external library use
pub use rumina::{ASTOptimizer, Compiler, Lexer, Parser, RuminaError, Value, ByteCode};

/// Compile Lamina source code to bytecode
pub fn compile(source: &str) -> Result<ByteCode, RuminaError> {
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize();
    
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().map_err(|e| RuminaError::runtime(e))?;
    
    let mut compiler = Compiler::new();
    compiler.compile(ast)
}

/// Compile Lamina source code to bytecode string (serialized)
pub fn compile_to_string(source: &str) -> Result<String, RuminaError> {
    let bytecode = compile(source)?;
    Ok(bytecode.serialize())
}

/// Compile a .lm file to bytecode string
pub fn compile_file(filename: &str) -> Result<String, RuminaError> {
    let source = std::fs::read_to_string(filename)
        .map_err(|e| RuminaError::runtime(format!("Error reading file '{}': {}", filename, e)))?;
    compile_to_string(&source)
}
