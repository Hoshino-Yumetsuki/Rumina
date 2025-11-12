// Re-export VM-related types from rumina for external library use
pub use rumina::{ByteCode, Interpreter, RuminaError, VM, Value};

/// Run bytecode from a string
pub fn run_bytecode(bytecode_str: &str) -> Result<Option<Value>, RuminaError> {
    let bytecode = ByteCode::deserialize(bytecode_str)?;
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    let mut vm = VM::new(globals);
    vm.load(bytecode);
    vm.run()
}

/// Load and run bytecode from a file
pub fn run_bytecode_file(filename: &str) -> Result<Option<Value>, RuminaError> {
    let contents = std::fs::read_to_string(filename)
        .map_err(|e| RuminaError::runtime(format!("Error reading file '{}': {}", filename, e)))?;
    run_bytecode(&contents)
}
