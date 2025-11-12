pub mod ast;
pub mod builtin;
pub mod error;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod value;
pub mod value_ops;
pub mod vm;
pub mod vm_ops;

// WASM 接口模块
#[cfg(target_arch = "wasm32")]
pub mod wasm;

pub use error::{ErrorType, RuminaError, StackFrame};
pub use interpreter::Interpreter;
pub use lexer::Lexer;
pub use parser::Parser;
pub use value::Value;
pub use vm::{ByteCode, OpCode, VM};
pub use vm_ops::VMOperations;

// Note: Compiler and ASTOptimizer implementations are in the `ruminac` package
// Note: The rumina library provides shared core functionality including VM
