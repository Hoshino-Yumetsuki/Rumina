/// LSR-000 Error handling module
/// Implements typed errors with stack traces
use std::fmt;

/// Lamina runtime error types as specified in LSR-000
#[derive(Debug, Clone, PartialEq)]
pub enum ErrorType {
    RuntimeError,
    TypeError,
    IndexError,
    KeyError,
    DivisionByZeroError,
    UndefinedVariableError,
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorType::RuntimeError => write!(f, "RuntimeError"),
            ErrorType::TypeError => write!(f, "TypeError"),
            ErrorType::IndexError => write!(f, "IndexError"),
            ErrorType::KeyError => write!(f, "KeyError"),
            ErrorType::DivisionByZeroError => write!(f, "DivisionByZeroError"),
            ErrorType::UndefinedVariableError => write!(f, "UndefinedVariableError"),
        }
    }
}

/// Stack frame for tracing call hierarchy
#[derive(Debug, Clone)]
pub struct StackFrame {
    pub function_name: String,
    pub file_name: String,
    pub line_number: Option<usize>,
}

/// Lamina runtime error with stack trace
#[derive(Debug, Clone)]
pub struct RuminaError {
    pub error_type: ErrorType,
    pub message: String,
    pub stack_trace: Vec<StackFrame>,
}

impl RuminaError {
    pub fn new(error_type: ErrorType, message: String) -> Self {
        RuminaError {
            error_type,
            message,
            stack_trace: Vec::new(),
        }
    }

    pub fn runtime(message: String) -> Self {
        Self::new(ErrorType::RuntimeError, message)
    }

    pub fn type_error(message: String) -> Self {
        Self::new(ErrorType::TypeError, message)
    }

    pub fn index_error(message: String) -> Self {
        Self::new(ErrorType::IndexError, message)
    }

    pub fn key_error(message: String) -> Self {
        Self::new(ErrorType::KeyError, message)
    }

    pub fn division_by_zero() -> Self {
        Self::new(
            ErrorType::DivisionByZeroError,
            "Division by zero".to_string(),
        )
    }

    pub fn undefined_variable(var_name: &str) -> Self {
        Self::new(
            ErrorType::UndefinedVariableError,
            format!("Undefined variable '{}'", var_name),
        )
    }

    /// Add a stack frame to the error
    pub fn add_frame(&mut self, frame: StackFrame) {
        self.stack_trace.push(frame);
    }

    /// Format error with stack trace (LSR-000 format)
    pub fn format_error(&self) -> String {
        let mut output = String::new();

        // Print traceback header
        if !self.stack_trace.is_empty() {
            output.push_str("Traceback (most recent call last):\n");

            // Print stack frames in reverse order (most recent first)
            for frame in self.stack_trace.iter().rev() {
                output.push_str(&format!(
                    "  File \"{}\", line {}, in {}\n",
                    frame.file_name,
                    frame.line_number.map_or("?".to_string(), |l| l.to_string()),
                    frame.function_name
                ));
            }
        }

        // Print error type and message
        output.push_str(&format!("{}: {}\n", self.error_type, self.message));

        output
    }
}

impl fmt::Display for RuminaError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.format_error())
    }
}

impl std::error::Error for RuminaError {}

/// Convert from String error to RuminaError (for backward compatibility)
impl From<String> for RuminaError {
    fn from(message: String) -> Self {
        // Try to infer error type from message
        if message.contains("type") || message.contains("Type") {
            RuminaError::type_error(message)
        } else if message.contains("index") || message.contains("Index") {
            RuminaError::index_error(message)
        } else if message.contains("key") || message.contains("Key") {
            RuminaError::key_error(message)
        } else if message.contains("division") || message.contains("Division by zero") {
            RuminaError::division_by_zero()
        } else if message.contains("not defined") || message.contains("Undefined") {
            RuminaError::runtime(message)
        } else {
            RuminaError::runtime(message)
        }
    }
}

/// Convert from &str error to RuminaError
impl From<&str> for RuminaError {
    fn from(message: &str) -> Self {
        RuminaError::from(message.to_string())
    }
}
