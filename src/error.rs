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

    pub fn runtime(message: impl Into<String>) -> Self {
        Self::new(ErrorType::RuntimeError, message.into())
    }

    pub fn type_error(message: impl Into<String>) -> Self {
        Self::new(ErrorType::TypeError, message.into())
    }

    pub fn index_error(message: impl Into<String>) -> Self {
        Self::new(ErrorType::IndexError, message.into())
    }

    pub fn key_error(message: impl Into<String>) -> Self {
        Self::new(ErrorType::KeyError, message.into())
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_type_display() {
        assert_eq!(ErrorType::RuntimeError.to_string(), "RuntimeError");
        assert_eq!(ErrorType::TypeError.to_string(), "TypeError");
        assert_eq!(ErrorType::IndexError.to_string(), "IndexError");
        assert_eq!(ErrorType::KeyError.to_string(), "KeyError");
        assert_eq!(
            ErrorType::DivisionByZeroError.to_string(),
            "DivisionByZeroError"
        );
        assert_eq!(
            ErrorType::UndefinedVariableError.to_string(),
            "UndefinedVariableError"
        );
    }

    #[test]
    fn test_rumina_error_new() {
        let err = RuminaError::new(ErrorType::RuntimeError, "test error".to_string());
        assert_eq!(err.error_type, ErrorType::RuntimeError);
        assert_eq!(err.message, "test error");
        assert!(err.stack_trace.is_empty());
    }

    #[test]
    fn test_error_constructors() {
        let err = RuminaError::runtime("runtime error");
        assert_eq!(err.error_type, ErrorType::RuntimeError);

        let err = RuminaError::type_error("type error");
        assert_eq!(err.error_type, ErrorType::TypeError);

        let err = RuminaError::index_error("index error");
        assert_eq!(err.error_type, ErrorType::IndexError);

        let err = RuminaError::key_error("key error");
        assert_eq!(err.error_type, ErrorType::KeyError);

        let err = RuminaError::division_by_zero();
        assert_eq!(err.error_type, ErrorType::DivisionByZeroError);
        assert_eq!(err.message, "Division by zero");

        let err = RuminaError::undefined_variable("x");
        assert_eq!(err.error_type, ErrorType::UndefinedVariableError);
        assert_eq!(err.message, "Undefined variable 'x'");
    }

    #[test]
    fn test_add_frame() {
        let mut err = RuminaError::runtime("test");
        let frame = StackFrame {
            function_name: "main".to_string(),
            file_name: "test.lm".to_string(),
            line_number: Some(10),
        };
        err.add_frame(frame);
        assert_eq!(err.stack_trace.len(), 1);
    }

    #[test]
    fn test_format_error_no_stack() {
        let err = RuminaError::runtime("test error");
        let formatted = err.format_error();
        assert!(formatted.contains("RuntimeError: test error"));
        assert!(!formatted.contains("Traceback"));
    }

    #[test]
    fn test_format_error_with_stack() {
        let mut err = RuminaError::type_error("invalid type");
        err.add_frame(StackFrame {
            function_name: "foo".to_string(),
            file_name: "test.lm".to_string(),
            line_number: Some(5),
        });
        err.add_frame(StackFrame {
            function_name: "bar".to_string(),
            file_name: "main.lm".to_string(),
            line_number: None,
        });

        let formatted = err.format_error();
        assert!(formatted.contains("Traceback (most recent call last)"));
        assert!(formatted.contains("File \"main.lm\", line ?, in bar"));
        assert!(formatted.contains("File \"test.lm\", line 5, in foo"));
        assert!(formatted.contains("TypeError: invalid type"));
    }

    #[test]
    fn test_display_trait() {
        let err = RuminaError::runtime("display test");
        let display_str = format!("{}", err);
        assert!(display_str.contains("RuntimeError: display test"));
    }

    #[test]
    fn test_from_string() {
        let err: RuminaError = "type mismatch".to_string().into();
        assert_eq!(err.error_type, ErrorType::TypeError);

        let err: RuminaError = "index out of bounds".to_string().into();
        assert_eq!(err.error_type, ErrorType::IndexError);

        let err: RuminaError = "key not found".to_string().into();
        assert_eq!(err.error_type, ErrorType::KeyError);

        let err: RuminaError = "Division by zero".to_string().into();
        assert_eq!(err.error_type, ErrorType::DivisionByZeroError);

        let err: RuminaError = "generic error".to_string().into();
        assert_eq!(err.error_type, ErrorType::RuntimeError);
    }

    #[test]
    fn test_from_str() {
        let err: RuminaError = "test error".into();
        assert_eq!(err.error_type, ErrorType::RuntimeError);
    }
}
