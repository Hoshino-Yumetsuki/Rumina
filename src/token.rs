use crate::numeric::BigInt;

/// Token类型定义
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // 字面量
    Int(i64),
    BigIntLiteral(BigInt),
    Float(f64),
    Decimal(String), // Decimal literal like "0.1" - will be converted to rational
    String(String),
    True,
    False,
    Null,

    // 标识符
    Ident(String),

    // 关键字
    Var,
    Let,
    Const,
    BigInt,
    Struct,
    Func,
    Return,
    If,
    Else,
    While,
    For,
    Loop,
    Break,
    Continue,
    Include,
    Import,
    Use,
    As,
    Do,
    Try,
    Catch,
    Match,
    Table,
    Vec,

    // LSR-005: Type keywords for type declarations
    TypeInt,
    TypeNum,
    TypeFloat,
    TypeBool,
    TypeString,
    TypeRational,
    TypeIrrational,
    TypeComplex,
    TypeArray,

    // 运算符
    Plus,     // +
    Minus,    // -
    Star,     // *
    Slash,    // /
    Percent,  // %
    Caret,    // ^ (幂运算)
    Bang,     // !
    Question, // ?

    // 比较运算符
    Equal,        // =
    EqualEqualEqual, // ===
    EqualEqual,   // ==
    BangEqual,    // !=
    Greater,      // >
    GreaterEqual, // >=
    Less,         // <
    LessEqual,    // <=

    // 逻辑运算符
    And,
    Or,
    Not,

    // 分隔符
    Semicolon,   // ;
    Comma,       // ,
    Dot,         // .
    Colon,       // :
    DoubleColon, // ::
    Pipe,        // |
    PipeForward, // |>
    Backslash,   // \ (续行符)

    // 括号
    LParen,   // (
    RParen,   // )
    LBrace,   // {
    RBrace,   // }
    LBracket, // [
    RBracket, // ]

    // 特殊
    Arrow,    // ->
    FatArrow, // =>
    At,       // @ (decorator)
    Eof,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::Int(n) => write!(f, "{}", n),
            Token::BigIntLiteral(n) => write!(f, "{}", n),
            Token::Float(n) => write!(f, "{}", n),
            Token::Decimal(s) => write!(f, "{}", s),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Try => write!(f, "try"),
            Token::Catch => write!(f, "catch"),
            Token::Question => write!(f, "?"),
            _ => write!(f, "{:?}", self),
        }
    }
}

#[cfg(test)]
#[allow(clippy::approx_constant)]
mod tests {
    use super::*;

    #[test]
    fn test_token_display() {
        assert_eq!(Token::Int(42).to_string(), "42");
        assert_eq!(Token::BigIntLiteral(BigInt::from(42)).to_string(), "42");
        assert_eq!(Token::Float(3.14).to_string(), "3.14");
        assert_eq!(Token::Decimal("0.1".to_string()).to_string(), "0.1");
        assert_eq!(Token::String("hello".to_string()).to_string(), "\"hello\"");
        assert_eq!(Token::Ident("x".to_string()).to_string(), "x");
        assert_eq!(Token::Try.to_string(), "try");
        assert_eq!(Token::Catch.to_string(), "catch");
        assert_eq!(Token::Question.to_string(), "?");
    }

    #[test]
    fn test_token_clone() {
        let t = Token::Int(42);
        assert_eq!(t.clone(), Token::Int(42));
        let t = Token::BigIntLiteral(BigInt::from(42));
        assert_eq!(t.clone(), Token::BigIntLiteral(BigInt::from(42)));
    }
}
