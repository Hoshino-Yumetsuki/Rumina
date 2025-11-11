/// Token类型定义
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // 字面量
    Int(i64),
    Float(f64),
    String(String),
    True,
    False,
    Null,

    // 标识符
    Ident(String),

    // 关键字
    Var,
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
    Do,

    // LSR-005: Type keywords for type declarations
    TypeInt,
    TypeFloat,
    TypeBool,
    TypeString,
    TypeRational,
    TypeIrrational,
    TypeComplex,
    TypeArray,

    // 运算符
    Plus,    // +
    Minus,   // -
    Star,    // *
    Slash,   // /
    Percent, // %
    Caret,   // ^ (幂运算)
    Bang,    // ! (阶乘或逻辑非)

    // 比较运算符
    Equal,        // =
    EqualEqual,   // ==
    BangEqual,    // !=
    Greater,      // >
    GreaterEqual, // >=
    Less,         // <
    LessEqual,    // <=

    // 逻辑运算符
    And, // &&
    Or,  // ||

    // 分隔符
    Semicolon,   // ;
    Comma,       // ,
    Dot,         // .
    Colon,       // :
    DoubleColon, // ::
    Pipe,        // |
    Backslash,   // \ (续行符)

    // 括号
    LParen,   // (
    RParen,   // )
    LBrace,   // {
    RBrace,   // }
    LBracket, // [
    RBracket, // ]

    // 特殊
    Arrow, // ->
    Eof,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::Int(n) => write!(f, "{}", n),
            Token::Float(n) => write!(f, "{}", n),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Ident(s) => write!(f, "{}", s),
            _ => write!(f, "{:?}", self),
        }
    }
}
