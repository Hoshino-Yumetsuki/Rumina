/// AST节点定义
use std::fmt;

/// LSR-005: Declared types for type annotations
#[derive(Debug, Clone, PartialEq)]
pub enum DeclaredType {
    Int,
    Float,
    Bool,
    String,
    Rational,
    Irrational,
    Complex,
    Array,
    BigInt,
}

/// 语句
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    // 变量声明
    VarDecl {
        name: String,
        is_bigint: bool,                     // Deprecated: Use declared_type instead
        declared_type: Option<DeclaredType>, // LSR-005: Optional type annotation
        value: Expr,
    },

    // 赋值
    Assign {
        name: String,
        value: Expr,
    },

    // 成员赋值（obj.member = value）
    MemberAssign {
        object: Expr,
        member: String,
        value: Expr,
    },

    // 表达式语句
    Expr(Expr),

    // 函数定义
    FuncDef {
        name: String,
        params: Vec<String>,
        body: Vec<Stmt>,
    },

    // 返回语句
    Return(Option<Expr>),

    // If语句
    If {
        condition: Expr,
        then_branch: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
    },

    // While循环
    While {
        condition: Expr,
        body: Vec<Stmt>,
    },

    // For循环 (for (init; condition; update) { body })
    For {
        init: Option<Box<Stmt>>,   // 初始化语句 (var i = 0)
        condition: Option<Expr>,   // 条件表达式 (i < n)
        update: Option<Box<Stmt>>, // 更新语句 (i = i + 1)
        body: Vec<Stmt>,
    },

    // Loop循环
    Loop {
        body: Vec<Stmt>,
    },

    // Break
    Break,

    // Continue
    Continue,

    // Include模块
    Include(String),

    // 块语句
    Block(Vec<Stmt>),
}

/// 表达式
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // 字面量
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Null,

    // 标识符
    Ident(String),

    // 数组字面量
    Array(Vec<Expr>),

    // 结构体字面量
    Struct(Vec<(String, Expr)>),

    // 二元运算
    Binary {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },

    // 一元运算
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },

    // 函数调用
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },

    // 成员访问
    Member {
        object: Box<Expr>,
        member: String,
    },

    // 索引访问
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
    },

    // Lambda表达式
    Lambda {
        params: Vec<String>,
        body: Box<Stmt>,
        is_simple: bool, // 简单形式 |a, b| a + b
    },

    // 命名空间访问
    Namespace {
        module: String,
        name: String,
    },
}

/// 二元运算符
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
    Pow, // ^

    Equal,     // ==
    NotEqual,  // !=
    Greater,   // >
    GreaterEq, // >=
    Less,      // <
    LessEq,    // <=

    And, // &&
    Or,  // ||
}

/// 一元运算符
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,       // -
    Not,       // !
    Factorial, // ! (后缀)
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::Pow => write!(f, "^"),
            BinOp::Equal => write!(f, "=="),
            BinOp::NotEqual => write!(f, "!="),
            BinOp::Greater => write!(f, ">"),
            BinOp::GreaterEq => write!(f, ">="),
            BinOp::Less => write!(f, "<"),
            BinOp::LessEq => write!(f, "<="),
            BinOp::And => write!(f, "&&"),
            BinOp::Or => write!(f, "||"),
        }
    }
}
