use crate::ast::*;
use crate::token::Token;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    fn current_token(&self) -> &Token {
        self.tokens.get(self.current).unwrap_or(&Token::Eof)
    }

    fn advance(&mut self) -> Token {
        let token = self.current_token().clone();
        if self.current < self.tokens.len() {
            self.current += 1;
        }
        token
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        if self.current_token() == &expected {
            self.advance();
            Ok(())
        } else {
            Err(format!(
                "Expected {:?}, found {:?}",
                expected,
                self.current_token()
            ))
        }
    }

    fn match_token(&mut self, token: &Token) -> bool {
        if self.current_token() == token {
            self.advance();
            true
        } else {
            false
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, String> {
        let mut statements = Vec::new();
        while self.current_token() != &Token::Eof {
            statements.push(self.parse_statement()?);
        }
        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Stmt, String> {
        match self.current_token() {
            Token::Var => self.parse_var_decl(false),
            Token::BigInt => self.parse_var_decl(true),
            Token::Struct => self.parse_struct_decl(),
            Token::Func => self.parse_func_def(),
            Token::Return => self.parse_return(),
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::Loop => self.parse_loop(),
            Token::Break => {
                self.advance();
                self.match_token(&Token::Semicolon);
                Ok(Stmt::Break)
            }
            Token::Continue => {
                self.advance();
                self.match_token(&Token::Semicolon);
                Ok(Stmt::Continue)
            }
            Token::Include => self.parse_include(),
            Token::LBrace => self.parse_block(),
            Token::Ident(_) => {
                // 可能是赋值、成员赋值或表达式语句
                // 先解析表达式（可能是标识符或成员访问）
                let expr = self.parse_expression()?;

                // 检查是否是赋值
                if self.match_token(&Token::Equal) {
                    let value = self.parse_expression()?;
                    self.match_token(&Token::Semicolon);

                    // 判断是简单赋值还是成员赋值
                    match expr {
                        Expr::Ident(name) => Ok(Stmt::Assign { name, value }),
                        Expr::Member { object, member } => Ok(Stmt::MemberAssign {
                            object: *object,
                            member,
                            value,
                        }),
                        _ => Err("Invalid assignment target".to_string()),
                    }
                } else {
                    // 表达式语句
                    self.match_token(&Token::Semicolon);
                    Ok(Stmt::Expr(expr))
                }
            }
            _ => {
                let expr = self.parse_expression()?;
                self.match_token(&Token::Semicolon);
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_var_decl(&mut self, is_bigint: bool) -> Result<Stmt, String> {
        self.advance(); // 跳过 var 或 bigint

        let name = if let Token::Ident(n) = self.current_token() {
            n.clone()
        } else {
            return Err(format!(
                "Expected identifier, found {:?}",
                self.current_token()
            ));
        };
        self.advance();

        self.expect(Token::Equal)?;
        let value = self.parse_expression()?;
        self.match_token(&Token::Semicolon);

        Ok(Stmt::VarDecl {
            name,
            is_bigint,
            value,
        })
    }

    fn parse_struct_decl(&mut self) -> Result<Stmt, String> {
        self.advance(); // 跳过 struct

        let name = if let Token::Ident(n) = self.current_token() {
            n.clone()
        } else {
            return Err(format!(
                "Expected struct name, found {:?}",
                self.current_token()
            ));
        };
        self.advance();

        // 解析结构体字面量
        let value = self.parse_struct()?;

        // struct声明不需要分号
        Ok(Stmt::VarDecl {
            name,
            is_bigint: false,
            value,
        })
    }

    fn parse_func_def(&mut self) -> Result<Stmt, String> {
        self.advance(); // 跳过 func

        let name = if let Token::Ident(n) = self.current_token() {
            n.clone()
        } else {
            return Err(format!(
                "Expected function name, found {:?}",
                self.current_token()
            ));
        };
        self.advance();

        // 参数列表（可选）
        let params = if self.match_token(&Token::LParen) {
            let mut params = Vec::new();
            if self.current_token() != &Token::RParen {
                loop {
                    if let Token::Ident(param) = self.current_token() {
                        params.push(param.clone());
                        self.advance();
                    } else {
                        return Err(format!(
                            "Expected parameter name, found {:?}",
                            self.current_token()
                        ));
                    }

                    if !self.match_token(&Token::Comma) {
                        break;
                    }
                }
            }
            self.expect(Token::RParen)?;
            params
        } else {
            Vec::new()
        };

        self.expect(Token::LBrace)?;
        let body = self.parse_block_statements()?;
        self.expect(Token::RBrace)?;

        Ok(Stmt::FuncDef { name, params, body })
    }

    fn parse_return(&mut self) -> Result<Stmt, String> {
        self.advance(); // 跳过 return

        if self.current_token() == &Token::Semicolon {
            self.advance();
            Ok(Stmt::Return(None))
        } else {
            let expr = self.parse_expression()?;
            self.match_token(&Token::Semicolon);
            Ok(Stmt::Return(Some(expr)))
        }
    }

    fn parse_if(&mut self) -> Result<Stmt, String> {
        self.advance(); // 跳过 if

        let condition = self.parse_expression()?;

        // 支持单行if语句（无大括号）或块if语句（有大括号）
        let then_branch = if self.current_token() == &Token::LBrace {
            self.advance(); // 跳过 {
            let stmts = self.parse_block_statements()?;
            self.expect(Token::RBrace)?;
            stmts
        } else {
            // 单行if语句
            vec![self.parse_statement()?]
        };

        let else_branch = if self.match_token(&Token::Else) {
            if self.current_token() == &Token::LBrace {
                self.advance(); // 跳过 {
                let else_stmts = self.parse_block_statements()?;
                self.expect(Token::RBrace)?;
                Some(else_stmts)
            } else {
                // 单行else语句
                Some(vec![self.parse_statement()?])
            }
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn parse_while(&mut self) -> Result<Stmt, String> {
        self.advance(); // 跳过 while

        let condition = self.parse_expression()?;

        // 支持单行while语句（无大括号）或块while语句（有大括号）
        let body = if self.current_token() == &Token::LBrace {
            self.advance(); // 跳过 {
            let stmts = self.parse_block_statements()?;
            self.expect(Token::RBrace)?;
            stmts
        } else {
            // 单行while语句
            vec![self.parse_statement()?]
        };

        Ok(Stmt::While { condition, body })
    }

    fn parse_loop(&mut self) -> Result<Stmt, String> {
        self.advance(); // 跳过 loop

        // 支持单行loop语句（无大括号）或块loop语句（有大括号）
        let body = if self.current_token() == &Token::LBrace {
            self.advance(); // 跳过 {
            let stmts = self.parse_block_statements()?;
            self.expect(Token::RBrace)?;
            stmts
        } else {
            // 单行loop语句
            vec![self.parse_statement()?]
        };

        Ok(Stmt::Loop { body })
    }

    fn parse_include(&mut self) -> Result<Stmt, String> {
        self.advance(); // 跳过 include

        let path = match self.current_token() {
            Token::String(p) => p.clone(),
            Token::Ident(p) => p.clone(),
            _ => {
                return Err(format!(
                    "Expected string or identifier, found {:?}",
                    self.current_token()
                ));
            }
        };
        self.advance();

        // 分号是可选的
        self.match_token(&Token::Semicolon);
        Ok(Stmt::Include(path))
    }

    fn parse_block(&mut self) -> Result<Stmt, String> {
        self.expect(Token::LBrace)?;
        let statements = self.parse_block_statements()?;
        self.expect(Token::RBrace)?;
        Ok(Stmt::Block(statements))
    }

    fn parse_block_statements(&mut self) -> Result<Vec<Stmt>, String> {
        let mut statements = Vec::new();
        while self.current_token() != &Token::RBrace && self.current_token() != &Token::Eof {
            statements.push(self.parse_statement()?);
        }
        Ok(statements)
    }

    // 表达式解析（优先级从低到高）
    fn parse_expression(&mut self) -> Result<Expr, String> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_and()?;

        while self.match_token(&Token::Or) {
            let right = self.parse_and()?;
            left = Expr::Binary {
                left: Box::new(left),
                op: BinOp::Or,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_and(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_equality()?;

        while self.match_token(&Token::And) {
            let right = self.parse_equality()?;
            left = Expr::Binary {
                left: Box::new(left),
                op: BinOp::And,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_comparison()?;

        loop {
            let op = match self.current_token() {
                Token::EqualEqual => BinOp::Equal,
                Token::BangEqual => BinOp::NotEqual,
                _ => break,
            };
            self.advance();
            let right = self.parse_comparison()?;
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_addition()?;

        loop {
            let op = match self.current_token() {
                Token::Greater => BinOp::Greater,
                Token::GreaterEqual => BinOp::GreaterEq,
                Token::Less => BinOp::Less,
                Token::LessEqual => BinOp::LessEq,
                _ => break,
            };
            self.advance();
            let right = self.parse_addition()?;
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_addition(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_multiplication()?;

        loop {
            let op = match self.current_token() {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplication()?;
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_multiplication(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_power()?;

        loop {
            let op = match self.current_token() {
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::Percent => BinOp::Mod,
                _ => break,
            };
            self.advance();
            let right = self.parse_power()?;
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_power(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_unary()?;

        if self.match_token(&Token::Caret) {
            let right = self.parse_power()?; // 右结合
            left = Expr::Binary {
                left: Box::new(left),
                op: BinOp::Pow,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, String> {
        match self.current_token() {
            Token::Minus => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::Unary {
                    op: UnaryOp::Neg,
                    expr: Box::new(expr),
                })
            }
            Token::Bang => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::Unary {
                    op: UnaryOp::Not,
                    expr: Box::new(expr),
                })
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary()?;

        loop {
            match self.current_token() {
                Token::LParen => {
                    // 函数调用
                    self.advance();
                    let args = self.parse_arguments()?;
                    self.expect(Token::RParen)?;
                    expr = Expr::Call {
                        func: Box::new(expr),
                        args,
                    };
                }
                Token::LBracket => {
                    // 索引访问
                    self.advance();
                    let index = self.parse_expression()?;
                    self.expect(Token::RBracket)?;
                    expr = Expr::Index {
                        object: Box::new(expr),
                        index: Box::new(index),
                    };
                }
                Token::Dot => {
                    // 成员访问
                    self.advance();
                    if let Token::Ident(member) = self.current_token() {
                        let member = member.clone();
                        self.advance();
                        expr = Expr::Member {
                            object: Box::new(expr),
                            member,
                        };
                    } else {
                        return Err(format!(
                            "Expected member name, found {:?}",
                            self.current_token()
                        ));
                    }
                }
                Token::Bang => {
                    // 阶乘（后缀）
                    self.advance();
                    expr = Expr::Unary {
                        op: UnaryOp::Factorial,
                        expr: Box::new(expr),
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expr>, String> {
        let mut args = Vec::new();

        if self.current_token() != &Token::RParen {
            loop {
                args.push(self.parse_expression()?);
                if !self.match_token(&Token::Comma) {
                    break;
                }
            }
        }

        Ok(args)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.current_token().clone() {
            Token::Int(n) => {
                self.advance();
                Ok(Expr::Int(n))
            }
            Token::Float(f) => {
                self.advance();
                Ok(Expr::Float(f))
            }
            Token::String(s) => {
                self.advance();
                Ok(Expr::String(s))
            }
            Token::True => {
                self.advance();
                Ok(Expr::Bool(true))
            }
            Token::False => {
                self.advance();
                Ok(Expr::Bool(false))
            }
            Token::Null => {
                self.advance();
                Ok(Expr::Null)
            }
            Token::Ident(name) => {
                self.advance();
                // 检查是否是命名空间访问
                if self.match_token(&Token::DoubleColon) {
                    if let Token::Ident(member) = self.current_token() {
                        let member = member.clone();
                        self.advance();
                        Ok(Expr::Namespace {
                            module: name,
                            name: member,
                        })
                    } else {
                        Err(format!(
                            "Expected identifier after '::', found {:?}",
                            self.current_token()
                        ))
                    }
                } else {
                    Ok(Expr::Ident(name))
                }
            }
            Token::LBracket => self.parse_array(),
            Token::LBrace => self.parse_struct(),
            Token::LParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            Token::Do => self.parse_lambda(false),
            Token::Pipe => self.parse_lambda(true),
            _ => Err(format!("Unexpected token: {:?}", self.current_token())),
        }
    }

    fn parse_array(&mut self) -> Result<Expr, String> {
        self.expect(Token::LBracket)?;
        let mut elements = Vec::new();

        if self.current_token() != &Token::RBracket {
            loop {
                elements.push(self.parse_expression()?);
                if !self.match_token(&Token::Comma) {
                    break;
                }
            }
        }

        self.expect(Token::RBracket)?;
        Ok(Expr::Array(elements))
    }

    fn parse_struct(&mut self) -> Result<Expr, String> {
        self.expect(Token::LBrace)?;
        let mut fields = Vec::new();

        while self.current_token() != &Token::RBrace && self.current_token() != &Token::Eof {
            let name = if let Token::Ident(n) = self.current_token() {
                n.clone()
            } else {
                return Err(format!(
                    "Expected field name, found {:?}",
                    self.current_token()
                ));
            };
            self.advance();

            self.expect(Token::Equal)?;
            let value = self.parse_expression()?;
            fields.push((name, value));

            // 字段后可以有分号或逗号，也可以没有
            if !self.match_token(&Token::Semicolon) {
                self.match_token(&Token::Comma);
            }

            // 如果下一个是右大括号，退出循环
            if self.current_token() == &Token::RBrace {
                break;
            }
        }

        self.expect(Token::RBrace)?;
        Ok(Expr::Struct(fields))
    }

    fn parse_lambda(&mut self, is_simple: bool) -> Result<Expr, String> {
        if !is_simple {
            self.expect(Token::Do)?;
        }

        // 检查是否有参数列表（以 | 开始）
        let mut params = Vec::new();
        if self.match_token(&Token::Pipe) {
            // 有参数列表
            if self.current_token() != &Token::Pipe {
                loop {
                    if let Token::Ident(param) = self.current_token() {
                        params.push(param.clone());
                        self.advance();
                    } else {
                        return Err(format!(
                            "Expected parameter name, found {:?}",
                            self.current_token()
                        ));
                    }

                    if !self.match_token(&Token::Comma) {
                        break;
                    }
                }
            }
            self.expect(Token::Pipe)?;
        }

        let body = if is_simple {
            // 简单形式: |a, b| a + b
            let expr = self.parse_expression()?;
            Box::new(Stmt::Return(Some(expr)))
        } else {
            // 完整形式: do |a, b| { ... } 或 do { ... }
            self.expect(Token::LBrace)?;
            let stmts = self.parse_block_statements()?;
            self.expect(Token::RBrace)?;
            Box::new(Stmt::Block(stmts))
        };

        Ok(Expr::Lambda {
            params,
            body,
            is_simple,
        })
    }
}
