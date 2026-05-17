use crate::ast::*;
use crate::numeric::{BigInt, BigIntExt, bigint_parse_bytes};
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

    /// Convert decimal string to rational expression (division)
    ///
    /// Converts decimal literals to exact rational representations:
    /// - "0.1" -> 1/10
    /// - "0.25" -> 1/4 (after simplification)
    /// - "3.14" -> 314/100 -> 157/50 (after simplification)
    ///
    /// Note: This function only receives positive decimal strings. Negative decimals
    /// like `-0.1` are handled by the parser as unary negation applied to the positive
    /// decimal, resulting in `Unary { op: Neg, expr: Decimal("0.1") }`.
    fn integer_literal_expr(&self, digits: &str) -> Result<Expr, String> {
        match digits.parse::<i64>() {
            Ok(n) => Ok(Expr::Int(n)),
            Err(_) => bigint_parse_bytes(digits.as_bytes(), 10)
                .map(Expr::BigInt)
                .ok_or_else(|| format!("Invalid integer literal: {}", digits)),
        }
    }

    fn decimal_to_rational(&self, decimal_str: &str) -> Result<Expr, String> {
        let parts: Vec<&str> = decimal_str.split('.').collect();
        if parts.len() != 2 {
            return Err(format!("Invalid decimal format: {}", decimal_str));
        }

        let integer_part = parts[0];
        let fractional_part = parts[1];

        let num_decimal_places = fractional_part.len();
        let denominator = BigInt::from(10u8).pow_u32(num_decimal_places as u32);

        let numerator_str = format!("{}{}", integer_part, fractional_part);
        let numerator_expr = self.integer_literal_expr(&numerator_str)?;
        let denominator_expr = self.integer_literal_expr(&denominator.to_string())?;

        // Division will automatically simplify to a rational
        Ok(Expr::Binary {
            left: Box::new(numerator_expr),
            op: BinOp::Div,
            right: Box::new(denominator_expr),
        })
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
            Token::Var => self.parse_var_decl_with_type(None, false),
            Token::Let => self.parse_var_decl_with_type(None, true),
            Token::BigInt => self.parse_var_decl_with_type(Some(DeclaredType::BigInt), false),
            // LSR-005: Type declaration support
            // But also check for namespace access (type::member)
            Token::TypeInt => self.parse_var_decl_with_type(Some(DeclaredType::Int), false),
            Token::TypeFloat => self.parse_var_decl_with_type(Some(DeclaredType::Float), false),
            Token::TypeBool => self.parse_var_decl_with_type(Some(DeclaredType::Bool), false),
            Token::TypeString => {
                // Check if this is namespace access (string::func) or type declaration (string x = ...)
                if self.tokens.get(self.current + 1) == Some(&Token::DoubleColon) {
                    // It's an expression statement like string::cat(...)
                    let expr = self.parse_expression()?;
                    if self.match_token(&Token::Equal) {
                        let value = self.parse_expression()?;
                        self.match_token(&Token::Semicolon);
                        match expr {
                            Expr::Ident(name) => Ok(Stmt::Assign { name, value }),
                            Expr::Member { object, member } => Ok(Stmt::MemberAssign {
                                object: *object,
                                member,
                                value,
                            }),
                            Expr::Namespace { .. } => Err("Cannot assign to namespace".to_string()),
                            _ => Err("Invalid assignment target".to_string()),
                        }
                    } else {
                        self.match_token(&Token::Semicolon);
                        Ok(Stmt::Expr(expr))
                    }
                } else {
                    self.parse_var_decl_with_type(Some(DeclaredType::String), false)
                }
            }
            Token::TypeRational => {
                self.parse_var_decl_with_type(Some(DeclaredType::Rational), false)
            }
            Token::TypeIrrational => {
                self.parse_var_decl_with_type(Some(DeclaredType::Irrational), false)
            }
            Token::TypeComplex => self.parse_var_decl_with_type(Some(DeclaredType::Complex), false),
            Token::TypeArray => self.parse_var_decl_with_type(Some(DeclaredType::Array), false),
            Token::Struct => self.parse_struct_decl(),
            Token::At => self.parse_decorated_func_def(), // LSR-011: Decorator support
            Token::Func => self.parse_func_def_with_decorators(Vec::new()),
            Token::Return => self.parse_return(),
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::For => self.parse_for(),
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
            Token::Try => self.parse_try_catch(),
            Token::LBrace => self.parse_block(),
            Token::Ident(_) => {
                // 可能是赋值、成员赋值或表达式语句
                let expr = self.parse_expression()?;

                if self.match_token(&Token::Equal) {
                    let value = self.parse_expression()?;
                    self.match_token(&Token::Semicolon);

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
                    self.match_token(&Token::Semicolon);
                    Ok(Stmt::Expr(expr))
                }
            }
            Token::Semicolon => {
                self.advance();
                Ok(Stmt::Empty)
            }
            _ => {
                let expr = self.parse_expression()?;
                self.match_token(&Token::Semicolon);
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_var_decl_with_type(
        &mut self,
        declared_type: Option<DeclaredType>,
        is_immutable: bool,
    ) -> Result<Stmt, String> {
        self.advance(); // 跳过类型关键字或var

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

        let is_bigint = matches!(declared_type, Some(DeclaredType::BigInt));

        if is_immutable {
            Ok(Stmt::LetDecl {
                name,
                is_bigint,
                declared_type,
                value,
            })
        } else {
            Ok(Stmt::VarDecl {
                name,
                is_bigint,
                declared_type,
                value,
            })
        }
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

        let value = self.parse_struct()?;
        // struct声明不需要分号
        Ok(Stmt::VarDecl {
            name,
            is_bigint: false,
            declared_type: None,
            value,
        })
    }

    fn parse_decorated_func_def(&mut self) -> Result<Stmt, String> {
        let mut decorators = Vec::new();
        while self.current_token() == &Token::At {
            self.advance(); // Skip @

            if let Token::Ident(name) = self.current_token() {
                decorators.push(name.clone());
                self.advance();
            } else {
                return Err(format!(
                    "Expected decorator name after @, found {:?}",
                    self.current_token()
                ));
            }
        }

        if self.current_token() != &Token::Func {
            return Err(format!(
                "Expected 'func' after decorator, found {:?}",
                self.current_token()
            ));
        }

        self.parse_func_def_with_decorators(decorators)
    }

    fn parse_func_def_with_decorators(&mut self, decorators: Vec<String>) -> Result<Stmt, String> {
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

        Ok(Stmt::FuncDef {
            name,
            params,
            body,
            decorators,
        })
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

    fn parse_for(&mut self) -> Result<Stmt, String> {
        self.advance(); // 跳过 for

        self.expect(Token::LParen)?;

        // 解析初始化语句 (可选)
        let init = if self.current_token() == &Token::Semicolon {
            None
        } else {
            Some(Box::new(self.parse_statement()?))
        };

        // 如果init不是以分号结尾的，需要consume分号
        if !matches!(self.current_token(), Token::Semicolon) {
            // init已经消费了分号，不需要再消费
        } else {
            self.advance(); // 跳过分号
        }

        // 解析条件表达式 (可选)
        let condition = if self.current_token() == &Token::Semicolon {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.expect(Token::Semicolon)?;

        // 解析更新语句 (可选) - 支持赋值等语句
        let update = if self.current_token() == &Token::RParen {
            None
        } else {
            let expr = self.parse_expression()?;

            let stmt = if self.current_token() == &Token::Equal {
                self.advance(); // 跳过 =
                let value = self.parse_expression()?;
                match expr {
                    Expr::Ident(name) => Stmt::Assign { name, value },
                    _ => return Err("Invalid assignment target in for update".to_string()),
                }
            } else {
                Stmt::Expr(expr)
            };

            Some(Box::new(stmt))
        };
        self.expect(Token::RParen)?;

        let body = if self.current_token() == &Token::LBrace {
            self.advance(); // 跳过 {
            let stmts = self.parse_block_statements()?;
            self.expect(Token::RBrace)?;
            stmts
        } else {
            // 单行for语句
            vec![self.parse_statement()?]
        };

        Ok(Stmt::For {
            init,
            condition,
            update,
            body,
        })
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

    fn parse_try_catch(&mut self) -> Result<Stmt, String> {
        self.expect(Token::Try)?;

        let try_block = self.parse_block()?;

        self.expect(Token::Catch)?;
        self.expect(Token::LParen)?;

        let error_name = if let Token::Ident(name) = self.current_token() {
            name.clone()
        } else {
            return Err(format!(
                "Expected catch binding name, found {:?}",
                self.current_token()
            ));
        };
        self.advance();

        self.expect(Token::RParen)?;
        let catch_block = self.parse_block()?;

        Ok(Stmt::TryCatch(
            Box::new(try_block),
            error_name,
            Box::new(catch_block),
        ))
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
        self.parse_pipeline()
    }

    fn parse_pipeline(&mut self) -> Result<Expr, String> {
        let left = self.parse_multi_value()?;

        if self.match_token(&Token::PipeForward) {
            let right = self.parse_pipeline()?;
            Ok(Expr::Binary {
                left: Box::new(left),
                op: BinOp::Pipe,
                right: Box::new(right),
            })
        } else {
            Ok(left)
        }
    }

    fn parse_multi_value(&mut self) -> Result<Expr, String> {
        let first = self.parse_or()?;

        if !matches!(self.current_token(), Token::Pipe) {
            return Ok(first);
        }

        let mut values = vec![first];
        while self.match_token(&Token::Pipe) {
            values.push(self.parse_or()?);
        }

        Ok(Expr::Multi(values))
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
                    self.advance();
                    let args = self.parse_arguments()?;
                    self.expect(Token::RParen)?;
                    expr = Expr::Call {
                        func: Box::new(expr),
                        args,
                    };
                }
                Token::LBracket => {
                    self.advance();
                    let index = self.parse_expression()?;
                    self.expect(Token::RBracket)?;
                    expr = Expr::Index {
                        object: Box::new(expr),
                        index: Box::new(index),
                    };
                }
                Token::Dot => {
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
                Token::Question => {
                    self.advance();
                    expr = Expr::Try(Box::new(expr));
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
            Token::BigIntLiteral(n) => {
                self.advance();
                Ok(Expr::BigInt(n))
            }
            Token::Float(f) => {
                self.advance();
                Ok(Expr::Float(f))
            }
            Token::Decimal(s) => {
                self.advance();
                self.decimal_to_rational(&s)
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
            // LSR-005: Allow type keywords to be used as function names
            Token::TypeInt => {
                self.advance();
                Ok(Expr::Ident("int".to_string()))
            }
            Token::TypeFloat => {
                self.advance();
                Ok(Expr::Ident("float".to_string()))
            }
            Token::TypeBool => {
                self.advance();
                Ok(Expr::Ident("bool".to_string()))
            }
            Token::TypeString => {
                self.advance();
                if self.match_token(&Token::DoubleColon) {
                    if let Token::Ident(member) = self.current_token() {
                        let member = member.clone();
                        self.advance();
                        Ok(Expr::Namespace {
                            module: "string".to_string(),
                            name: member,
                        })
                    } else {
                        Err(format!(
                            "Expected identifier after '::', found {:?}",
                            self.current_token()
                        ))
                    }
                } else {
                    Ok(Expr::Ident("string".to_string()))
                }
            }
            Token::TypeRational => {
                self.advance();
                Ok(Expr::Ident("rational".to_string()))
            }
            Token::TypeIrrational => {
                self.advance();
                Ok(Expr::Ident("irrational".to_string()))
            }
            Token::TypeComplex => {
                self.advance();
                Ok(Expr::Ident("complex".to_string()))
            }
            Token::TypeArray => {
                self.advance();
                Ok(Expr::Ident("array".to_string()))
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

        let mut params = Vec::new();
        if self.match_token(&Token::Pipe) {
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
