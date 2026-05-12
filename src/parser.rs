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

    fn declared_type_from_current_token(&self) -> Option<DeclaredType> {
        match self.current_token() {
            Token::TypeNum => Some(DeclaredType::Num),
            Token::TypeInt => Some(DeclaredType::Int),
            Token::TypeFloat => Some(DeclaredType::Float),
            Token::TypeBool => Some(DeclaredType::Bool),
            Token::TypeString => Some(DeclaredType::String),
            Token::TypeRational => Some(DeclaredType::Rational),
            Token::TypeIrrational => Some(DeclaredType::Irrational),
            Token::TypeComplex => Some(DeclaredType::Complex),
            Token::TypeArray => Some(DeclaredType::Array),
            Token::BigInt => Some(DeclaredType::BigInt),
            _ => None,
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
        // Split by decimal point
        let parts: Vec<&str> = decimal_str.split('.').collect();
        if parts.len() != 2 {
            return Err(format!("Invalid decimal format: {}", decimal_str));
        }

        let integer_part = parts[0];
        let fractional_part = parts[1];

        let num_decimal_places = fractional_part.len();
        let denominator = BigInt::from(10u8).pow_u32(num_decimal_places as u32);

        // Combine integer and fractional parts to create numerator
        let numerator_str = format!("{}{}", integer_part, fractional_part);
        let numerator_expr = self.integer_literal_expr(&numerator_str)?;
        let denominator_expr = self.integer_literal_expr(&denominator.to_string())?;

        // Create division expression: numerator / denominator
        // The division operation will automatically simplify to a rational
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
        if self.starts_type_first_declaration() {
            return Err(
                "Type-first declarations are not supported; use let|var|const name type = expr"
                    .to_string(),
            );
        }

        match self.current_token() {
            Token::Var => self.parse_var_decl_with_type(None, false),
            Token::Let => self.parse_var_decl_with_type(None, true),
            Token::Const => self.parse_var_decl_with_type(None, true),
            Token::BigInt
            | Token::TypeNum
            | Token::TypeInt
            | Token::TypeFloat
            | Token::TypeBool => self.parse_expression_statement(),
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
                    self.parse_expression_statement()
                }
            }
            Token::TypeRational | Token::TypeIrrational | Token::TypeComplex | Token::TypeArray => {
                self.parse_expression_statement()
            }
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
            Token::Import => self.parse_import(),
            Token::Use => self.parse_use(),
            Token::Try => self.parse_try_catch(),
            Token::LBrace => self.parse_block(),
            Token::Ident(_) => self.parse_expression_statement(),
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

    fn starts_type_first_declaration(&self) -> bool {
        matches!(
            self.current_token(),
            Token::BigInt
                | Token::TypeNum
                | Token::TypeInt
                | Token::TypeFloat
                | Token::TypeBool
                | Token::TypeString
                | Token::TypeRational
                | Token::TypeIrrational
                | Token::TypeComplex
                | Token::TypeArray
        ) && matches!(self.tokens.get(self.current + 1), Some(Token::Ident(_)))
            && self.tokens.get(self.current + 2) == Some(&Token::Equal)
    }

    fn parse_expression_statement(&mut self) -> Result<Stmt, String> {
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

        let declared_type = if declared_type.is_none() {
            let dtype = self.declared_type_from_current_token();
            if dtype.is_some() {
                self.advance();
            }
            dtype
        } else {
            declared_type
        };

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

        // 解析结构体字面量
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
        // Parse decorators (@decorator_name)
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

        // Expect func keyword after decorators
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
            // 解析更新部分作为语句（可以是赋值或表达式）
            // 尝试解析为表达式，然后检查是否是赋值
            let expr = self.parse_expression()?;

            // 检查是否是赋值
            let stmt = if self.current_token() == &Token::Equal {
                self.advance(); // 跳过 =
                let value = self.parse_expression()?;
                match expr {
                    Expr::Ident(name) => Stmt::Assign { name, value },
                    _ => return Err("Invalid assignment target in for update".to_string()),
                }
            } else {
                // 否则作为表达式语句
                Stmt::Expr(expr)
            };

            Some(Box::new(stmt))
        };
        self.expect(Token::RParen)?;

        // 解析循环体
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

    fn parse_module_path(&mut self) -> Result<Vec<String>, String> {
        let mut path = Vec::new();

        loop {
            if let Token::Ident(segment) = self.current_token() {
                path.push(segment.clone());
                self.advance();
            } else {
                return Err(format!(
                    "Expected module path segment, found {:?}",
                    self.current_token()
                ));
            }

            if self.current_token() != &Token::Dot
                || self.tokens.get(self.current + 1) == Some(&Token::LBrace)
            {
                break;
            }

            self.advance();
        }

        Ok(path)
    }

    fn parse_import(&mut self) -> Result<Stmt, String> {
        self.expect(Token::Import)?;
        let path = self.parse_module_path()?;
        let alias = if self.match_token(&Token::As) {
            if let Token::Ident(name) = self.current_token() {
                let alias = name.clone();
                self.advance();
                Some(alias)
            } else {
                return Err(format!(
                    "Expected import alias, found {:?}",
                    self.current_token()
                ));
            }
        } else {
            None
        };
        self.match_token(&Token::Semicolon);
        Ok(Stmt::Import { path, alias })
    }

    fn parse_use(&mut self) -> Result<Stmt, String> {
        self.expect(Token::Use)?;
        let path = self.parse_module_path()?;
        self.expect(Token::Dot)?;
        self.expect(Token::LBrace)?;

        let mut items = Vec::new();
        if self.current_token() != &Token::RBrace {
            loop {
                let name = self.parse_imported_symbol()?;

                let alias = if self.match_token(&Token::As) {
                    if let Token::Ident(alias) = self.current_token() {
                        let alias = alias.clone();
                        self.advance();
                        Some(alias)
                    } else {
                        return Err(format!(
                            "Expected imported symbol alias, found {:?}",
                            self.current_token()
                        ));
                    }
                } else {
                    None
                };

                items.push((name, alias));

                if !self.match_token(&Token::Comma) {
                    break;
                }
            }
        }

        self.expect(Token::RBrace)?;
        self.match_token(&Token::Semicolon);
        Ok(Stmt::Use { path, items })
    }

    fn parse_imported_symbol(&mut self) -> Result<String, String> {
        let name = match self.current_token() {
            Token::Ident(name) => name.clone(),
            Token::Var => "var".to_string(),
            _ => {
                return Err(format!(
                    "Expected imported symbol, found {:?}",
                    self.current_token()
                ));
            }
        };
        self.advance();
        Ok(name)
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
                Token::EqualEqualEqual => BinOp::Equivalent,
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
            Token::Not => {
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
                // Convert decimal string to rational (numerator/denominator)
                // e.g., "0.1" -> 1/10, "0.25" -> 25/100 -> 1/4
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
            // LSR-005: Allow type keywords to be used as function names
            Token::TypeInt => {
                self.advance();
                Ok(Expr::Ident("int".to_string()))
            }
            Token::TypeNum => {
                self.advance();
                Ok(Expr::Ident("num".to_string()))
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
                // 检查是否是命名空间访问
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
            Token::Vec => self.parse_vector(),
            Token::Table => self.parse_table(),
            Token::LBrace => self.parse_struct(),
            Token::LParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            Token::Do => self.parse_lambda(false),
            Token::Pipe => self.parse_lambda(true),
            Token::Match => self.parse_match(),
            _ => Err(format!("Unexpected token: {:?}", self.current_token())),
        }
    }

    fn parse_match(&mut self) -> Result<Expr, String> {
        self.expect(Token::Match)?;
        let target = self.parse_expression()?;
        self.expect(Token::LBrace)?;

        let mut arms = Vec::new();
        while self.current_token() != &Token::RBrace {
            let pattern = self.parse_match_pattern()?;
            let guard = if self.match_token(&Token::If) {
                Some(self.parse_expression()?)
            } else {
                None
            };
            self.expect(Token::FatArrow)?;
            let expr = self.parse_expression()?;
            arms.push(MatchArm { pattern, guard, expr });

            if !self.match_token(&Token::Comma) {
                break;
            }
        }

        self.expect(Token::RBrace)?;
        Ok(Expr::Match {
            target: Box::new(target),
            arms,
        })
    }

    fn parse_match_pattern(&mut self) -> Result<MatchPattern, String> {
        match self.current_token() {
            Token::Ident(name) if name == "_" => {
                self.advance();
                Ok(MatchPattern::Wildcard)
            }
            Token::Ident(name) => {
                let name = name.clone();
                self.advance();
                Ok(MatchPattern::Binding(name))
            }
            Token::Int(_) | Token::BigIntLiteral(_) | Token::Float(_) | Token::Decimal(_) | Token::String(_) | Token::True | Token::False | Token::Null => {
                Ok(MatchPattern::Literal(self.parse_primary()?))
            }
            _ => Err(format!("Expected match pattern, found {:?}", self.current_token())),
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

    fn parse_vector(&mut self) -> Result<Expr, String> {
        self.expect(Token::Vec)?;
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
        Ok(Expr::Vector(elements))
    }

    fn parse_table(&mut self) -> Result<Expr, String> {
        self.expect(Token::Table)?;
        self.expect(Token::LBrace)?;
        let mut entries = Vec::new();

        if self.current_token() != &Token::RBrace {
            loop {
                let key = self.parse_expression()?;
                self.expect(Token::FatArrow)?;
                let value = self.parse_expression()?;
                entries.push((key, value));

                if !self.match_token(&Token::Comma) {
                    break;
                }
            }
        }

        self.expect(Token::RBrace)?;
        Ok(Expr::Table(entries))
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
            self.expect(Token::Arrow)?;
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
