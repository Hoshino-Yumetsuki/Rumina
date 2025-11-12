/// Optimization passes for AST and bytecode
///
/// This module implements various optimization techniques:
/// - Constant folding: Evaluate constant expressions at compile time
/// - Dead code elimination: Remove unreachable code
use crate::ast::*;
use crate::error::RuminaError;

/// Optimizer for AST transformations
pub struct ASTOptimizer {
    /// Track if any optimizations were applied
    modified: bool,
}

impl ASTOptimizer {
    pub fn new() -> Self {
        ASTOptimizer { modified: false }
    }

    /// Optimize a list of statements
    pub fn optimize(&mut self, statements: Vec<Stmt>) -> Result<Vec<Stmt>, RuminaError> {
        let mut optimized = Vec::new();
        
        for stmt in statements {
            if let Some(opt_stmt) = self.optimize_stmt(stmt)? {
                optimized.push(opt_stmt);
            }
        }
        
        Ok(optimized)
    }

    /// Optimize a single statement
    /// Returns None if the statement should be eliminated
    fn optimize_stmt(&mut self, stmt: Stmt) -> Result<Option<Stmt>, RuminaError> {
        match stmt {
            Stmt::Expr(expr) => {
                let opt_expr = self.optimize_expr(expr)?;
                Ok(Some(Stmt::Expr(opt_expr)))
            }

            Stmt::VarDecl { name, value, is_bigint, declared_type } => {
                let opt_value = self.optimize_expr(value)?;
                Ok(Some(Stmt::VarDecl {
                    name,
                    value: opt_value,
                    is_bigint,
                    declared_type,
                }))
            }

            Stmt::Assign { name, value } => {
                let opt_value = self.optimize_expr(value)?;
                Ok(Some(Stmt::Assign {
                    name,
                    value: opt_value,
                }))
            }

            Stmt::Block(stmts) => {
                let mut opt_stmts = Vec::new();
                let mut has_return = false;

                for stmt in stmts {
                    if has_return {
                        // Dead code after return - eliminate it
                        self.modified = true;
                        continue;
                    }

                    if let Some(opt_stmt) = self.optimize_stmt(stmt)? {
                        // Check if this is a return statement
                        if matches!(opt_stmt, Stmt::Return(_)) {
                            has_return = true;
                        }
                        opt_stmts.push(opt_stmt);
                    }
                }

                if opt_stmts.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(Stmt::Block(opt_stmts)))
                }
            }

            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let opt_condition = self.optimize_expr(condition)?;

                // Constant folding for if conditions
                if let Expr::Bool(b) = opt_condition {
                    self.modified = true;
                    if b {
                        // Condition is always true, keep only then branch
                        let opt_then = self.optimize_stmts(then_branch)?;
                        if opt_then.is_empty() {
                            Ok(None)
                        } else {
                            Ok(Some(Stmt::Block(opt_then)))
                        }
                    } else {
                        // Condition is always false, keep only else branch
                        if let Some(else_stmts) = else_branch {
                            let opt_else = self.optimize_stmts(else_stmts)?;
                            if opt_else.is_empty() {
                                Ok(None)
                            } else {
                                Ok(Some(Stmt::Block(opt_else)))
                            }
                        } else {
                            Ok(None)
                        }
                    }
                } else {
                    let opt_then = self.optimize_stmts(then_branch)?;
                    let opt_else = if let Some(else_stmts) = else_branch {
                        let opt = self.optimize_stmts(else_stmts)?;
                        if opt.is_empty() {
                            None
                        } else {
                            Some(opt)
                        }
                    } else {
                        None
                    };

                    Ok(Some(Stmt::If {
                        condition: opt_condition,
                        then_branch: opt_then,
                        else_branch: opt_else,
                    }))
                }
            }

            Stmt::While { condition, body } => {
                let opt_condition = self.optimize_expr(condition)?;

                // Constant folding for while conditions
                if let Expr::Bool(false) = opt_condition {
                    // Loop never executes - dead code
                    self.modified = true;
                    Ok(None)
                } else {
                    let opt_body = self.optimize_stmts(body)?;
                    Ok(Some(Stmt::While {
                        condition: opt_condition,
                        body: opt_body,
                    }))
                }
            }

            Stmt::For {
                init,
                condition,
                update,
                body,
            } => {
                let opt_init = if let Some(i) = init {
                    self.optimize_stmt(*i)?.map(Box::new)
                } else {
                    None
                };

                let opt_condition = if let Some(c) = condition {
                    Some(self.optimize_expr(c)?)
                } else {
                    None
                };

                let opt_update = if let Some(u) = update {
                    self.optimize_stmt(*u)?.map(Box::new)
                } else {
                    None
                };

                let opt_body = self.optimize_stmts(body)?;

                Ok(Some(Stmt::For {
                    init: opt_init,
                    condition: opt_condition,
                    update: opt_update,
                    body: opt_body,
                }))
            }

            Stmt::Return(expr) => {
                let opt_expr = if let Some(e) = expr {
                    Some(self.optimize_expr(e)?)
                } else {
                    None
                };
                Ok(Some(Stmt::Return(opt_expr)))
            }

            Stmt::FuncDef {
                name,
                params,
                body,
                decorators,
            } => {
                let opt_body = self.optimize_stmts(body)?;
                Ok(Some(Stmt::FuncDef {
                    name,
                    params,
                    body: opt_body,
                    decorators,
                }))
            }

            // Break and Continue are kept as-is
            stmt => Ok(Some(stmt)),
        }
    }

    /// Optimize a list of statements
    fn optimize_stmts(&mut self, stmts: Vec<Stmt>) -> Result<Vec<Stmt>, RuminaError> {
        let mut optimized = Vec::new();
        
        for stmt in stmts {
            if let Some(opt_stmt) = self.optimize_stmt(stmt)? {
                optimized.push(opt_stmt);
            }
        }
        
        Ok(optimized)
    }

    /// Optimize an expression (constant folding)
    fn optimize_expr(&mut self, expr: Expr) -> Result<Expr, RuminaError> {
        match expr {
            Expr::Binary { left, op, right } => {
                let opt_left = self.optimize_expr(*left)?;
                let opt_right = self.optimize_expr(*right)?;

                // Constant folding for binary operations
                match (&opt_left, &op, &opt_right) {
                    // Integer arithmetic
                    (Expr::Int(a), BinOp::Add, Expr::Int(b)) => {
                        self.modified = true;
                        Ok(Expr::Int(a + b))
                    }
                    (Expr::Int(a), BinOp::Sub, Expr::Int(b)) => {
                        self.modified = true;
                        Ok(Expr::Int(a - b))
                    }
                    (Expr::Int(a), BinOp::Mul, Expr::Int(b)) => {
                        self.modified = true;
                        Ok(Expr::Int(a * b))
                    }
                    (Expr::Int(a), BinOp::Div, Expr::Int(b)) if *b != 0 => {
                        self.modified = true;
                        Ok(Expr::Int(a / b))
                    }
                    (Expr::Int(a), BinOp::Mod, Expr::Int(b)) if *b != 0 => {
                        self.modified = true;
                        Ok(Expr::Int(a % b))
                    }

                    // Float arithmetic
                    (Expr::Float(a), BinOp::Add, Expr::Float(b)) => {
                        self.modified = true;
                        Ok(Expr::Float(a + b))
                    }
                    (Expr::Float(a), BinOp::Sub, Expr::Float(b)) => {
                        self.modified = true;
                        Ok(Expr::Float(a - b))
                    }
                    (Expr::Float(a), BinOp::Mul, Expr::Float(b)) => {
                        self.modified = true;
                        Ok(Expr::Float(a * b))
                    }
                    (Expr::Float(a), BinOp::Div, Expr::Float(b)) if *b != 0.0 => {
                        self.modified = true;
                        Ok(Expr::Float(a / b))
                    }

                    // Boolean operations
                    (Expr::Bool(a), BinOp::And, Expr::Bool(b)) => {
                        self.modified = true;
                        Ok(Expr::Bool(*a && *b))
                    }
                    (Expr::Bool(a), BinOp::Or, Expr::Bool(b)) => {
                        self.modified = true;
                        Ok(Expr::Bool(*a || *b))
                    }

                    // Comparison operations
                    (Expr::Int(a), BinOp::Equal, Expr::Int(b)) => {
                        self.modified = true;
                        Ok(Expr::Bool(a == b))
                    }
                    (Expr::Int(a), BinOp::NotEqual, Expr::Int(b)) => {
                        self.modified = true;
                        Ok(Expr::Bool(a != b))
                    }
                    (Expr::Int(a), BinOp::Greater, Expr::Int(b)) => {
                        self.modified = true;
                        Ok(Expr::Bool(a > b))
                    }
                    (Expr::Int(a), BinOp::GreaterEq, Expr::Int(b)) => {
                        self.modified = true;
                        Ok(Expr::Bool(a >= b))
                    }
                    (Expr::Int(a), BinOp::Less, Expr::Int(b)) => {
                        self.modified = true;
                        Ok(Expr::Bool(a < b))
                    }
                    (Expr::Int(a), BinOp::LessEq, Expr::Int(b)) => {
                        self.modified = true;
                        Ok(Expr::Bool(a <= b))
                    }

                    // String concatenation
                    (Expr::String(a), BinOp::Add, Expr::String(b)) => {
                        self.modified = true;
                        Ok(Expr::String(format!("{}{}", a, b)))
                    }

                    // Algebraic simplifications
                    (_, BinOp::Mul, Expr::Int(0)) | (Expr::Int(0), BinOp::Mul, _) => {
                        self.modified = true;
                        Ok(Expr::Int(0))
                    }
                    (expr, BinOp::Mul, Expr::Int(1)) | (Expr::Int(1), BinOp::Mul, expr) => {
                        self.modified = true;
                        Ok(expr.clone())
                    }
                    (expr, BinOp::Add, Expr::Int(0)) | (Expr::Int(0), BinOp::Add, expr) => {
                        self.modified = true;
                        Ok(expr.clone())
                    }
                    (expr, BinOp::Sub, Expr::Int(0)) => {
                        self.modified = true;
                        Ok(expr.clone())
                    }

                    // No optimization possible
                    _ => Ok(Expr::Binary {
                        left: Box::new(opt_left),
                        op,
                        right: Box::new(opt_right),
                    }),
                }
            }

            Expr::Unary { op, expr } => {
                let opt_expr = self.optimize_expr(*expr)?;

                match (&op, &opt_expr) {
                    (UnaryOp::Neg, Expr::Int(n)) => {
                        self.modified = true;
                        Ok(Expr::Int(-n))
                    }
                    (UnaryOp::Neg, Expr::Float(f)) => {
                        self.modified = true;
                        Ok(Expr::Float(-f))
                    }
                    (UnaryOp::Not, Expr::Bool(b)) => {
                        self.modified = true;
                        Ok(Expr::Bool(!b))
                    }
                    _ => Ok(Expr::Unary {
                        op,
                        expr: Box::new(opt_expr),
                    }),
                }
            }

            Expr::Array(elements) => {
                let mut opt_elements = Vec::new();
                for elem in elements {
                    opt_elements.push(self.optimize_expr(elem)?);
                }
                Ok(Expr::Array(opt_elements))
            }

            Expr::Call { func, args } => {
                let opt_func = Box::new(self.optimize_expr(*func)?);
                let mut opt_args = Vec::new();
                for arg in args {
                    opt_args.push(self.optimize_expr(arg)?);
                }
                Ok(Expr::Call {
                    func: opt_func,
                    args: opt_args,
                })
            }

            Expr::Index { object, index } => {
                let opt_object = Box::new(self.optimize_expr(*object)?);
                let opt_index = Box::new(self.optimize_expr(*index)?);
                Ok(Expr::Index {
                    object: opt_object,
                    index: opt_index,
                })
            }

            Expr::Member { object, member } => {
                let opt_object = Box::new(self.optimize_expr(*object)?);
                Ok(Expr::Member {
                    object: opt_object,
                    member,
                })
            }

            Expr::Lambda { params, body, is_simple } => {
                // Lambda body is a single Stmt, not Vec<Stmt>
                let opt_body = if let Some(opt_stmt) = self.optimize_stmt(*body)? {
                    opt_stmt
                } else {
                    // If body was eliminated, keep original (should not happen in practice)
                    Stmt::Block(vec![])
                };
                Ok(Expr::Lambda {
                    params,
                    body: Box::new(opt_body),
                    is_simple,
                })
            }

            Expr::Struct(fields) => {
                let mut opt_fields = Vec::new();
                for (key, value) in fields {
                    opt_fields.push((key, self.optimize_expr(value)?));
                }
                Ok(Expr::Struct(opt_fields))
            }

            // Literals are already optimized
            expr => Ok(expr),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constant_folding_arithmetic() {
        let mut optimizer = ASTOptimizer::new();

        // 2 + 3 should become 5
        let expr = Expr::Binary {
            left: Box::new(Expr::Int(2)),
            op: BinOp::Add,
            right: Box::new(Expr::Int(3)),
        };

        let result = optimizer.optimize_expr(expr).unwrap();
        assert_eq!(result, Expr::Int(5));
        assert!(optimizer.modified);
    }

    #[test]
    fn test_constant_folding_multiplication() {
        let mut optimizer = ASTOptimizer::new();

        // 4 * 5 should become 20
        let expr = Expr::Binary {
            left: Box::new(Expr::Int(4)),
            op: BinOp::Mul,
            right: Box::new(Expr::Int(5)),
        };

        let result = optimizer.optimize_expr(expr).unwrap();
        assert_eq!(result, Expr::Int(20));
    }

    #[test]
    fn test_algebraic_simplification_multiply_by_zero() {
        let mut optimizer = ASTOptimizer::new();

        // x * 0 should become 0
        let expr = Expr::Binary {
            left: Box::new(Expr::Ident("x".to_string())),
            op: BinOp::Mul,
            right: Box::new(Expr::Int(0)),
        };

        let result = optimizer.optimize_expr(expr).unwrap();
        assert_eq!(result, Expr::Int(0));
        assert!(optimizer.modified);
    }

    #[test]
    fn test_algebraic_simplification_multiply_by_one() {
        let mut optimizer = ASTOptimizer::new();

        // x * 1 should become x
        let expr = Expr::Binary {
            left: Box::new(Expr::Ident("x".to_string())),
            op: BinOp::Mul,
            right: Box::new(Expr::Int(1)),
        };

        let result = optimizer.optimize_expr(expr).unwrap();
        assert_eq!(result, Expr::Ident("x".to_string()));
        assert!(optimizer.modified);
    }

    #[test]
    fn test_algebraic_simplification_add_zero() {
        let mut optimizer = ASTOptimizer::new();

        // x + 0 should become x
        let expr = Expr::Binary {
            left: Box::new(Expr::Ident("x".to_string())),
            op: BinOp::Add,
            right: Box::new(Expr::Int(0)),
        };

        let result = optimizer.optimize_expr(expr).unwrap();
        assert_eq!(result, Expr::Ident("x".to_string()));
        assert!(optimizer.modified);
    }

    #[test]
    fn test_constant_folding_boolean() {
        let mut optimizer = ASTOptimizer::new();

        // true && false should become false
        let expr = Expr::Binary {
            left: Box::new(Expr::Bool(true)),
            op: BinOp::And,
            right: Box::new(Expr::Bool(false)),
        };

        let result = optimizer.optimize_expr(expr).unwrap();
        assert_eq!(result, Expr::Bool(false));
    }

    #[test]
    fn test_constant_folding_comparison() {
        let mut optimizer = ASTOptimizer::new();

        // 10 > 5 should become true
        let expr = Expr::Binary {
            left: Box::new(Expr::Int(10)),
            op: BinOp::Greater,
            right: Box::new(Expr::Int(5)),
        };

        let result = optimizer.optimize_expr(expr).unwrap();
        assert_eq!(result, Expr::Bool(true));
    }

    #[test]
    fn test_dead_code_elimination_if_true() {
        let mut optimizer = ASTOptimizer::new();

        // if (true) { x; } else { y; } should become just x;
        let stmt = Stmt::If {
            condition: Expr::Bool(true),
            then_branch: vec![Stmt::Expr(Expr::Ident("x".to_string()))],
            else_branch: Some(vec![Stmt::Expr(Expr::Ident("y".to_string()))]),
        };

        let result = optimizer.optimize_stmt(stmt).unwrap();
        assert!(optimizer.modified);
        
        match result {
            Some(Stmt::Block(stmts)) => {
                assert_eq!(stmts.len(), 1);
            }
            _ => panic!("Expected block statement"),
        }
    }

    #[test]
    fn test_dead_code_elimination_if_false() {
        let mut optimizer = ASTOptimizer::new();

        // if (false) { x; } else { y; } should become just y;
        let stmt = Stmt::If {
            condition: Expr::Bool(false),
            then_branch: vec![Stmt::Expr(Expr::Ident("x".to_string()))],
            else_branch: Some(vec![Stmt::Expr(Expr::Ident("y".to_string()))]),
        };

        let result = optimizer.optimize_stmt(stmt).unwrap();
        assert!(optimizer.modified);
        
        match result {
            Some(Stmt::Block(stmts)) => {
                assert_eq!(stmts.len(), 1);
            }
            _ => panic!("Expected block statement"),
        }
    }

    #[test]
    fn test_dead_code_elimination_while_false() {
        let mut optimizer = ASTOptimizer::new();

        // while (false) { x; } should be eliminated
        let stmt = Stmt::While {
            condition: Expr::Bool(false),
            body: vec![Stmt::Expr(Expr::Ident("x".to_string()))],
        };

        let result = optimizer.optimize_stmt(stmt).unwrap();
        assert!(optimizer.modified);
        assert!(result.is_none());
    }

    #[test]
    fn test_dead_code_after_return() {
        let mut optimizer = ASTOptimizer::new();

        // { return x; y; } should become just { return x; }
        let stmt = Stmt::Block(vec![
            Stmt::Return(Some(Expr::Ident("x".to_string()))),
            Stmt::Expr(Expr::Ident("y".to_string())),
        ]);

        let result = optimizer.optimize_stmt(stmt).unwrap();
        assert!(optimizer.modified);
        
        match result {
            Some(Stmt::Block(stmts)) => {
                assert_eq!(stmts.len(), 1);
                assert!(matches!(stmts[0], Stmt::Return(_)));
            }
            _ => panic!("Expected block statement"),
        }
    }

    #[test]
    fn test_nested_constant_folding() {
        let mut optimizer = ASTOptimizer::new();

        // (2 + 3) * 4 should become 20
        let expr = Expr::Binary {
            left: Box::new(Expr::Binary {
                left: Box::new(Expr::Int(2)),
                op: BinOp::Add,
                right: Box::new(Expr::Int(3)),
            }),
            op: BinOp::Mul,
            right: Box::new(Expr::Int(4)),
        };

        let result = optimizer.optimize_expr(expr).unwrap();
        assert_eq!(result, Expr::Int(20));
    }
}
