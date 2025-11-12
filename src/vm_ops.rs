/// Bridge module for VM operations
///
/// This module provides operation implementations for the VM using optimized
/// standalone operation functions that don't require creating Interpreter instances.
use crate::ast::{BinOp, UnaryOp};
use crate::value::Value;
use crate::value_ops::{value_binary_op, value_unary_op};

/// Operations trait for VM
pub trait VMOperations {
    fn vm_add(&self, other: &Value) -> Result<Value, String>;
    fn vm_sub(&self, other: &Value) -> Result<Value, String>;
    fn vm_mul(&self, other: &Value) -> Result<Value, String>;
    fn vm_div(&self, other: &Value) -> Result<Value, String>;
    fn vm_mod(&self, other: &Value) -> Result<Value, String>;
    fn vm_pow(&self, other: &Value) -> Result<Value, String>;
    fn vm_neg(&self) -> Result<Value, String>;
    fn vm_not(&self) -> Result<Value, String>;
    fn vm_factorial(&self) -> Result<Value, String>;

    // Comparison operations
    fn vm_eq(&self, other: &Value) -> Result<Value, String>;
    fn vm_neq(&self, other: &Value) -> Result<Value, String>;
    fn vm_gt(&self, other: &Value) -> Result<Value, String>;
    fn vm_gte(&self, other: &Value) -> Result<Value, String>;
    fn vm_lt(&self, other: &Value) -> Result<Value, String>;
    fn vm_lte(&self, other: &Value) -> Result<Value, String>;

    // Logical operations
    fn vm_and(&self, other: &Value) -> Result<Value, String>;
    fn vm_or(&self, other: &Value) -> Result<Value, String>;
}

impl VMOperations for Value {
    fn vm_add(&self, other: &Value) -> Result<Value, String> {
        value_binary_op(self, BinOp::Add, other)
    }

    fn vm_sub(&self, other: &Value) -> Result<Value, String> {
        value_binary_op(self, BinOp::Sub, other)
    }

    fn vm_mul(&self, other: &Value) -> Result<Value, String> {
        value_binary_op(self, BinOp::Mul, other)
    }

    fn vm_div(&self, other: &Value) -> Result<Value, String> {
        value_binary_op(self, BinOp::Div, other)
    }

    fn vm_mod(&self, other: &Value) -> Result<Value, String> {
        value_binary_op(self, BinOp::Mod, other)
    }

    fn vm_pow(&self, other: &Value) -> Result<Value, String> {
        value_binary_op(self, BinOp::Pow, other)
    }

    fn vm_neg(&self) -> Result<Value, String> {
        value_unary_op(UnaryOp::Neg, self)
    }

    fn vm_not(&self) -> Result<Value, String> {
        value_unary_op(UnaryOp::Not, self)
    }

    fn vm_factorial(&self) -> Result<Value, String> {
        value_unary_op(UnaryOp::Factorial, self)
    }

    fn vm_eq(&self, other: &Value) -> Result<Value, String> {
        value_binary_op(self, BinOp::Equal, other)
    }

    fn vm_neq(&self, other: &Value) -> Result<Value, String> {
        value_binary_op(self, BinOp::NotEqual, other)
    }

    fn vm_gt(&self, other: &Value) -> Result<Value, String> {
        value_binary_op(self, BinOp::Greater, other)
    }

    fn vm_gte(&self, other: &Value) -> Result<Value, String> {
        value_binary_op(self, BinOp::GreaterEq, other)
    }

    fn vm_lt(&self, other: &Value) -> Result<Value, String> {
        value_binary_op(self, BinOp::Less, other)
    }

    fn vm_lte(&self, other: &Value) -> Result<Value, String> {
        value_binary_op(self, BinOp::LessEq, other)
    }

    fn vm_and(&self, other: &Value) -> Result<Value, String> {
        value_binary_op(self, BinOp::And, other)
    }

    fn vm_or(&self, other: &Value) -> Result<Value, String> {
        value_binary_op(self, BinOp::Or, other)
    }
}
