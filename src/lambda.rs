//! Lamba expressions and evaluation
use crate::data_new::{Cell, Value};

use std::rc::Rc;
use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::fmt;

use failure::{Error, format_err};

/// Errors triggered by invalid operations
#[derive(Debug, Clone)]
enum Fault {
    Raise(Value),
}

impl fmt::Display for Fault {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}

impl std::error::Error for Fault {}

/// Result of partial or full evaluation
type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Operator {
    Roll,
    Unroll,
    Raise,
    Except,
    Catch,
    Pair,
    First,
    Second,
    Left,
    Right,
    Case,
    Not,
    And,
    Or,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Equal,
    NotEqual,
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    ShiftLeft,
    ShiftRight,
    BitwiseNot,
    BitwiseAnd,
    BitwiseOr,
    BitwiseExclusiveOr,
    Signed,
    Unsigned,
    Ceiling,
    Floor,
    RoundUp,
    RoundDown,
}

impl Operator {
    fn value(&self) -> Value {
        unimplemented!()
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    /// Lambda variable binding
    Lambda(Cell<Expr>),
    /// Application of a lambda
    Apply(Cell<Expr>, Cell<Expr>),
    /// De Bruijin indexed variable reference
    Variable(u64),
    /// Builtin operator
    Operator(Operator),
    /// Builtin value
    Value(Value),
}

impl Expr {
    /// Strict evaluation of the value
    pub fn eval(self) -> Result<Value> {
        unimplemented!()
    }

    pub fn value(&self) -> Option<&Value> {
        match self {
            Expr::Value(value) => Some(value),
            _ => None,
        }
    }

    pub fn reduce(&mut self) -> Result<Value> {
        unimplemented!()
    }

    fn substitute(&mut self, expr: Cell<Expr>, index: i64) {
        unimplemented!()
    }

    fn remove_dead_code(&mut self) {
        unimplemented!()
    }
}
