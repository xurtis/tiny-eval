//! Lamba expressions and evaluation
use crate::data_new::{Cell, Value};
use crate::operator::Operator;

use std::rc::Rc;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Expr {
    /// Lambda variable binding
    Lambda(Cell<Expr>),
    /// Application of a lambda
    Apply(Cell<Expr>, Cell<Expr>),
    /// De Bruijin indexed variable reference
    Variable(usize),
    /// Builtin operator
    Operator(Operator),
    /// Builtin value
    Value(Value),
}

impl Expr {
    /// Strict evaluation of the lambda expression to a value.
    pub fn eval(&mut self) -> Result<Value> {
        if let Expr::Value(value) = self {
            Ok(value.clone())
        } else {
            self.eval_with_context(List::new())
        }
    }

    fn eval_with_context(&mut self, context: List<Value>) -> Result<Value> {
        match self {
            Expr::Lambda(expr) => {
                let expr = expr.clone();
                let context = context.clone();
                let function = move |arg| {
                    let context = context.push(arg);
                    expr.borrow_mut().eval_with_context(context)
                };
                *self = Expr::Value(Value::function_try(function));
                self.eval()
            }
            Expr::Apply(lambda, argument) => {
                let lambda = lambda.borrow_mut().reduce(context)?;
                *self = Expr::Value(lambda.apply(Self::thunk(argument))?);
                self.eval()
            }
            Expr::Variable(variable) => {
                let value = context.get(*variable).ok_or(NotBound(*variable))?;
                *self = Expr::Value(value);
                self.eval()
            }
            Expr::Operator(operator) => {
                *self = Expr::Value((*operator).into());
                self.eval()
            }
            Expr::Value(value) => Ok(value.clone()),
        }
    }

    fn reduce(&mut self, context: List<Value>) -> Result<Value> {
        self.eval_with_context(context)?;
        if let Expr::Value(value) = self {
            value.reduce()?;
        }
        self.eval()
    }

    fn thunk(expr: &Cell<Self>) -> Value {
        let expr = expr.clone();
        Value::Thunk(Rc::new(move || expr.borrow_mut().eval()))
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expr::*;
        match self {
            Lambda(expr) => write!(f, "λ.{}", expr.borrow()),
            Apply(function, argument) => {
                write!(f, "({} {})", function.borrow(), argument.borrow())
            }
            Variable(name) => write!(f, "{}", name),
            Value(value) => write!(f, "{}", value),
            Operator(operator) => write!(f, "{}", operator),
        }
    }
}

/// Persistent linked list structure
#[derive(Debug)]
enum ListNode<T> {
    Node(T, List<T>),
    Empty,
}

#[derive(Debug)]
struct List<T>(Rc<ListNode<T>>);

impl<T: Clone> List<T> {
    fn new() -> Self {
        List(Rc::new(ListNode::Empty))
    }

    fn push(&self, value: T) -> List<T> {
        List(Rc::new(ListNode::Node(value, (*self).clone())))
    }

    fn get(&self, index: usize) -> Option<T> {
        use ListNode::*;
        match (index, self.0.as_ref()) {
            (0, Node(value, _)) => Some(value.clone()),
            (n, Node(_, next)) => next.get(n - 1),
            (_, Empty) => None,
        }
    }
}

impl<T> Clone for List<T> {
    fn clone(&self) -> Self {
        List(self.0.clone())
    }
}

/// Errors triggered by invalid operations
#[derive(Debug)]
pub enum Error {
    Raise(Value),
    NotBound(usize),
    NotUnit(Value),
    NotBool(Value),
    NotInt(Value),
    NotUInt(Value),
    NotFloat(Value),
    NotNumeric(Value),
    NotInteger(Value),
    NotBinary(Value),
    NotPair(Value),
    NotSum(Value),
    NotFunction(Value),
}
use Error::*;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Raise(value) => write!(f, "{} raised as error by program", value),
            NotBound(index) => write!(f, "{} is not bound", index),
            NotUnit(value) => write!(f, "{} is not a unit", value),
            NotBool(value) => write!(f, "{} is not a boolean", value),
            NotInt(value) => write!(f, "{} is not an integer", value),
            NotUInt(value) => write!(f, "{} is not an unsigned integer", value),
            NotFloat(value) => write!(f, "{} is not a float", value),
            NotNumeric(value) => {
                write!(f, "{} is not numeric (integer, unsigned integer, or float)", value)
            }
            NotInteger(value) => {
                write!(f, "{} is not integer (signed or unsigned)", value)
            }
            NotBinary(value) => {
                write!(f, "{} is not binary (integer, unsigned integer, or boolean)", value)
            }
            NotPair(value) => write!(f, "{} is not a pair", value),
            NotSum(value) => write!(f, "{} is not a sum", value),
            NotFunction(value) => write!(f, "{} is not a function", value),
        }
    }
}

impl std::error::Error for Error {}

impl From<::std::convert::Infallible> for Error {
    fn from(_: ::std::convert::Infallible) -> Error {
        unreachable!()
    }
}

/// Result of partial or full evaluation
type Result<T> = ::std::result::Result<T, Error>;
