//! Lamba expressions and evaluation
use crate::data_new::{Cell, Value};
use crate::typing::{Integer, Numeric};

use std::rc::Rc;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Operator {
    /// `Roll : T (rec t. T t) -> rec t. T t`
    Roll,
    /// `Unroll : rec t. T t -> T (rec t. T t)`
    Unroll,
    /// `Fold : forall f, a. (forall a, b. (a -> b) -> f a -> f b) -> (f a -> a) -> rec t. f t -> a`
    Fold,
    /// `Recurse : forall a. (a -> a) -> a`
    ///
    /// Behold; the Y-combinator, in all it's glory.
    Recurse,
    /// `Raise : forall a, b. a -> b`
    Raise,
    /// `Pair : forall a, b. a -> b -> (a, b)`
    Pair,
    /// `First : forall a, b. (a, b) -> a`
    First,
    /// `Second : forall a, b. (a, b) -> b`
    Second,
    /// `Empty : forall a. rec t. () + (t, a)`
    Empty,
    /// `Element n : forall a[0..n], b. (a[0], (..., (a[n - 1], b))) -> b`
    ///
    /// Allows for quickly selecting an element of a deeply nested pair by effectively performing n
    /// stacked selects on the right.
    Element(u64),
    /// `Index : UInt -> forall a. rec t. () + (a, t) -> () + a`
    Index,
    /// `Left : forall a, b. a -> a + b`
    Left,
    /// `Right : forall a, b. b -> a + b`
    Right,
    /// `Case : forall a, b, c. (a -> c) -> (b -> c) -> (a + b) -> c`
    Case,
    /// `SkipLeft n : forall a[0..n], b, c. (forall a. a -> c) -> (b -> c) -> (a[0] + (... + (a[n - 1] + b))) -> c`
    ///
    /// Allows for quickly matching deeply nested sum values to match a single specific case by
    /// discarding effectively performing n stacked matches on the right.
    SkipLeft(u64),
    /// `Not : Bool -> Bool`
    Not,
    /// `And : Bool -> Bool -> Bool`
    And,
    /// `Or : Bool -> Bool -> Bool`
    Or,
    /// `Greater : Numeric n => n -> n -> n`
    Greater(Numeric),
    /// `GreaterEqual : Numeric n => n -> n -> Bool`
    GreaterOrEqual(Numeric),
    /// `Less : Numeric n => n -> n -> Bool`
    Less(Numeric),
    /// `LessEqual : Numeric n => n -> n -> Bool`
    LessOrEqual(Numeric),
    /// `Equal : Numeric n => n -> n -> n`
    Equal(Numeric),
    /// `NotEqual : Numeric n => n -> n -> n`
    NotEqual(Numeric),
    /// `Add : Numeric n => n -> n -> n`
    Add(Numeric),
    /// `Subtract : Numeric n => n -> n -> n`
    Subtract(Numeric),
    /// `Multiply : Numeric n => n -> n -> n`
    Multiply(Numeric),
    /// `Divide : Numeric n => n -> n -> n`
    Divide(Numeric),
    /// `Remainder : Numeric n => n -> n -> n`
    Remainder(Numeric),
    /// `ShiftLeft : Integer i => i -> i`
    ShiftLeft(Integer),
    /// `ShiftRight : Integer i => i -> i`
    ShiftRight(Integer),
    /// `BitwiseNot : Integer i => i -> i`
    BitwiseNot(Integer),
    /// `BitwiseAnd : Integer i => i -> i`
    BitwiseAnd(Integer),
    /// `BitwiseOr : Integer i => i -> i`
    BitwiseOr(Integer),
    /// `BitwiseExclusiveOr : Integer i => i -> i`
    BitwiseExclusiveOr(Integer),
    /// `Signed : Integer i => i -> Int`
    Signed(Integer),
    /// `Unsigned : Integer i => i -> UInt`
    Unsigned(Integer),
    /// `Float : Integer i => i -> Float`
    Float(Integer),
    /// `Ceiling : Float -> Int`
    Ceiling,
    /// `Floor : Float -> Int`
    Floor,
    /// `RoundUp : Float -> Int`
    ///
    /// Rounds so that exact halves increase in magnitude (0.5 -> 1 and -0.5 -> -1).
    RoundUp,
    /// `RoundDown : Float -> Int`
    ///
    /// Rounds so that exact halves decrease in magnitude (0.5 -> 0 and -0.5 -> 0).
    RoundDown,
}

impl Operator {
    fn value(&self) -> Value {
        match self {
            _ => unimplemented!()
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
    Variable(usize),
    /// Builtin operator
    Operator(Operator),
    /// Builtin value
    Value(Value),
    /// Exception handling
    Except(Cell<Expr>, Cell<Expr>),
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
        use crate::data_new::Value::*;
        use Expr::*;

        match self {
            Lambda(expr) => {
                let expr = expr.clone();
                let context = context.clone();
                let function = move |arg| {
                    let context = context.push(arg);
                    expr.borrow_mut().eval_with_context(context)
                };
                *self = Value(Function(Rc::new(function)));
                self.eval()
            }
            Apply(lambda, argument) => {
                let lambda = lambda.borrow_mut().reduce(context)?;
                *self = Value(lambda.apply(Self::thunk(argument))?);
                self.eval()
            }
            Variable(variable) => {
                let value = context.get(*variable).ok_or(NotBound(*variable))?;
                *self = Value(value);
                self.eval()
            }
            Operator(operator) => {
                *self = Value(operator.value());
                self.eval()
            }
            Value(value) => Ok(value.clone()),
            Except(try_expr, except) => {
                let result = try_expr.borrow_mut().reduce(context.clone());
                match result {
                    Ok(value) => {
                        *self = Value(value);
                    }
                    _error => {
                        // TODO: Lift the error to become the argument
                        let exception = crate::data_new::Value::unit();
                        let except = except.borrow_mut().reduce(context)?;
                        *self = Value(except.apply(exception)?);
                    }
                }
                self.eval()
            }
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
            Except(try_expr, except) => {
                write!(f, "(try {} except {})", try_expr.borrow(), except.borrow())
            }
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
    NotBool(Value),
    NotInt(Value),
    NotUInt(Value),
    NotFloat(Value),
    NotNumeric(Value),
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
            NotBool(value) => write!(f, "{} is not a boolean", value),
            NotInt(value) => write!(f, "{} is not a integer", value),
            NotUInt(value) => write!(f, "{} is not an unsigned integer", value),
            NotFloat(value) => write!(f, "{} is not a float", value),
            NotNumeric(value) => write!(f, "{} is not numeric", value),
            NotPair(value) => write!(f, "{} is not a pair", value),
            NotSum(value) => write!(f, "{} is not a sum", value),
            NotFunction(value) => write!(f, "{} is not a function", value),
        }
    }
}

impl std::error::Error for Error {}

/// Result of partial or full evaluation
type Result<T> = ::std::result::Result<T, Error>;
