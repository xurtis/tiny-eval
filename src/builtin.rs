//! Builtin high-level expressions

use std::fmt;
use std::rc::Rc;
use std::convert::{TryInto, TryFrom};

use crate::data::Value;
use crate::{Identifier, Error, Result, ValueResult};

#[derive(Debug, Clone)]
pub enum Builtin<I> {
    /// Produce an error when evaluated
    Raise,
    /// Replace an value that produces an error with another value
    Except,
    /// Handle an exception raised within the program
    Catch,

    /* Value builtins */
    Unit,
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    Function(Function<I>),

    /* Control flow */
    Condition,
    Pair,
    First,
    Second,
    Left,
    Right,
    Case,

    /* Boolean Operators */
    Not,
    And,
    Or,

    /* Comparison Operators */
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Equal,
    NotEqual,

    /* Arithmetic Operators */
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,

    /* Binary Operators */
    ShiftLeft,
    ShiftRight,
    BinaryOr,
    BinaryAnd,
    BinaryNot,
    BinaryExclusiveOr,
}

#[derive(Clone)]
pub struct Function<I>(I, Rc<dyn Fn(Value<I>) -> ValueResult<I> + 'static>);

impl<I: fmt::Debug> fmt::Debug for Function<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl<I: fmt::Display> fmt::Display for Function<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub mod construct {
    use super::Builtin::*;
    use crate::expr::{Expr, Identifier, construct::apply};

    macro_rules! auto_apply {
        ($(#[$doc:meta])* $builtin:ident => $name:ident($a:ident)) => {
            $(#[$doc])*
            pub fn $name<I: Identifier>($a: impl Into<Expr<I>>) -> Expr<I> {
                apply($builtin, $a)
            }
        };
        ($(#[$doc:meta])* $builtin:ident => $name:ident($a:ident, $b:ident)) => {
            $(#[$doc])*
            pub fn $name<I: Identifier>(
                $a: impl Into<Expr<I>>,
                $b: impl Into<Expr<I>>,
            ) -> Expr<I> {
                apply(apply($builtin, $a), $b)
            }
        };
        ($(#[$doc:meta])* $builtin:ident => $name:ident($a:ident, $b:ident, $c:ident)) => {
            $(#[$doc])*
            pub fn $name<I: Identifier>(
                $a: impl Into<Expr<I>>,
                $b: impl Into<Expr<I>>,
                $c: impl Into<Expr<I>>,
            ) -> Expr<I> {
                apply(apply(apply($builtin, $a), $b), $c)
            }
        };
    }

    auto_apply!(Raise => raise(v));
    auto_apply!(Except => except(v, e));
    auto_apply!(Catch => catch(v, e));
    auto_apply!(Condition => condition(cond, t, f));
    auto_apply!(Pair => pair(first, second));
    auto_apply!(First => first(pair));
    auto_apply!(Second => second(pair));
    auto_apply!(Left => left(value));
    auto_apply!(Right => right(value));
    auto_apply!(Case => case(sum, left, right));

    auto_apply!(Not => not(a));
    auto_apply!(And => and(a, b));
    auto_apply!(Or => or(a, b));

    auto_apply!(Greater => greater(a, b));
    auto_apply!(GreaterEqual => greater_equal(a, b));
    auto_apply!(Less => less(a, b));
    auto_apply!(LessEqual => less_equal(a, b));
    auto_apply!(Equal => equal(a, b));
    auto_apply!(NotEqual => not_equal(a, b));

    auto_apply!(Add => add(a, b));
    auto_apply!(Subtract => subtract(a, b));
    auto_apply!(Multiply => multiply(a, b));
    auto_apply!(Divide => divide(a, b));
    auto_apply!(Remainder => remainder(a, b));

    auto_apply!(ShiftLeft => shift_left(a, b));
    auto_apply!(ShiftRight => shift_right(a, b));
    auto_apply!(BinaryOr => binary_or(a, b));
    auto_apply!(BinaryAnd => binary_and(a, b));
    auto_apply!(BinaryNot => binary_not(a));
    auto_apply!(BinaryExclusiveOr => binary_exclusive_or(a, b));
}

macro_rules! binary_numeric {
    ($impl:expr) => {
        Ok(binary_numeric_op($impl, $impl, $impl))
    }
}

macro_rules! binary_unsigned {
    ($impl:expr) => {
        Ok(binary_unsigned_op($impl))
    }
}

impl<I: Identifier> Builtin<I> {
    pub(crate) fn eval(&self) -> ValueResult<I> {
        use Builtin::*;
        match self {
            Raise => Ok(raise()),
            Except => Ok(except()),
            Catch => Ok(catch()),
            Unit => Ok(Value::Unit),
            Bool(v) => Ok(Value::Bool(*v)),
            Int(v) => Ok(Value::Int(*v)),
            UInt(v) => Ok(Value::UInt(*v)),
            Float(v) => Ok(Value::Float(*v)),
            Function(self::Function(_, lambda)) => Ok(Value::Function(lambda.clone())),
            Condition => Ok(condition()),
            Pair => Ok(pair()),
            First => Ok(first()),
            Second => Ok(second()),
            Left => Ok(left()),
            Right => Ok(right()),
            Case => Ok(case()),
            Not => Ok(unary_op(|a: bool| Ok(Value::Bool(!a)))),
            And => Ok(binary_op(|a: bool, b: bool| Ok(Value::Bool(a && b)))),
            Or => Ok(binary_op(|a: bool, b: bool| Ok(Value::Bool(a || b)))),
            Greater => binary_numeric!(|a, b| a > b),
            GreaterEqual => binary_numeric!(|a, b| a >= b),
            Less => binary_numeric!(|a, b| a < b),
            LessEqual => binary_numeric!(|a, b| a <= b),
            Equal => binary_numeric!(|a, b| a == b),
            NotEqual => binary_numeric!(|a, b| a != b),
            Add => binary_numeric!(|a, b| a + b),
            Subtract => binary_numeric!(|a, b| a - b),
            Multiply => binary_numeric!(|a, b| a * b),
            Divide => binary_numeric!(|a, b| a / b),
            Remainder => binary_numeric!(|a, b| a % b),
            ShiftLeft => binary_unsigned!(|a, b| b << a),
            ShiftRight => binary_unsigned!(|a, b| b >> a),
            BinaryOr => binary_unsigned!(|a, b| b | a),
            BinaryAnd => binary_unsigned!(|a, b| b & a),
            BinaryExclusiveOr => binary_unsigned!(|a, b| b ^ a),
            BinaryNot => Ok(unary_unsigned_op(|a| !a)),
        }
    }
}

impl<I: fmt::Display> fmt::Display for Builtin<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Builtin::*;
        match self {
            Raise => write!(f, "raise"),
            Except => write!(f, "except"),
            Catch => write!(f, "catch"),
            Unit => write!(f, "()"),
            Bool(true) => write!(f, "true"),
            Bool(false) => write!(f, "false"),
            Int(v) => write!(f, "{}", v),
            UInt(v) => write!(f, "{}u", v),
            Float(v) => write!(f, "{}f", v),
            Function(function) => write!(f, "{}", function),
            Condition => write!(f, "if"),
            Pair => write!(f, "pair"),
            First => write!(f, "first"),
            Second => write!(f, "second"),
            Left => write!(f, "left"),
            Right => write!(f, "right"),
            Case => write!(f, "case"),
            Not => write!(f, "not"),
            And => write!(f, "and"),
            Or => write!(f, "or"),
            Greater => write!(f, ">"),
            GreaterEqual => write!(f, ">="),
            Less => write!(f, "<"),
            LessEqual => write!(f, "<="),
            Equal => write!(f, "="),
            NotEqual => write!(f, "!="),
            Add => write!(f, "+"),
            Subtract => write!(f, "-"),
            Multiply => write!(f, "*"),
            Divide => write!(f, "/"),
            Remainder => write!(f, "%"),
            ShiftLeft => write!(f, "<<"),
            ShiftRight => write!(f, ">>"),
            BinaryOr => write!(f, "|"),
            BinaryAnd => write!(f, "&"),
            BinaryNot => write!(f, "~"),
            BinaryExclusiveOr => write!(f, "^"),
        }
    }
}

macro_rules! builtin_value {
    ($from:ty => $var:ident) => {
        impl<I> From<$from> for Builtin<I> {
            fn from(value: $from) -> Self {
                Builtin::$var(value)
            }
        }
    };
    ($from:ty => $var:ident($to:ty)) => {
        impl<I> From<$from> for Builtin<I> {
            fn from(value: $from) -> Self {
                Builtin::$var(value as $to)
            }
        }
    };
}

builtin_value!(i8 => Int(i64));
builtin_value!(i16 => Int(i64));
builtin_value!(i32 => Int(i64));
builtin_value!(i64 => Int(i64));
builtin_value!(u8 => UInt(u64));
builtin_value!(u16 => UInt(u64));
builtin_value!(u32 => UInt(u64));
builtin_value!(u64 => UInt(u64));
builtin_value!(f32 => Float(f64));
builtin_value!(f64 => Float(f64));
builtin_value!(bool => Bool);

impl<I> From<()> for Builtin<I> {
    fn from(_: ()) -> Self {
        Builtin::Unit
    }
}

pub fn value<I: Identifier>(value: impl Into<Builtin<I>>) -> Builtin<I> {
    value.into()
}

pub fn function<I: Identifier>(
    name: I,
    lambda: impl Fn(Value<I>) -> ValueResult<I> + 'static
) -> Builtin<I> {
    Builtin::Function(Function(name, Rc::new(lambda)))
}

#[derive(Debug, Clone, Copy)]
enum Number {
    Int(i64),
    UInt(u64),
    Float(f64),
}

impl<I: Identifier> TryFrom<Value<I>> for Number {
    type Error = Error<I>;

    fn try_from(value: Value<I>) -> Result<Number, I> {
        match value.finalise()? {
            Value::Int(i) => Ok(Number::Int(i)),
            Value::UInt(u) => Ok(Number::UInt(u)),
            Value::Float(u) => Ok(Number::Float(u)),
            other => Err(Error::NotNumeric(other)),
        }
    }
}

impl<I> Into<Value<I>> for Number {
    fn into(self) -> Value<I> {
        use Number::*;
        match self {
            Int(i) => Value::Int(i),
            UInt(u) => Value::UInt(u),
            Float(f) => Value::Float(f),
        }
    }
}

fn simple_op<I: Identifier>(f: impl Sized + 'static + Fn(Value<I>) -> ValueResult<I>) -> Value<I> {
    Value::Function(Rc::new(move |a| f(a)))
}

fn infallable_op<I: Identifier>(f: impl Sized + 'static + Fn(Value<I>) -> Value<I>) -> Value<I> {
    Value::Function(Rc::new(move |a| Ok(f(a))))
}

fn unary_op<I, A>(f: impl Sized + 'static + Fn(A) -> ValueResult<I>) -> Value<I>
where
    I: Identifier,
    Value<I>: TryInto<A, Error = Error<I>>,
{
    let cast = move |a: Value<I>| f(a.try_into()?);
    Value::Function(Rc::new(cast))
}

fn binary_op<I, A, B>(f: impl Sized + 'static + Fn(A, B) -> ValueResult<I>) -> Value<I>
where
    I: Identifier,
    A: Clone + 'static,
    Value<I>: TryInto<A, Error = Error<I>>,
    Value<I>: TryInto<B, Error = Error<I>>,
{
    let f = Rc::new(f);
    unary_op(move |a: A| {
        let f = f.clone();
        Ok(unary_op(move |b: B| {
            f(a.clone(), b)
        }))
    })
}

fn binary_numeric_op<I: Identifier, N: Into<Value<I>>, U: Into<Value<I>>, F: Into<Value<I>>>(
    int: impl Fn(i64, i64) -> N + 'static,
    uint: impl Fn(u64, u64) -> U + 'static,
    float: impl Fn(f64, f64) -> F + 'static,
) -> Value<I> {
    use Number::*;

    let cast = move |a: Number, b: Number| {
        let result = match (a, b) {
            (UInt(a), UInt(b)) => uint(a, b).into(),
            (Int(a), Int(b)) => int(a, b).into(),
            (UInt(a), Int(b)) => int(a as i64, b).into(),
            (Int(a), UInt(b)) => int(a, b as i64).into(),
            (Float(a), Int(b)) => float(a, b as f64).into(),
            (Int(a), Float(b)) => float(a as f64, b).into(),
            (Float(a), UInt(b)) => float(a, b as f64).into(),
            (UInt(a), Float(b)) => float(a as f64, b).into(),
            (Float(a), Float(b)) => float(a, b).into(),
        };
        Ok(result)
    };

    binary_op(cast)
}

fn unary_unsigned_op<I: Identifier>(op: impl Fn(u64) -> u64 + 'static) -> Value<I> {
    use Number::*;

    let cast = move |a: Number| {
        let result = match a {
            UInt(a) => op(a as u64).into(),
            Int(a) => op(a as u64).into(),
            Float(a) => op(a as u64).into(),
        };
        Ok(result)
    };

    unary_op(cast)
}

fn binary_unsigned_op<I: Identifier>(op: impl Fn(u64, u64) -> u64 + 'static) -> Value<I> {
    use Number::*;

    let cast = move |a: Number, b: Number| {
        let result = match (a, b) {
            (UInt(a), UInt(b)) => op(a as u64, b as u64).into(),
            (Int(a), Int(b)) => op(a as u64, b as u64).into(),
            (UInt(a), Int(b)) => op(a as u64, b as u64).into(),
            (Int(a), UInt(b)) => op(a as u64, b as u64).into(),
            (Float(a), Int(b)) => op(a as u64, b as u64).into(),
            (Int(a), Float(b)) => op(a as u64, b as u64).into(),
            (Float(a), UInt(b)) => op(a as u64, b as u64).into(),
            (UInt(a), Float(b)) => op(a as u64, b as u64).into(),
            (Float(a), Float(b)) => op(a as u64, b as u64).into(),
        };
        Ok(result)
    };

    binary_op(cast)
}

/// Simple exception handling
fn except<I: Identifier>() -> Value<I> {
    infallable_op(|default| {
        let default = default.clone();
        Value::Except(Rc::new(move |v| {
            match v {
                Err(_) => Ok(default.clone()),
                v => v
            }
        }))
    })
}

/// Catch errors raised within the program
fn catch<I: Identifier>() -> Value<I> {
    infallable_op(|handler| {
        let handler = handler.clone();
        Value::Except(Rc::new(move |v| {
            match v {
                Err(Error::Raise(error)) => {
                    (handler.clone().as_function()?)(Ok(error))
                }
                v => v
            }
        }))
    })
}

fn raise<I: Identifier>() -> Value<I> {
    simple_op(|value| {
        Err(Error::Raise(value))
    })
}

/// Branching conditional (if)
fn condition<I: Identifier>() -> Value<I> {
    unary_op(|cond: bool| {
        if cond {
            Ok(infallable_op(|t: Value<I>| {
                let t = t.clone();
                infallable_op(move |_: Value<I>| {
                    t.clone()
                })
            }))
        } else {
            Ok(infallable_op(|_: Value<I>| {
                infallable_op(|f: Value<I>| {
                    f
                })
            }))
        }
    })
}

/// Construct a pair
fn pair<I: Identifier>() -> Value<I> {
    infallable_op(|first: Value<I>| {
        let first = first.clone();
        infallable_op(move |second: Value<I>| {
            Value::Pair(Rc::new(first.clone()), Rc::new(second))
        })
    })
}

/// Taking the first value of a pair
fn first<I: Identifier>() -> Value<I> {
    simple_op(|value: Value<I>| {
        match value.finalise()? {
            Value::Pair(first, _) => Ok(first.as_ref().clone()),
            value => Err(Error::NotPair(value)),
        }
    })
}

/// Taking the second value of a pair
fn second<I: Identifier>() -> Value<I> {
    simple_op(|value: Value<I>| {
        match value.finalise()? {
            Value::Pair(_, second) => Ok(second.as_ref().clone()),
            value => Err(Error::NotPair(value)),
        }
    })
}

/// Construct the left part of a sum
fn left<I: Identifier>() -> Value<I> {
    infallable_op(|left: Value<I>| {
        Value::Left(Rc::new(left))
    })
}

/// Construct the right part of a sum
fn right<I: Identifier>() -> Value<I> {
    infallable_op(|right: Value<I>| {
        Value::Right(Rc::new(right))
    })
}

/// Handling cases of a sum
fn case<I: Identifier>() -> Value<I> {
    simple_op(|value: Value<I>| {
        match value.finalise()? {
            Value::Left(value) => {
                Ok(infallable_op(move |left: Value<I>| {
                    let left = left.clone();
                    let value = value.as_ref().clone();
                    simple_op(move |_: Value<I>| {
                        let left = left.clone();
                        let value = value.clone();
                        (left.as_function()?)(Ok(value))
                    })
                }))
            }
            Value::Right(value) => {
                Ok(infallable_op(move |_: Value<I>| {
                    let value = value.as_ref().clone();
                    simple_op(move |right: Value<I>| {
                        let value = value.clone();
                        (right.as_function()?)(Ok(value))
                    })
                }))
            }
            value => Err(Error::NotSum(value)),
        }
    })
}
