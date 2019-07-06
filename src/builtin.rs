//! Builtin high-level expressions

use std::fmt;
use std::rc::Rc;
use std::convert::{TryInto, TryFrom};

use failure::{Error, bail};

use crate::data::Value;
use crate::Result;

#[derive(Debug, Clone)]
pub enum Builtin {
    /// Produce an error when evaluated
    Error(String),

    /* Value builtins */
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),

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

impl Builtin {
    pub(crate) fn eval(&self) -> Result<Value> {
        use Builtin::*;
        match self {
            Error(msg) => Ok(error(msg.clone())),
            Bool(v) => Ok(Value::Bool(*v)),
            Int(v) => Ok(Value::Int(*v)),
            UInt(v) => Ok(Value::UInt(*v)),
            Float(v) => Ok(Value::Float(*v)),
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

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Builtin::*;
        match self {
            Error(e) => write!(f, "(error {:?})", e),
            Bool(true) => write!(f, "true"),
            Bool(false) => write!(f, "false"),
            Int(v) => write!(f, "{}", v),
            UInt(v) => write!(f, "{}u", v),
            Float(v) => write!(f, "{}f", v),
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
        impl From<$from> for Builtin {
            fn from(value: $from) -> Self {
                Builtin::$var(value)
            }
        }
    };
    ($from:ty => $var:ident($to:ty)) => {
        impl From<$from> for Builtin {
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

pub fn val(value: impl Into<Builtin>) -> Builtin {
    value.into()
}

pub fn error(e: String) -> Value {
    Value::Thunk(Rc::new(move || bail!("{}", e)))
}

#[derive(Debug, Clone, Copy)]
enum Number {
    Int(i64),
    UInt(u64),
    Float(f64),
}

impl TryFrom<Value> for Number {
    type Error = Error;

    fn try_from(value: Value) -> Result<Number> {
        match value.finalise()? {
            Value::Int(i) => Ok(Number::Int(i)),
            Value::UInt(u) => Ok(Number::UInt(u)),
            Value::Float(u) => Ok(Number::Float(u)),
            other => bail!("{} is not numeric", other),
        }
    }
}

impl Into<Value> for Number {
    fn into(self) -> Value {
        use Number::*;
        match self {
            Int(i) => Value::Int(i),
            UInt(u) => Value::UInt(u),
            Float(f) => Value::Float(f),
        }
    }
}

fn unary_op<A>(f: impl Sized + 'static + Fn(A) -> Result<Value>) -> Value
where
    Value: TryInto<A, Error = Error>,
    <Value as TryInto<A>>::Error: Send + Sync + 'static,
{
    let cast = move |a: Value| f(a.try_into()?);
    Value::Function(Rc::new(cast))
}

fn binary_op<A, B>(f: impl Sized + 'static + Fn(A, B) -> Result<Value>) -> Value
where
    A: Clone + 'static,
    Value: TryInto<A, Error = Error>,
    <Value as TryInto<A>>::Error: Send + Sync + 'static,
    Value: TryInto<B, Error = Error>,
    <Value as TryInto<B>>::Error: Send + Sync + 'static,
{
    let f = Rc::new(f);
    unary_op(move |a: A| {
        let f = f.clone();
        Ok(unary_op(move |b: B| {
            f(a.clone(), b)
        }))
    })
}

fn binary_numeric_op<I: Into<Value>, U: Into<Value>, F: Into<Value>>(
    int: impl Fn(i64, i64) -> I + 'static,
    uint: impl Fn(u64, u64) -> U + 'static,
    float: impl Fn(f64, f64) -> F + 'static,
) -> Value {
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

fn unary_unsigned_op(op: impl Fn(u64) -> u64 + 'static) -> Value {
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

fn binary_unsigned_op(op: impl Fn(u64, u64) -> u64 + 'static) -> Value {
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
