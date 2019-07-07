/// Data held during evaluation

use crate::Result;

use std::rc::Rc;
use std::fmt;
use std::convert::TryInto;

use failure::{bail, Error};

#[derive(Clone)]
pub enum Value {
    Unit,
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    Pair(Rc<Value>, Rc<Value>),
    Left(Rc<Value>),
    Right(Rc<Value>),
    Function(Rc<dyn Fn(Value) -> Result<Value>>),
    Except(Rc<dyn Fn(Result<Value>) -> Result<Value>>),
    Thunk(Rc<dyn Fn(Value) -> Result<Value>>),
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::Unit
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::Int(i)
    }
}

impl From<u64> for Value {
    fn from(u: u64) -> Self {
        Value::UInt(u)
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Value::Float(f)
    }
}

impl TryInto<bool> for Value {
    type Error = Error;

    fn try_into(self) -> Result<bool> {
        match self.finalise()? {
            Value::Bool(b) => Ok(b),
            other => bail!("{} is not a bool", other),
        }
    }
}

impl TryInto<i64> for Value {
    type Error = Error;

    fn try_into(self) -> Result<i64> {
        match self.finalise()? {
            Value::Int(i) => Ok(i),
            other => bail!("{} is not an integer", other),
        }
    }
}

impl TryInto<u64> for Value {
    type Error = Error;

    fn try_into(self) -> Result<u64> {
        match self.finalise()? {
            Value::UInt(u) => Ok(u),
            other => bail!("{} is not an unsigned integer", other),
        }
    }
}

impl TryInto<f64> for Value {
    type Error = Error;

    fn try_into(self) -> Result<f64> {
        match self.finalise()? {
            Value::Float(f) => Ok(f),
            other => bail!("{} is not an float", other),
        }
    }
}

impl Value {
    /// Reduce a lazy thunk down to a final value.
    pub fn finalise(mut self) -> Result<Value> {
        while let Value::Thunk(thunk) = self {
            let value = Value::Thunk(thunk.clone());
            self = thunk(value)?;
        }

        Ok(self)
    }

    /// Recursively reducy a data structure with lazy thunks to a final value.
    pub fn finalise_recursive(self) -> Result<Value> {
        use Value::*;

        let value = match self {
            Pair(first, second) => {
                Pair(
                    Rc::new(first.as_ref().clone().finalise_recursive()?),
                    Rc::new(second.as_ref().clone().finalise_recursive()?),
                )
            }
            Left(left) => Left(Rc::new(left.as_ref().clone().finalise_recursive()?)),
            Right(right) => Right(Rc::new(right.as_ref().clone().finalise_recursive()?)),
            thunk @ Thunk(_) => thunk.finalise()?.finalise_recursive()?,
            value => value,
        };

        Ok(value)
    }

    /// Take the value as a function.
    pub fn as_function(mut self) -> Result<Rc<dyn Fn(Result<Value>) -> Result<Value>>> {
        self = self.finalise()?;

        if !self.can_call() {
            bail!("'{}' is not a function", self)
        }

        let call = move |value: Result<Value>| {
            let callable = self.clone();
            match (callable, value) {
                (Value::Function(lambda), Ok(value)) => lambda(value),
                (Value::Except(except), error @ Err(_)) => except(error),
                (Value::Except(except), Ok(value)) => except(value.finalise_recursive()),
                (_, error @ Err(_)) => error,
                _ => unreachable!(),
            }
        };

        Ok(Rc::new(call))
    }

    /// Call a function with a set of arguments to finalise it.
    pub fn call(mut self, args: impl Into<Vec<Value>>) -> Result<Value> {
        for arg in args.into() {
            self = (self.as_function()?)(Ok(arg))?;
        }
        self = self.finalise()?;

        Ok(self)
    }

    pub fn can_render(&self) -> bool {
        use Value::*;
        match self {
            Unit => true,
            Bool(_) => true,
            Int(_) => true,
            UInt(_) => true,
            Float(_) => true,
            Pair(l, r) => l.can_render() && r.can_render(),
            Left(l) => l.can_render(),
            Right(r) => r.can_render(),
            Function(_) => false,
            Except(_) => false,
            Thunk(_) => false,
        }
    }

    pub fn can_call(&self) -> bool {
        use Value::*;
        match self {
            Function(_) | Except(_) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;
        match self {
            Unit => write!(f, "()"),
            Bool(true) => write!(f, "true"),
            Bool(false) => write!(f, "false"),
            Int(i) => write!(f, "{}", i),
            UInt(u) => write!(f, "{}u", u),
            Float(v) => write!(f, "{}f", v),
            Pair(l, r) => write!(f, "({} * {})", l, r),
            Left(l) => write!(f, "({} + V)", l),
            Right(r) => write!(f, "(V + {})", r),
            Function(_) => write!(f, "V -> V"),
            Except(_) => write!(f, "E -> V"),
            thunk @ Thunk(_) => {
                if let Ok(thunk) = thunk.clone().finalise() {
                    fmt::Display::fmt(&thunk, f)
                } else {
                    write!(f, "E")
                }
            }
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
