/// Data held during evaluation

use std::rc::Rc;
use std::fmt;
use std::convert::TryInto;

use crate::{Identifier, Result, ValueResult, Error};

#[derive(Clone)]
pub enum Value<I> {
    Unit,
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    Pair(Rc<Value<I>>, Rc<Value<I>>),
    Left(Rc<Value<I>>),
    Right(Rc<Value<I>>),
    Function(Rc<dyn Fn(Value<I>) -> ValueResult<I>>),
    Except(Rc<dyn Fn(ValueResult<I>) -> ValueResult<I>>),
    Thunk(Rc<dyn Fn(Value<I>) -> ValueResult<I>>),
}

impl<I> From<()> for Value<I> {
    fn from(_: ()) -> Self {
        Value::Unit
    }
}

impl<I> From<bool> for Value<I> {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

impl<I> From<i64> for Value<I> {
    fn from(i: i64) -> Self {
        Value::Int(i)
    }
}

impl<I> From<u64> for Value<I> {
    fn from(u: u64) -> Self {
        Value::UInt(u)
    }
}

impl<I> From<f64> for Value<I> {
    fn from(f: f64) -> Self {
        Value::Float(f)
    }
}

impl<I: Identifier> TryInto<bool> for Value<I> {
    type Error = Error<I>;

    fn try_into(self) -> Result<bool, I> {
        match self.finalise()? {
            Value::Bool(b) => Ok(b),
            other => Err(Error::NotBool(other)),
        }
    }
}

impl<I: Identifier> TryInto<i64> for Value<I> {
    type Error = Error<I>;

    fn try_into(self) -> Result<i64, I> {
        match self.finalise()? {
            Value::Int(i) => Ok(i),
            other => Err(Error::NotInt(other)),
        }
    }
}

impl<I: Identifier> TryInto<u64> for Value<I> {
    type Error = Error<I>;

    fn try_into(self) -> Result<u64, I> {
        match self.finalise()? {
            Value::UInt(u) => Ok(u),
            other => Err(Error::NotUInt(other)),
        }
    }
}

impl<I: Identifier> TryInto<f64> for Value<I> {
    type Error = Error<I>;

    fn try_into(self) -> Result<f64, I> {
        match self.finalise()? {
            Value::Float(f) => Ok(f),
            other => Err(Error::NotFloat(other)),
        }
    }
}

impl<I: Identifier> Value<I> {
    /// Reduce a lazy thunk down to a final value.
    pub fn finalise(mut self) -> ValueResult<I> {
        while let Value::Thunk(thunk) = self {
            let value = Value::Thunk(thunk.clone());
            self = thunk(value)?;
        }

        Ok(self)
    }

    /// Recursively reducy a data structure with lazy thunks to a final value.
    pub fn finalise_recursive(self) -> ValueResult<I> {
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
    pub fn as_function(mut self) -> Result<Rc<dyn Fn(ValueResult<I>) -> ValueResult<I>>, I> {
        self = self.finalise()?;

        if !self.can_call() {
            return Err(Error::NotFunction(self));
        }

        let call = move |value: ValueResult<I>| {
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
    pub fn call(mut self, args: impl Into<Vec<Value<I>>>) -> ValueResult<I> {
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

impl<I: Identifier> fmt::Display for Value<I> {
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

impl<I: fmt::Debug> fmt::Debug for Value<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;
        match self {
            Unit => write!(f, "Unit"),
            Bool(b) => write!(f, "Bool({:?})", b),
            Int(i) => write!(f, "Int({:?})", i),
            UInt(u) => write!(f, "UInt({:?})", u),
            Float(v) => write!(f, "Float({:?})", v),
            Pair(l, r) => write!(f, "Pair({:?}, {:?})", l, r),
            Left(l) => write!(f, "Left({:?})", l),
            Right(r) => write!(f, "Right({:?})", r),
            Function(_) => write!(f, "Function"),
            Except(_) => write!(f, "Except"),
            Thunk(_) => write!(f, "Thunk"),
        }
    }
}
