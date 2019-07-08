mod data;
mod builtin;
mod expr;

pub use data::Value;
pub use expr::construct::*;
pub use expr::{Identifier, Expr, Context, eval};
pub use builtin::{value, function, Builtin};
pub use builtin::construct::*;

use std::rc::Rc;
use std::fmt;

/// Errors produced during evaluation
#[derive(Debug, Clone)]
pub enum Error<I> {
    NotBool(Value<I>),
    NotInt(Value<I>),
    NotUInt(Value<I>),
    NotFloat(Value<I>),
    NotNumeric(Value<I>),
    NotPair(Value<I>),
    NotSum(Value<I>),
    NotFunction(Value<I>),
    NotBound(I),
    External(Rc<dyn ::std::error::Error>),
    Raise(Value<I>),
}

impl<I: Identifier> fmt::Display for Error<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::*;
        match self {
            NotBool(value) => write!(f, "{} is not a boolean", value),
            NotInt(value) => write!(f, "{} is not a integer", value),
            NotUInt(value) => write!(f, "{} is not an unsigned integer", value),
            NotFloat(value) => write!(f, "{} is not a float", value),
            NotNumeric(value) => write!(f, "{} is not numeric", value),
            NotPair(value) => write!(f, "{} is not a pair", value),
            NotSum(value) => write!(f, "{} is not a sum", value),
            NotFunction(value) => write!(f, "{} is not a function", value),
            NotBound(identifier) => write!(f, "{} is not bound", identifier),
            External(error) => write!(f, "{}", error),
            Raise(value) => write!(f, "{} raised as error by program", value),
        }
    }
}

impl<I: Identifier> ::std::error::Error for Error<I> {}

pub type Result<T, I> = ::std::result::Result<T, Error<I>>;
pub type ValueResult<I> = Result<Value<I>, I>;

