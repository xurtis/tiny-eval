mod data;
mod builtin;
mod expr;

pub use data::Value;
pub use expr::construct::*;
pub use expr::{Expr, Context, Identifier, IdentifierView, eval};
pub use builtin::{value, function, Builtin};
pub use builtin::construct::*;

use std::rc::Rc;
use std::fmt;

/// Errors produced during evaluation
#[derive(Debug, Clone)]
pub enum Error {
    NotBool(Value),
    NotInt(Value),
    NotUInt(Value),
    NotFloat(Value),
    NotNumeric(Value),
    NotPair(Value),
    NotSum(Value),
    NotFunction(Value),
    NotBound(Rc<dyn IdentifierView>),
    External(Rc<dyn ::std::error::Error>),
    Raise(Value),
}

impl fmt::Display for Error {
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

impl ::std::error::Error for Error {}

pub type Result<T> = ::std::result::Result<T, Error>;
