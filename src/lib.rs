mod data_new;
pub mod binding;
pub mod lambda;
pub mod typing;
pub mod operator;

use std::fmt;

/// Errors produced during evaluation
#[derive(Debug)]
pub enum Error<I> {
    Eval(lambda::Error),
    Expr(binding::Error<I>),
}

impl<I: fmt::Display> fmt::Display for Error<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Eval(error) => write!(f, "Evaluation: {}", error),
            Error::Expr(error) => write!(f, "Binding experssion: {}", error),
        }
    }
}

impl<I> From<lambda::Error> for Error<I> {
    fn from(error: lambda::Error) -> Self {
        Error::Eval(error)
    }
}

impl<I> From<binding::Error<I>> for Error<I> {
    fn from(error: binding::Error<I>) -> Self {
        Error::Expr(error)
    }
}

impl<I: fmt::Display + fmt::Debug> ::std::error::Error for Error<I> {}

pub type Result<T, I> = ::std::result::Result<T, Error<I>>;
