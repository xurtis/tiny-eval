mod data;
mod builtin;
mod expr;

pub type Result<T> = ::std::result::Result<T, ::failure::Error>;

pub use expr::construct::*;
pub use expr::{Expr, Context, eval};
pub use builtin::{val, error, Builtin};
pub use Builtin::*;
