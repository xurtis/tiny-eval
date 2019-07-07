//! Evaluable expressions

use std::rc::Rc;
use std::fmt;

use failure::format_err;

use crate::Result;
use crate::builtin::Builtin;
use crate::data::Value;

pub fn eval<I: Identifier + Clone + 'static>(expr: impl Into<Expr<I>>) -> Result<Value> {
    Context::new().eval(&expr.into())
}

/// Types that can be used as identifiers in expressions
pub trait Identifier: Eq + ::std::fmt::Display {}

impl<I: ::std::hash::Hash + Eq + ::std::fmt::Display> Identifier for I {}

#[derive(Clone, Debug)]
pub enum Expr<I: Identifier> {
    Builtin(Builtin),
    Apply(Rc<Expr<I>>, Rc<Expr<I>>),
    Lambda(I, Rc<Expr<I>>),
    Bind {
        name: I,
        value: Rc<Expr<I>>,
        expr: Rc<Expr<I>>,
    },
    Variable(I),
}

pub mod construct {
    use std::rc::Rc;

    use super::{Expr, Identifier};

    /// Apply an argument to a function
    pub fn apply<I: Identifier>(
        function: impl Into<Expr<I>>,
        argument: impl Into<Expr<I>>,
    ) -> Expr<I> {
        Expr::Apply(Rc::new(function.into()), Rc::new(argument.into()))
    }

    pub fn lambda<I: Identifier>(name: I, expr: impl Into<Expr<I>>) -> Expr<I> {
        Expr::Lambda(name, Rc::new(expr.into()))
    }

    pub fn bind<I: Identifier>(
        name: I,
        value: impl Into<Expr<I>>,
        expr: impl Into<Expr<I>>,
    ) -> Expr<I> {
        let value = Rc::new(value.into());
        let expr = Rc::new(expr.into());
        Expr::Bind { name, value, expr }
    }

    #[macro_export]
    macro_rules! apply_expr {
        (@apply($applied:expr): ) => {
            $applied
        };
        (@apply($applied:expr): $arg:ident) => {
            apply($applied, $arg)
        };
        (@apply($applied:expr): $arg:ident, $($rest:ident),*) => {
            apply_expr!(@apply(apply($applied, $arg)): $($rest),*)
        };
        ($(#[$doc:meta])* $name:ident($($arg:ident),*)) => {
            $(#[$doc])*
            fn $name(
                $($arg: impl Into<$crate::Expr<&'static str>>),*
            ) -> $crate::Expr<&'static str> {
                let function = $crate::Expr::<&'static str>::from(stringify!($name));
                apply_expr!(@apply(function): $($arg),*)
            }
        };
        ($(#[$doc:meta])* pub $name:ident($($arg:ident),*)) => {
            $(#[$doc])*
            pub fn $name(
                $($arg: impl Into<$crate::Expr<&'static str>>),*
            ) -> $crate::Expr<&'static str> {
                let function = $crate::Expr::<&'static str>::from(stringify!($name));
                apply_expr!(@apply(function): $($arg),*)
            }
        };
        ($(#[$doc:meta])* pub($($scope:tt)*) $name:ident($($arg:ident),*)) => {
            $(#[$doc])*
            pub($($scope)*) fn $name(
                $($arg: impl Into<$crate::Expr<&'static str>>),*
            ) -> $crate::Expr<&'static str> {
                let function = $crate::Expr::<&'static str>::from(stringify!($name));
                apply_expr!(@apply(function): $($arg),*)
            }
        };
    }
}

impl<I: Identifier> fmt::Display for Expr<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Builtin(b) => write!(f, "{}", b),
            Expr::Apply(l, a) => write!(f, "({} {})", l, a),
            Expr::Lambda(n, v) => write!(f, "({} -> {})", n, v),
            Expr::Bind { name, value, expr } => {
                write!(f, "(let {} := {} in {})", name, value, expr)
            }
            Expr::Variable(n) => write!(f, "{}", n),
        }
    }
}

impl<I: Identifier> From<I> for Expr<I> {
    fn from(ident: I) -> Self {
        Expr::Variable(ident)
    }
}

impl<I: Identifier> From<Builtin> for Expr<I> {
    fn from(value: Builtin) -> Self {
        Expr::Builtin(value)
    }
}

#[derive(Clone, Debug)]
pub struct Context<I: Identifier>(Binding<I>);

impl<I: Identifier> Default for Context<I> {
    fn default() -> Self {
        Context(Binding::new())
    }
}

impl<I: Identifier + Clone + 'static> Context<I> {
    /// Create a new empty context
    pub fn new() -> Self {
        Default::default()
    }

    /// Add a binding expression to the context
    ///
    /// Returns the evaluated result of the added expression.
    pub fn extend(&mut self, name: I, expr: impl Into<Expr<I>>) -> Result<Value> {
        let expr = expr.into();
        let value = {
            let context = self.clone();
            let expr = expr.clone();
            let name = name.clone();
            Value::Thunk(Rc::new(move |thunk| {
                let mut context = context.clone();
                context.0.extend(name.clone(), thunk, Some(expr.clone()));
                context.eval(&expr)
            }))
        };
        self.0.extend(name, value.clone(), Some(expr));
        Ok(value)
    }

    /// Find a named expression within a context
    pub fn find_expr(&self, name: impl AsRef<I>) -> Option<&Expr<I>> {
        self.0.find_expr(name.as_ref())
    }

    /// Find a named value within a context
    pub fn find(&self, name: impl AsRef<I>) -> Option<&Value> {
        self.0.find(name.as_ref())
    }

    /// Evaluate an expression within a context
    pub fn eval(&self, expr: &Expr<I>) -> Result<Value> {
        match expr {
            Expr::Builtin(b) => b.eval(),
            Expr::Apply(function, argument) => {
                let function = self.eval(function)?;
                let argument = self.eval(argument);
                (function.as_function()?)(argument)
            }
            Expr::Lambda(name, value) => {
                let closure = self.clone();
                let value = value.clone();
                let name = name.clone();
                let lambda = move |argument: Value| {
                    let closure = closure.clone();
                    let name = name.clone();
                    let value = value.clone();
                    Ok(Value::Thunk(Rc::new(move |_| {
                        let mut closure = closure.clone();
                        closure.0.extend(name.clone(), argument.clone(), None);
                        closure.eval(&value)
                    })))
                };
                Ok(Value::Function(Rc::new(lambda)))
            }
            Expr::Bind { name, value, expr } => {
                let mut context = self.clone();
                context.extend(name.clone(), value.as_ref().clone())?;
                context.eval(expr)
            }
            Expr::Variable(name) => {
                let value = self.0.find(name).ok_or(format_err!("No such variable: {}", name))?;
                Ok(value.clone())
            }
        }
    }
}

impl<I: Identifier> fmt::Display for Context<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

/// Linked list of bindings (more efficient to copy)
#[derive(Debug, Clone)]
enum Binding<I: Identifier> {
    Empty,
    Bind {
        name: I,
        value: Value,
        expr: Option<Rc<Expr<I>>>,
        tail: Rc<Binding<I>>,
    }
}

impl<I: Identifier> Binding<I> {
    fn new() -> Self {
        Binding::Empty
    }

    fn is_empty(&self) -> bool {
        match self {
            Binding::Empty => true,
            _ => false,
        }
    }
}

impl<I: Identifier> Binding<I> {
    /// Add a binding expression to the context
    ///
    /// Returns the evaluated result of the added expression.
    fn extend(&mut self, name: I, value: Value, expr: Option<Expr<I>>) {
        let expr = expr.map(Rc::new);
        let mut tail = Binding::Empty;
        ::std::mem::swap(&mut tail, self);
        let tail = Rc::new(tail);
        *self = Binding::Bind { name, value, expr, tail };
    }

    /// Find a named expression within a context
    fn find_expr(&self, name: &I) -> Option<&Expr<I>> {
        use Binding::*;
        match self {
            Bind { name: bound, expr, .. } if name == bound => expr.as_ref().map(|e| e.as_ref()),
            Bind { tail, .. } => tail.find_expr(name),
            _ => None
        }
    }

    /// Find a named value within a context
    fn find(&self, name: &I) -> Option<&Value> {
        use Binding::*;
        match self {
            Bind { name: bound, value, .. } if name == bound => Some(value),
            Bind { tail, .. } => tail.find(name),
            _ => None
        }
    }
}

impl<I: Identifier> fmt::Display for Binding<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Binding::*;
        match self {
            Bind { name, expr: Some(expr), value, tail } if !value.can_render() => {
                write!(f, "{} := {}", name, expr)?;
                if !tail.is_empty() {
                    write!(f, ", ")?;
                    fmt::Display::fmt(tail, f)?;
                }
            },
            Bind { name, value, tail, .. } => {
                write!(f, "{} := {}", name, value)?;
                if !tail.is_empty() {
                    write!(f, ", ")?;
                    fmt::Display::fmt(tail, f)?;
                }
            }
            _ => {}
        }

        Ok(())
    }
}
