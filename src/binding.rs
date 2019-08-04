//! Binding language

use crate::data_new::{Value, Cell, new_cell};
use crate::lambda;
use crate::operator::Operator;

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Expr<I> {
    /// Binding of an expression to a name
    Bind(Rc<I>, Cell<Expr<I>>, Cell<Expr<I>>),
    /// Lambda variable binding
    Lambda(Rc<I>, Cell<Expr<I>>),
    /// Application of a function
    Apply(Cell<Expr<I>>, Cell<Expr<I>>),
    /// Named variable reference
    Variable(Rc<I>),
    /// Builtin value
    Value(Value),
    /// Builtin operator
    Operator(Operator),
    /// Exception handling
    Except(Cell<Expr<I>>, Cell<Expr<I>>),
}

enum VariableBind {
    Index,
    Expression(lambda::Expr),
}

impl<I: Eq + Hash + fmt::Display + Clone> Expr<I> {
    pub fn eval(&self) -> Result<Value, I> {
        Ok(self.unbind()?.eval()?)
    }

    /// Translate into a pure lambda expression by removing all bindings.
    ///
    /// May fail if there are any free variables.
    pub fn unbind(&self) -> Result<lambda::Expr, I> {
        let mut bindings = Vec::new();
        self.unbind_rec(&mut bindings)
    }

    fn unbind_rec(&self, bindings: &mut Vec<(Rc<I>, VariableBind)>) -> Result<lambda::Expr, I> {
        use VariableBind::*;
        match self {
            Expr::Bind(name, value, expr) => {
                let value = value.borrow().unbind_rec(bindings)?;
                bindings.push((name.clone(), Expression(value)));
                let expr = expr.borrow().unbind_rec(bindings)?;
                bindings.pop();
                Ok(expr)
            }
            Expr::Variable(name) => {
                let mut index = 0;
                for (bound, expr) in bindings.iter().rev() {
                    match expr {
                        Expression(expr) if bound == name => {
                            return Ok(expr.clone());
                        }
                        Index if bound == name => {
                            return Ok(lambda::Expr::Variable(index));
                        }
                        Index if bound != name => {
                            index += 1;
                        }
                        _ => {}
                    }
                }

                Err(name_not_bound(name))
            }
            Expr::Lambda(name, expr) => {
                bindings.push((name.clone(), Index));
                let expr = expr.borrow().unbind_rec(bindings)?;
                bindings.pop();
                Ok(expr)
            }
            Expr::Apply(function, argument) => {
                let function = function.borrow().unbind_rec(bindings)?;
                let argument = argument.borrow().unbind_rec(bindings)?;
                Ok(lambda::Expr::Apply(new_cell(function), new_cell(argument)))
            }
            Expr::Value(value) => Ok(lambda::Expr::Value(value.clone())),
            Expr::Operator(op) => Ok(lambda::Expr::Operator(op.clone().into())),
            Expr::Except(try_expr, except) => {
                let try_expr = try_expr.borrow().unbind_rec(bindings)?;
                let except = except.borrow().unbind_rec(bindings)?;
                Ok(lambda::Expr::Except(new_cell(try_expr), new_cell(except)))
            }
        }
    }
}

impl<I: Hash + Eq + Clone + fmt::Display> Expr<I> {
    pub fn substitute(&mut self, context: &Context<I>) -> Result<(), I> {
        let mut bound = Vec::new();
        self.substitute_rec(context, &mut bound)
    }

    fn substitute_rec(&mut self, context: &Context<I>, bound: &mut Vec<Rc<I>>) -> Result<(), I> {
        use Expr::*;
        match self {
            Bind(name, value, expr) => {
                value.borrow_mut().substitute_rec(context, bound)?;
                bound.push(name.clone());
                expr.borrow_mut().substitute_rec(context, bound)?;
                bound.pop();
            }
            Variable(name) => {
                if !bound.contains(&name) {
                    *self = Value(context.get_value(&name)?);
                }
            }
            Lambda(name, expr) => {
                bound.push(name.clone());
                expr.borrow_mut().substitute_rec(context, bound)?;
                bound.pop();
            }
            Apply(function, argument) => {
                function.borrow_mut().substitute_rec(context, bound)?;
                argument.borrow_mut().substitute_rec(context, bound)?;
            }
            Except(try_expr, except) => {
                try_expr.borrow_mut().substitute_rec(context, bound)?;
                except.borrow_mut().substitute_rec(context, bound)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn free_variables(&self) -> HashSet<Rc<I>> {
        let mut variables = HashSet::new();
        let mut bound = Vec::new();
        self.free_variables_rec(&mut variables, &mut bound);
        variables
    }

    fn free_variables_rec(&self, free: &mut HashSet<Rc<I>>, bound: &mut Vec<Rc<I>>) {
        use Expr::*;
        match self {
            Bind(name, value, expr) => {
                value.borrow().free_variables_rec(free, bound);
                bound.push(name.clone());
                expr.borrow().free_variables_rec(free, bound);
                bound.pop();
            }
            Variable(name) => {
                if !bound.contains(&name) {
                    free.insert(name.clone());
                }
            }
            Lambda(name, expr) => {
                bound.push(name.clone());
                expr.borrow().free_variables_rec(free, bound);
                bound.pop();
            }
            Apply(function, argument) => {
                function.borrow().free_variables_rec(free, bound);
                argument.borrow().free_variables_rec(free, bound);
            }
            Except(try_expr, except) => {
                try_expr.borrow().free_variables_rec(free, bound);
                except.borrow().free_variables_rec(free, bound);
            }
            _ => {}
        }
    }
}

impl<I: fmt::Display> fmt::Display for Expr<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Expr::*;
        match self {
            Bind(name, value, expr) => {
                write!(f, "(let {} := {} in {})", name, value.borrow(), expr.borrow())
            }
            Lambda(name, expr) => write!(f, "({} -> {})", name, expr.borrow()),
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

#[derive(Debug, Clone)]
pub struct Context<I: Hash + Eq>(HashMap<Rc<I>, Binding<I>>);

impl<I: Hash + Eq> Default for Context<I> {
    fn default() -> Self {
        Context(HashMap::new())
    }
}

impl<I: Hash + Eq + fmt::Display + Clone> Context<I> {
    /// Create a new empty context
    pub fn new() -> Self {
        Default::default()
    }

    /// Add a binding expression to the context
    ///
    /// Can fail if there is introducing the expression would cause a cycle.
    /// Can fail if the name is already bound.
    pub fn bind(&mut self, name: I, expr: impl Into<Expr<I>>) -> Result<(), I> {
        let expr = expr.into();
        let name = Rc::new(name);
        self.ensure_undefined(&name)?.check_cycles(name.clone(), &expr)?;

        if let Some(binding) = self.0.get_mut(&name) {
            *binding = Binding::Bound(expr);
        } else {
            self.0.insert(name, Binding::Bound(expr));
        }

        Ok(())
    }

    /// Reduce the named expression to a lambda expression
    ///
    /// Can fail if the name is not bound in the context.
    /// Can fail if not all values in the bound expression are bound to a value.
    pub fn eval_named(&mut self, name: impl AsRef<I>) -> Result<Value, I> {
        let mut eval_stack = Vec::new();
        let name = Rc::new(name.as_ref().clone());
        let binding = self.take_bound(&name)?;
        eval_stack.push((name.clone(), binding, false));

        while let Some((name, mut binding, expanded)) = eval_stack.pop() {
            if expanded {
                // We've already evaluated the free variables
                binding.substitute(&self)?;
                binding.eval()?;
                self.0.insert(name, binding);
            } else {
                // We haven't checked all of the free variables yet so we just push on all
                // unevaluated dependent variables.
                let free_variables = binding.free_variables();
                eval_stack.push((name, binding, true));
                for variable in free_variables {
                    if !self.is_evaluated(&variable) {
                        let binding = self.take_bound(&variable)?;
                        eval_stack.push((variable, binding, false));
                    }
                }
            }
        }

        self.get_value(&name)
    }

    /// Evaluate an expression within a context
    pub fn eval(&mut self, mut expr: Expr<I>) -> Result<Value, I> {
        for variable in expr.free_variables() {
            if !self.is_evaluated(&variable) {
                self.eval_named(variable)?;
            }
        }

        expr.substitute(self)?;
        expr.eval()
    }

    /// Ensure the name has not been bound to an expression or value
    fn ensure_undefined<'c>(&'c self, name: &I) -> Result<&'c Self, I> {
        if self.0.contains_key(name) {
            Err(name_already_bound(name))
        } else {
            Ok(self)
        }
    }

    /// Check whether introducing a given expression with a given name would introduce cycles in
    /// the dependency graph. This is done by a depth first search on all dependant variables to
    /// see if the refer back to the proposed name.
    fn check_cycles<'c>(&'c self, name: Rc<I>, expr: &Expr<I>) -> Result<&'c Self, I> {
        let mut unchecked = expr.free_variables().into_iter().collect::<Vec<_>>();
        let mut checked = HashSet::new();

        while let Some(variable) = unchecked.pop() {
            if variable == name {
                return Err(recursion(&name));
            }

            checked.insert(variable.clone());

            if let Some(binding) = self.0.get(&variable) {
                if let Some(expr) = binding.expr() {
                    for free_variable in expr.free_variables() {
                        if !checked.contains(&free_variable) {
                            unchecked.push(free_variable.clone())
                        }
                    }
                }
            }
        }

        Ok(self)
    }

    fn get_bound(&self, name: &I) -> Result<&Binding<I>, I> {
        self.0.get(name).ok_or_else(|| name_not_bound(name))
    }

    fn take_bound(&mut self, name: &I) -> Result<Binding<I>, I> {
        self.0.remove(name).ok_or_else(|| name_not_bound(name))
    }

    fn is_evaluated(&self, name: &I) -> bool {
        self.0.get(name).map(|binding| binding.is_evaluated()).unwrap_or(false)
    }

    fn get_value(&self, name: &I) -> Result<Value, I> {
        self.get_bound(name)?.value().ok_or(expr_not_evaluated(name))
    }
}

#[derive(Debug, Clone)]
enum Binding<I> {
    Bound(Expr<I>),
    Evaluated(Value),
}

impl<I: Hash + Eq + Clone + fmt::Display> Binding<I> {
    fn eval(&mut self) -> Result<(), I> {
        match self {
            Binding::Evaluated(_) => Ok(()),
            Binding::Bound(expr) => {
                *self = Binding::Evaluated(expr.eval()?);
                Ok(())
            }
        }
    }

    fn substitute(&mut self, context: &Context<I>) -> Result<(), I> {
        if let Binding::Bound(expr) = self {
            expr.substitute(context)
        } else {
            Ok(())
        }
    }

    fn free_variables(&self) -> HashSet<Rc<I>> {
        match self {
            Binding::Bound(expr) => expr.free_variables(),
            Binding::Evaluated(_) => HashSet::new(),
        }
    }
}

impl<I> Binding<I> {
    fn is_evaluated(&self) -> bool {
        if let Binding::Evaluated(_) = self {
            true
        } else {
            false
        }
    }

    fn value(&self) -> Option<Value> {
        if let Binding::Evaluated(value) = self {
            Some(value.clone())
        } else {
            None
        }
    }

    fn expr(&self) -> Option<&Expr<I>> {
        if let Binding::Bound(expr) = self {
            Some(expr)
        } else {
            None
        }
    }
}

fn name_already_bound<I: fmt::Display + Clone>(identifier: &I) -> crate::Error<I> {
    Error::AlreadyBound(identifier.clone()).into()
}

fn recursion<I: fmt::Display + Clone>(identifier: &I) -> crate::Error<I> {
    Error::Recursion(identifier.clone()).into()
}

fn name_not_bound<I: fmt::Display + Clone>(identifier: &I) -> crate::Error<I> {
    Error::NotBound(identifier.clone()).into()
}

fn expr_not_evaluated<I: fmt::Display + Clone>(identifier: &I) -> crate::Error<I> {
    Error::NotEvaluated(identifier.clone()).into()
}

#[derive(Debug)]
pub enum Error<I> {
    NotBound(I),
    AlreadyBound(I),
    Recursion(I),
    NotEvaluated(I),
}

impl<I: fmt::Display> fmt::Display for Error<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::*;
        match self {
            NotBound(identifier) => write!(f, "{} is not bound", identifier),
            AlreadyBound(identifier) => write!(f, "{} is already bound", identifier),
            Recursion(identifier) => write!(f, "Recursion dected in binding of {}", identifier),
            NotEvaluated(identifier) => write!(f, "{} has not been evaluated", identifier),
        }
    }
}

type Result<T, I> = ::std::result::Result<T, super::Error<I>>;
