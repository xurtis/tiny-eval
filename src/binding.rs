//! Binding language

use crate::data_new::{Value, Cell};
use crate::lambda;
use crate::typing::{Numeric, Integer};

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;
use std::rc::Rc;

use failure::{Error, format_err};

type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Operator {
    /// `Roll : T (rec t. T t) -> rec t. T t`
    Roll,
    /// `Unroll : rec t. T t -> T (rec t. T t)`
    Unroll,
    /// `Raise : forall a, b. a -> b`
    Raise,
    /// `Except : forall a. (forall b. b -> a) -> a -> a`
    Except,
    /// `Pair : forall a, b. a -> b -> (a, b)`
    Pair,
    /// `First : forall a, b. (a, b) -> a`
    First,
    /// `Second : forall a, b. (a, b) -> b`
    Second,
    /// `Element n : forall a[0..n], b. (a[0], (..., (a[n - 1], b))) -> b`
    ///
    /// Allows for quickly selecting an element of a deeply nested pair by effectively performing n
    /// stacked selects on the right.
    Element(u64),
    /// `Index : UInt -> forall a. rec t. () + (a, t) -> () + a`
    Index,
    /// `Left : forall a, b. a -> a + b`
    Left,
    /// `Right : forall a, b. b -> a + b`
    Right,
    /// `Case : forall a, b, c. (a -> c) -> (b -> c) -> (a + b) -> c`
    Case,
    /// `SkipLeft n : forall a[0..n], b, c. (forall a. a -> c) -> (b -> c) -> (a[0] + (... + (a[n - 1] + b))) -> c`
    ///
    /// Allows for quickly matching deeply nested sum values to match a single specific case by
    /// discarding effectively performing n stacked matches on the right.
    SkipLeft(u64),
    /// `Not : Bool -> Bool`
    Not,
    /// `And : Bool -> Bool -> Bool`
    And,
    /// `Or : Bool -> Bool -> Bool`
    Or,
    /// `Greater : Numeric n => n -> n -> n`
    Greater(Numeric),
    /// `GreaterEqual : Numeric n => n -> n -> Bool`
    GreaterOrEqual(Numeric),
    /// `Less : Numeric n => n -> n -> Bool`
    Less(Numeric),
    /// `LessEqual : Numeric n => n -> n -> Bool`
    LessOrEqual(Numeric),
    /// `Equal : Numeric n => n -> n -> n`
    Equal(Numeric),
    /// `NotEqual : Numeric n => n -> n -> n`
    NotEqual(Numeric),
    /// `Add : Numeric n => n -> n -> n`
    Add(Numeric),
    /// `Subtract : Numeric n => n -> n -> n`
    Subtract(Numeric),
    /// `Multiply : Numeric n => n -> n -> n`
    Multiply(Numeric),
    /// `Divide : Numeric n => n -> n -> n`
    Divide(Numeric),
    /// `Remainder : Numeric n => n -> n -> n`
    Remainder(Numeric),
    /// `ShiftLeft : Integer i => i -> i`
    ShiftLeft(Integer),
    /// `ShiftRight : Integer i => i -> i`
    ShiftRight(Integer),
    /// `BitwiseNot : Integer i => i -> i`
    BitwiseNot(Integer),
    /// `BitwiseAnd : Integer i => i -> i`
    BitwiseAnd(Integer),
    /// `BitwiseOr : Integer i => i -> i`
    BitwiseOr(Integer),
    /// `BitwiseExclusiveOr : Integer i => i -> i`
    BitwiseExclusiveOr(Integer),
    /// `Signed : Integer i => i -> Int`
    Signed(Integer),
    /// `Unsigned : Integer i => i -> UInt`
    Unsigned(Integer),
    /// `Float : Integer i => i -> Float`
    Float(Integer),
    /// `Ceiling : Float -> Int`
    Ceiling,
    /// `Floor : Float -> Int`
    Floor,
    /// `RoundUp : Float -> Int`
    ///
    /// Rounds so that exact halves increase in magnitude (0.5 -> 1 and -0.5 -> -1).
    RoundUp,
    /// `RoundDown : Float -> Int`
    ///
    /// Rounds so that exact halves decrease in magnitude (0.5 -> 0 and -0.5 -> 0).
    RoundDown,
}

impl Into<lambda::Operator> for Operator {
    fn into(self) -> lambda::Operator {
        match self {
            Operator::Roll => lambda::Operator::Roll,
            Operator::Unroll => lambda::Operator::Unroll,
            Operator::Raise => lambda::Operator::Raise,
            Operator::Except => lambda::Operator::Except,
            Operator::Pair => lambda::Operator::Pair,
            Operator::First => lambda::Operator::First,
            Operator::Second => lambda::Operator::Second,
            Operator::Element(count) => lambda::Operator::Element(count),
            Operator::Index => lambda::Operator::Index,
            Operator::Left => lambda::Operator::Left,
            Operator::Right => lambda::Operator::Right,
            Operator::Case => lambda::Operator::Case,
            Operator::SkipLeft(count) => lambda::Operator::SkipLeft(count),
            Operator::Not => lambda::Operator::Not,
            Operator::And => lambda::Operator::And,
            Operator::Or => lambda::Operator::Or,
            Operator::Greater(numeric) => lambda::Operator::Greater(numeric),
            Operator::GreaterOrEqual(numeric) => lambda::Operator::GreaterOrEqual(numeric),
            Operator::Less(numeric) => lambda::Operator::Less(numeric),
            Operator::LessOrEqual(numeric) => lambda::Operator::LessOrEqual(numeric),
            Operator::Equal(numeric) => lambda::Operator::Equal(numeric),
            Operator::NotEqual(numeric) => lambda::Operator::NotEqual(numeric),
            Operator::Add(numeric) => lambda::Operator::Add(numeric),
            Operator::Subtract(numeric) => lambda::Operator::Subtract(numeric),
            Operator::Multiply(numeric) => lambda::Operator::Multiply(numeric),
            Operator::Divide(numeric) => lambda::Operator::Divide(numeric),
            Operator::Remainder(numeric) => lambda::Operator::Remainder(numeric),
            Operator::ShiftLeft(integer) => lambda::Operator::ShiftLeft(integer),
            Operator::ShiftRight(integer) => lambda::Operator::ShiftRight(integer),
            Operator::BitwiseNot(integer) => lambda::Operator::BitwiseNot(integer),
            Operator::BitwiseAnd(integer) => lambda::Operator::BitwiseAnd(integer),
            Operator::BitwiseOr(integer) => lambda::Operator::BitwiseOr(integer),
            Operator::BitwiseExclusiveOr(integer) => lambda::Operator::BitwiseExclusiveOr(integer),
            Operator::Signed(integer) => lambda::Operator::Signed(integer),
            Operator::Unsigned(integer) => lambda::Operator::Unsigned(integer),
            Operator::Float(integer) => lambda::Operator::Float(integer),
            Operator::Ceiling => lambda::Operator::Ceiling,
            Operator::Floor => lambda::Operator::Floor,
            Operator::RoundUp => lambda::Operator::RoundUp,
            Operator::RoundDown => lambda::Operator::RoundDown,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr<I> {
    /// Binding of an expression to a name
    Bind(Rc<I>, Cell<Expr<I>>, Cell<Expr<I>>),
    /// Named variable reference
    Variable(Rc<I>),
    /// Lambda variable binding
    Lambda(Rc<I>, Cell<Expr<I>>),
    /// Builtin value
    Value(Value),
    /// Builtin operator
    Operator(Operator),
}

enum VariableBind {
    Index,
    Expression(lambda::Expr),
}

impl<I: Eq + fmt::Display> Expr<I> {
    pub fn eval(&self) -> Result<Value> {
        self.unbind().and_then(|expr| expr.eval())
    }

    /// Translate into a pure lambda expression by removing all bindings.
    ///
    /// May fail if there are any free variables.
    pub fn unbind(&self) -> Result<lambda::Expr> {
        let mut bindings = Vec::new();
        self.unbind_rec(&mut bindings)
    }

    fn unbind_rec(&self, bindings: &mut Vec<(Rc<I>, VariableBind)>) -> Result<lambda::Expr> {
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
            Expr::Value(value) => Ok(lambda::Expr::Value(value.clone())),
            Expr::Operator(op) => Ok(lambda::Expr::Operator(op.clone().into())),
        }
    }
}

impl<I: Hash + Eq + Clone + fmt::Display> Expr<I> {
    pub fn substitute(&mut self, context: &Context<I>) -> Result<()> {
        let mut bound = Vec::new();
        self.substitute_rec(context, &mut bound)
    }

    fn substitute_rec(&mut self, context: &Context<I>, bound: &mut Vec<Rc<I>>) -> Result<()> {
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
            _ => {}
        }
    }
}

impl<I: fmt::Display> fmt::Display for Expr<I> {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
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
    pub fn bind(&mut self, name: I, expr: impl Into<Expr<I>>) -> Result<()> {
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
    pub fn eval_named(&mut self, name: impl AsRef<I>) -> Result<Value> {
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
    pub fn eval(&mut self, mut expr: Expr<I>) -> Result<Value> {
        for variable in expr.free_variables() {
            if !self.is_evaluated(&variable) {
                self.eval_named(variable)?;
            }
        }

        expr.substitute(self)?;
        expr.eval()
    }

    /// Ensure the name has not been bound to an expression or value
    fn ensure_undefined<'c>(&'c self, name: &I) -> Result<&'c Self> {
        if self.0.contains_key(name) {
            Err(name_already_bound(name))
        } else {
            Ok(self)
        }
    }

    /// Check whether introducing a given expression with a given name would introduce cycles in
    /// the dependency graph. This is done by a depth first search on all dependant variables to
    /// see if the refer back to the proposed name.
    fn check_cycles<'c>(&'c self, name: Rc<I>, expr: &Expr<I>) -> Result<&'c Self> {
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

    fn get_bound(&self, name: &I) -> Result<&Binding<I>> {
        self.0.get(name).ok_or_else(|| name_not_bound(name))
    }

    fn take_bound(&mut self, name: &I) -> Result<Binding<I>> {
        self.0.remove(name).ok_or_else(|| name_not_bound(name))
    }

    fn is_evaluated(&self, name: &I) -> bool {
        self.0.get(name).map(|binding| binding.is_evaluated()).unwrap_or(false)
    }

    fn get_value(&self, name: &I) -> Result<Value> {
        self.get_bound(name)?.value().ok_or(format_err!("{} is not evaluated", name))
    }
}

#[derive(Debug, Clone)]
enum Binding<I> {
    Bound(Expr<I>),
    Evaluated(Value),
}

impl<I: Hash + Eq + Clone + fmt::Display> Binding<I> {
    fn eval(&mut self) -> Result<()> {
        match self {
            Binding::Evaluated(_) => Ok(()),
            Binding::Bound(expr) => {
                *self = Binding::Evaluated(expr.eval()?);
                Ok(())
            }
        }
    }

    fn substitute(&mut self, context: &Context<I>) -> Result<()> {
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

fn name_already_bound<I: fmt::Display>(identifier: &I) -> Error {
    format_err!("Name already bound: {}", identifier)
}

fn recursion<I: fmt::Display>(identifier: &I) -> Error {
    format_err!("Recursion dected in binding of {}", identifier)
}

fn name_not_bound<I: fmt::Display>(identifier: &I) -> Error {
    format_err!("Name not bound: {}", identifier)
}
