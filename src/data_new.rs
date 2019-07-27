//! Data representation

use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;
use std::ops::Range;

pub type Cell<T> = Rc<RefCell<T>>;

pub fn new_cell<T>(value: T) -> Cell<T> {
    Rc::new(RefCell::new(value))
}

pub trait Show: fmt::Display + fmt::Debug {}

impl<T: fmt::Debug + fmt::Display> Show for T {}

#[derive(Debug)]
pub enum Error {
    External(Box<dyn Show>),
    Internal(Value),
}

impl Clone for Error {
    fn clone(&self) -> Self {
        unimplemented!()
    }
}

pub type Result = ::std::result::Result<Value, Error>;

#[derive(Clone)]
pub enum Value {
    /// `Bool : bool -> Bool`
    Bool(bool),
    /// `Int : i64 -> Int`
    Int(i64),
    /// `UInt : u64 -> UInt`
    UInt(u64),
    /// `Float : f64 -> Float`
    Float(f64),
    /// `Pair : forall a, b. a -> b -> (a, b)`
    Pair(Cell<Value>, Cell<Value>),
    /// List and the canonical element in the list that is the start of the list.
    ///
    /// Used to represent the unit value (`()`), or an empty list
    /// (`() : forall a. rec t. () + (a, t)`) as well as a list of any length or any large pair.
    ///
    /// The additional head index allows for representation of the tail of the list without
    /// duplicating the entire list and mutating it.
    List(Cell<Vec<Value>>, usize),
    /// `Left : forall a, b. a -> (a + b)`
    Left(Cell<Value>),
    /// `Right : n -> forall a[0..n], b. b -> (a[0] + (... + (a[n - 1] + b)))`
    Right(u64, Cell<Value>),
    /// `Function : forall a, b. (a -> b) -> Function`
    Function(Rc<dyn Fn(Value) -> Result>),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}

impl Value {
    pub fn apply(&self, argument: Result) -> Result {
        unimplemented!()
    }
}
