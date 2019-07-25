//! Data representation

use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;

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
    Unit,
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    Pair(Cell<Value>, Cell<Value>),
    Left(Cell<Value>),
    Right(Cell<Value>),
    Function(Rc<dyn Fn(Value) -> Result>),
    Except(Rc<dyn Fn(Result) -> Result>),
    Thunk(Rc<dyn Fn() -> Result>),
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
