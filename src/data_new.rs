//! Data representation

use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;

use crate::lambda::Error;

pub type Cell<T> = Rc<RefCell<T>>;

pub fn new_cell<T>(value: T) -> Cell<T> {
    Rc::new(RefCell::new(value))
}

pub trait Show: fmt::Display + fmt::Debug {}

impl<T: fmt::Debug + fmt::Display> Show for T {}

pub type Result = ::std::result::Result<Value, Error>;

#[derive(Clone)]
pub enum Value {
    /// `Unit : Unit`
    Unit,
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
    /// Recursive lists of the form `forall a. rec t. () + (a, t)`
    ///
    /// Elements are stored in reverse order for optimal push, pop, and index semantics.
    List(Cell<Vec<Value>>, usize),
    /// Used to efficiently represent large pairs.
    ///
    /// Elements are stored in reverse order for consistency in index semantics.
    Record(Cell<Vec<Value>>, usize),
    /// `Left : forall a, b. a -> (a + b)`
    Left(Cell<Value>),
    /// `Right : n -> forall a[0..n], b. b -> (a[0] + (... + (a[n - 1] + b)))`
    Right(u64, Cell<Value>),
    /// `Function : forall a, b. (a -> b) -> Function`
    Function(Rc<dyn Fn(Value) -> Result>),
    /// `Thunk : forall a. (() -> a) -> a`
    Thunk(Rc<dyn Fn() -> Result>),
}

impl Value {
    /// Reduce a value down to a final result.
    pub fn reduce(&mut self) -> Result {
        while let Value::Thunk(thunk) = self {
            *self = thunk()?;
        }

        Ok(self.clone())
    }

    pub fn unit() -> Self {
        Value::Unit
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;
        match self {
            Unit => write!(f, "Unit"),
            Bool(b) => write!(f, "Bool({:?})", b),
            Int(i) => write!(f, "Int({:?})", i),
            UInt(u) => write!(f, "UInt({:?})", u),
            Float(n) => write!(f, "Float({:?})", n),
            Pair(l, r) => write!(f, "Pair({:?}, {:?})", l, r),
            List(values, length) => write!(f, "List({:?}, {:?})", values, length),
            Record(values, length) => write!(f, "Record({:?}, {:?})", values, length),
            Left(value) => write!(f, "Left({:?})", value),
            Right(n, value) => write!(f, "Right({:?}, {:?})", n, value),
            Function(_) => write!(f, "Function"),
            Thunk(_) => write!(f, "Thunk"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;
        match self {
            Unit => write!(f, "()"),
            Bool(b) => write!(f, "{}", b),
            Int(i) => write!(f, "{}i", i),
            UInt(u) => write!(f, "{}u", u),
            Float(n) => write!(f, "{}f", n),
            Pair(l, r) => write!(f, "({}, {})", l.borrow(), r.borrow()),
            List(values, length) => {
                if *length > values.borrow().len() {
                    write!(f, "!invalid record!")
                } else {
                    write!(f, "[")?;
                    for index in (0..*length).skip(1).rev() {
                        write!(f, "{}, ", values.borrow()[index])?;
                    }
                    if *length > 0 {
                        write!(f, "{}", values.borrow()[0])?;
                    }
                    write!(f, "]")
                }
            }
            Record(_, 0) => write!(f, "()"),
            Record(values, length) => {
                if *length > values.borrow().len() {
                    write!(f, "!invalid record!")
                } else {
                    write!(
                        f,
                        "({}, {})",
                        values.borrow()[length - 1],
                        Record(values.clone(), length - 1),
                    )
                }
            }
            Left(value) => write!(f, "(Left {})", value.borrow()),
            Right(0, value) => write!(f, "{}", value.borrow()),
            Right(n, value) => write!(f, "(Right {})", Right(n - 1, value.clone())),
            Function(_) => write!(f, "? -> ?"),
            Thunk(_) => write!(f, "?"),
        }
    }
}

impl Value {
    pub fn apply(&self, argument: Value) -> Result {
        if let Value::Function(function) = self {
            function(argument)
        } else {
            Err(Error::NotFunction(self.clone()))
        }
    }
}
