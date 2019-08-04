//! Data representation

use std::rc::Rc;
use std::cell::RefCell;
use std::convert::TryInto;
use std::fmt;

use crate::lambda::Error;

pub type Cell<T> = Rc<RefCell<T>>;

pub fn new_cell<T>(value: T) -> Cell<T> {
    Rc::new(RefCell::new(value))
}

pub trait Show: fmt::Display + fmt::Debug {}

impl<T: fmt::Debug + fmt::Display> Show for T {}

pub type Result<T> = ::std::result::Result<T, Error>;

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
    Pair(Rc<Value>, Rc<Value>),
    /// Recursive lists of the form `forall a. rec t. () + (a, t)`
    ///
    /// Elements are stored in reverse order for optimal push, pop, and index semantics.
    List(Rc<Vec<Value>>, usize),
    /// Used to efficiently represent large pairs.
    ///
    /// Elements are stored in reverse order for consistency in index semantics.
    Record(Rc<Vec<Value>>, usize),
    /// `Left : forall a, b. a -> (a + b)`
    Left(Rc<Value>),
    /// `Right : n -> forall a[0..n], b. b -> (a[0] + (... + (a[n - 1] + b)))`
    Right(usize, Rc<Value>),
    /// `Function : forall a, b. (a -> b) -> Function`
    Function(Rc<dyn Fn(Value) -> Result<Value>>),
    /// `Thunk : forall a. (() -> a) -> a`
    Thunk(Rc<dyn Fn() -> Result<Value>>),
}

impl Value {
    /// Reduce a value down to a final result.
    pub fn reduce(&mut self) -> Result<Value> {
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
            Record(values, length) => write!(f, "Record({:?}, {:?})", values, length),
            List(values, length) => write!(f, "List({:?}, {:?})", values, length),
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
            Pair(l, r) => write!(f, "({}, {})", l, r),
            List(values, length) => {
                if *length > values.len() {
                    write!(f, "!invalid record!")
                } else {
                    write!(f, "[")?;
                    for index in (0..*length).skip(1).rev() {
                        write!(f, "{}, ", values[index])?;
                    }
                    if *length > 0 {
                        write!(f, "{}", values[0])?;
                    }
                    write!(f, "]")
                }
            }
            Record(_, 0) => write!(f, "()"),
            Record(values, length) => {
                if *length > values.len() {
                    write!(f, "!invalid record!")
                } else {
                    write!(
                        f,
                        "({}, {})",
                        values[length - 1],
                        Record(values.clone(), length - 1),
                    )
                }
            }
            Left(value) => write!(f, "(Left {})", value),
            Right(0, value) => write!(f, "{}", value),
            Right(n, value) => write!(f, "(Right {})", Right(n - 1, value.clone())),
            Function(_) => write!(f, "? -> ?"),
            Thunk(_) => write!(f, "?"),
        }
    }
}

impl Value {
    pub fn apply(&self, argument: Value) -> Result<Value> {
        if let Value::Function(function) = self {
            function(argument)
        } else {
            Err(Error::NotFunction(self.clone()))
        }
    }

    pub fn function(function: impl Fn(Value) -> Value + 'static) -> Value {
        Value::Function(Rc::new(move |arg| Ok(function(arg))))
    }

    pub fn function_try(function: impl Fn(Value) -> Result<Value> + 'static) -> Value {
        Value::Function(Rc::new(function))
    }

    pub fn thunk(thunk: impl Fn() -> Value + 'static) -> Value {
        Value::Thunk(Rc::new(move || Ok(thunk())))
    }

    pub fn thunk_try(thunk: impl Fn() -> Result<Value> + 'static) -> Value {
        Value::Thunk(Rc::new(thunk))
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Value {
        Value::Unit
    }
}

macro_rules! builtin_value {
    ($from:ty => $var:ident($to:ty) | $err:ident) => {
        impl From<$from> for Value {
            fn from(value: $from) -> Self {
                Value::$var(value as $to)
            }
        }

        impl TryInto<$from> for Value {
            type Error = Error;

            fn try_into(self) -> Result<$from> {
                if let Value::$var(value) = self {
                    Ok(value as $from)
                } else {
                    Err(Error::$err(self))
                }
            }
        }
    };
}

builtin_value!(i8 => Int(i64) | NotInt);
builtin_value!(i16 => Int(i64) | NotInt);
builtin_value!(i32 => Int(i64) | NotInt);
builtin_value!(i64 => Int(i64) | NotInt);
builtin_value!(u8 => UInt(u64) | NotUInt);
builtin_value!(u16 => UInt(u64) | NotUInt);
builtin_value!(u32 => UInt(u64) | NotUInt);
builtin_value!(u64 => UInt(u64) | NotUInt);
builtin_value!(f32 => Float(f64) | NotFloat);
builtin_value!(f64 => Float(f64)| NotFloat);
builtin_value!(bool => Bool(bool)| NotBool);
