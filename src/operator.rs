//! Operators and builtin constructors

use crate::data_new::{Value};
use crate::lambda::Error;
use crate::typing::{Integer, Numeric, Binary};

use std::fmt;
use std::rc::Rc;

type Result<T> = ::std::result::Result<T, Error>;

macro_rules! numeric {
    ($hof:ident($impl:expr)) => {
        crate::operator::higher_order::Numeric::$hof($impl, $impl, $impl)
    };
}

macro_rules! integer {
    ($hof:ident($impl:expr)) => {
        crate::operator::higher_order::Integer::$hof($impl, $impl)
    };
}

macro_rules! binary {
    ($hof:ident($impl:expr)) => {
        crate::operator::higher_order::Binary::$hof($impl, $impl, $impl)
    };
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    /// `Condition : forall a. Bool -> a -> a -> a`
    Condition,
    /// `Roll : T (rec t. T t) -> rec t. T t`
    Roll,
    /// `Unroll : rec t. T t -> T (rec t. T t)`
    Unroll,
    /// `Fold : forall f, a. (forall a, b. (a -> b) -> f a -> f b) -> (f a -> a) -> rec t. f t -> a`
    Fold,
    /// `Recurse : forall a. (a -> a) -> a`
    ///
    /// Behold; the Y-combinator, in all it's glory.
    Recurse,
    /// `Raise : forall a, b. a -> b`
    Raise,
    /// `Pair : forall a, b. a -> b -> (a, b)`
    Pair,
    /// `First : forall a, b. (a, b) -> a`
    First,
    /// `Second : forall a, b. (a, b) -> b`
    Second,
    /// `Empty : forall a. rec t. () + (t, a)`
    Empty,
    /// `Element n : forall a[0..n], b. (a[0], (..., (a[n - 1], b))) -> b`
    ///
    /// Allows for quickly selecting an element of a deeply nested pair by effectively performing n
    /// stacked selects on the right.
    Element(usize),
    /// `Index : UInt -> forall a. rec t. () + (a, t) -> () + a`
    Index,
    /// `Left : forall a, b. a -> a + b`
    Left,
    /// `Right : forall a, b. b -> a + b`
    Right,
    /// DeepRight is a deeply nested right constructor
    DeepRight(usize),
    /// `Case : forall a, b, c. (a -> c) -> (b -> c) -> (a + b) -> c`
    Case,
    /// `SkipLeft n : forall a[0..n], b, c. c -> (b -> c) -> (a[0] + (... + (a[n - 1] + b))) -> c`
    ///
    /// Allows for quickly matching deeply nested sum values to match a single specific case by
    /// discarding effectively performing n stacked matches on the right.
    SkipLeft(usize),
    /// `Not : Binary b => b -> b`
    Not(Binary),
    /// `And : Binary b => b -> b -> b`
    And(Binary),
    /// `Or : Binary b => b -> b -> b`
    Or(Binary),
    /// `ExclusiveOr : Binary b => b -> b -> b`
    ExclusiveOr(Binary),
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
    /// `ShiftLeft : Integer i => UInt -> i -> i`
    ShiftLeft(Integer),
    /// `ShiftRight : Integer i => UInt -> i -> i`
    ShiftRight(Integer),
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
    Round,
}

impl Operator {
    fn value(&self) -> Value {
        use Operator::*;
        use higher_order::*;

        match self {
            Condition => ternary(Self::condition),
            Roll => unary(Self::id),
            Unroll => unary(Self::id),
            Fold => ternary(Self::fold),
            Recurse => unary_try(Self::recurse),
            Raise => unary_try(Self::raise),
            Pair => binary(Self::pair),
            First => unary_try(Self::first),
            Second => unary_try(Self::second),
            Empty => Value::List(Rc::new(Vec::new()), 0),
            Element(n) => Self::element(*n),
            Index => binary_try(Self::index),
            Left => unary(Self::left),
            Right => unary(Self::right),
            DeepRight(n) => Self::deep_right(*n),
            Case => ternary_try(Self::case),
            SkipLeft(n) => Self::skip_left(*n),
            Not(_) => binary!(unary(|a| !a)),
            And(_) => binary!(binary(|a, b| a & b)),
            Or(_) => binary!(binary(|a, b| a | b)),
            ExclusiveOr(_) => binary!(binary(|a, b| a ^ b)),
            Greater(_) => numeric!(binary(|a, b| a > b)),
            GreaterOrEqual(_) => numeric!(binary(|a, b| a >= b)),
            Less(_) => numeric!(binary(|a, b| a < b)),
            LessOrEqual(_) => numeric!(binary(|a, b| a <= b)),
            Equal(_) => numeric!(binary(|a, b| a == b)),
            NotEqual(_) => numeric!(binary(|a, b| a != b)),
            Add(_) => numeric!(binary(|a, b| a + b)),
            Subtract(_) => numeric!(binary(|a, b| a - b)),
            Multiply(_) => numeric!(binary(|a, b| a * b)),
            Divide(_) => numeric!(binary(|a, b| a / b)),
            Remainder(_) => numeric!(binary(|a, b| a % b)),
            ShiftLeft(_) => integer!(shift(|a, b| b << a)),
            ShiftRight(_) => integer!(shift(|a, b| b >> a)),
            Signed(_) => integer!(cast(|a| a as i64)),
            Unsigned(_) => integer!(cast(|a| a as u64)),
            Float(_) => integer!(cast(|a| a as f64)),
            Ceiling => unary(|a: f64| a.ceil()),
            Floor => unary(|a: f64| a.floor()),
            Round => unary(|a: f64| a.round()),
        }
    }

    /// Conditional branching
    fn condition(condition: bool, left: Value, right: Value) -> Value {
        if condition { left } else { right }
    }

    /// Identity operator
    fn id(value: Value) -> Value {
        value
    }

    /// Catamorphic fold
    fn fold(fmap: Value, algebra: Value, recursive: Value) -> Value {
        unimplemented!()
    }

    /// Y-combinator recursion
    fn recurse(recursive: Value) -> Result<Value> {
        let inner = recursive.clone();
        let thunk = move || {
            let inner = inner.clone();
            Self::recurse(inner)
        };
        let thunk = Value::thunk_try(thunk);
        recursive.apply(thunk)
    }

    /// Raise an error
    fn raise(value: Value) -> Result<Value> {
        Err(Error::Raise(value))
    }

    /// Construct a pair
    fn pair(first: Value, second: Value) -> Value {
        match second {
            Value::Unit => {
                let record = vec![first];
                Value::List(Rc::new(record), 1)
            }
            Value::Record(mut record, depth) => {
                Rc::make_mut(&mut record).push(first);
                Value::List(record, depth + 1)
            }
            second => {
                let second = Rc::new(second);
                let first = Rc::new(first);
                Value::Pair(first, second)
            }
        }
    }

    /// First element of a pair
    fn first(value: Value) -> Result<Value> {
        match value {
            Value::Pair(first, _) => Ok(first.take()),
            // Unit representation with a record
            value @ Value::Record(_, 0) => Err(Error::NotPair(value)),
            Value::Record(record, depth) => Ok(record[depth - 1].clone()),
            value => Err(Error::NotPair(value))
        }
    }

    /// Second element of a pair
    fn second(value: Value) -> Result<Value> {
        match value {
            Value::Pair(_, second) => Ok(second.take()),
            // Unit representation with a record
            value @ Value::Record(_, 0) => Err(Error::NotPair(value)),
            Value::Record(record, depth) => Ok(Value::Record(record, depth - 1)),
            value => Err(Error::NotPair(value))
        }
    }

    /// Get an element from a large pair
    fn element(index: usize) -> Value {
        if index == 0 {
            higher_order::unary(Self::id)
        } else {
            let operator = move |value: Value| -> Result<Value> {
                match value {
                    Value::Pair(_, right) => {
                        let thunk = move || {
                            let right = right.clone();
                            Self::element(index - 1).apply(right.take())
                        };
                        Ok(Value::thunk_try(thunk))
                    }
                    Value::Record(record, length) => {
                        if index > length {
                            Err(Error::NotPair(Value::Record(record, 0)))
                        } else {
                            Ok(Value::Record(record, length - index))
                        }
                    }
                    value => Err(Error::NotPair(value)),
                }
            };
            higher_order::unary_try(operator)
        }
    }

    /// Get an item from a list at some index
    fn index(index: Value, list: Value) -> Result<Value> {
        unimplemented!()
    }

    /// Construct a left value of a sum
    fn left(value: Value) -> Value {
        Value::Left(Rc::new(value))
    }

    /// Construct a right value of a sum
    fn right(value: Value) -> Value {
        match value {
            Value::Right(depth, value) => Value::Right(depth + 1, value),
            value => Value::Right(1, Rc::new(value))
        }
    }

    /// Construct a deeply nested right value of a sim
    fn deep_right(depth: usize) -> Value {
        let constructor = move |value: Value| {
            Value::Right(depth, Rc::new(value))
        };
        higher_order::unary(constructor)
    }

    /// Match on a sum
    fn case(left: Value, right: Value, value: Value) -> Result<Value> {
        match value {
            Value::Left(value) => {
                left.apply(value.take())
            }
            Value::Right(0, value) => {
                let thunk = move || {
                    let left = left.clone();
                    let right = right.clone();
                    let value = value.clone();
                    Self::case(left, right, value.take())
                };
                Ok(Value::thunk_try(thunk))
            }
            Value::Right(depth, value) => {
                right.apply(Value::Right(depth - 1, value))
            }
            value => Err(Error::NotSum(value)),
        }
    }

    /// Match a particular case of a deeply nested sum
    fn skip_left(count: usize) -> Value {
        if count == 0 {
            let operator = move |_default: Value, _case: Value, sum: Value| sum;
            higher_order::ternary(operator)
        } else {
            let operator = move |default: Value, case: Value, sum: Value| {
                match sum {
                    Value::Left(_) => Ok(default),
                    Value::Right(depth, value) => {
                        if count > depth {
                            let thunk = move || {
                                let value = value.clone();
                                Self::skip_left(count - depth).apply(value.take())
                            };
                            Ok(Value::thunk_try(thunk))
                        } else {
                            case.apply(Value::Right(depth - count, value))
                        }
                    }
                    value => Err(Error::NotSum(value)),
                }
            };
            higher_order::ternary_try(operator)
        }
    }
}

impl Into<Value> for Operator {
    fn into(self) -> Value {
        self.value()
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}

trait RcExt<T> {
    fn take(self) -> T;
}

impl<T: Clone> RcExt<T> for Rc<T> {
    fn take(self) -> T {
        Rc::try_unwrap(self).unwrap_or_else(|value| (*value).clone())
    }
}

mod higher_order {
    use crate::data_new::Value;
    use crate::lambda::Error;
    use crate::lambda::Error::*;
    use super::Result;

    use std::rc::Rc;
    use std::convert::{TryFrom, TryInto};

    pub(super) fn unary<F, A, AE, R>(f: F) -> Value
    where
        F: Fn(A) -> R,
        F: Sized + 'static,
        Value: TryInto<A, Error = AE>,
        Error: From<AE>,
        R: Into<Value>,
    {
        let unary = move |a: Value| Ok(f(a.try_into()?).into());
        Value::function_try(unary)
    }

    pub(super) fn unary_try<F, A, AE, R>(f: F) -> Value
    where
        F: Fn(A) -> Result<R>,
        F: Sized + 'static,
        Value: TryInto<A, Error = AE>,
        Error: From<AE>,
        R: Into<Value>,
    {
        let unary = move |a: Value| Ok(f(a.try_into()?)?.into());
        Value::function_try(unary)
    }

    pub(super) fn binary<F, A, AE, B, BE, R>(f: F) -> Value
    where
        F: Fn(A, B) -> R,
        F: Sized + 'static,
        Value: TryInto<A, Error = AE>,
        Error: From<AE>,
        A: Clone + 'static,
        Value: TryInto<B, Error = BE>,
        Error: From<BE>,
        R: Into<Value>,
    {
        let f = Rc::new(f);
        let binary = move |a: Value| {
            let f = f.clone();
            let unary = move |b: Value| {
                let a = a.clone();
                Ok(f(a.try_into()?, b.try_into()?).into())
            };
            Ok(Value::function_try(unary))
        };
        Value::function_try(binary)
    }

    pub(super) fn binary_try<F, A, AE, B, BE, R>(f: F) -> Value
    where
        F: Fn(A, B) -> Result<R>,
        F: Sized + 'static,
        Value: TryInto<A, Error = AE>,
        Error: From<AE>,
        A: Clone + 'static,
        Value: TryInto<B, Error = BE>,
        Error: From<BE>,
        R: Into<Value>,
    {
        let f = Rc::new(f);
        let binary = move |a: Value| {
            let f = f.clone();
            let unary = move |b: Value| {
                let a = a.clone();
                Ok(f(a.try_into()?, b.try_into()?)?.into())
            };
            Ok(Value::function_try(unary))
        };
        Value::function_try(binary)
    }

    pub(super) fn ternary<F, A, AE, B, BE, C, CE, R>(f: F) -> Value
    where
        F: Fn(A, B, C) -> R,
        F: Sized + 'static,
        Value: TryInto<A, Error = AE>,
        Error: From<AE>,
        A: Clone + 'static,
        Value: TryInto<B, Error = BE>,
        Error: From<BE>,
        B: Clone + 'static,
        Value: TryInto<C, Error = CE>,
        Error: From<CE>,
        R: Into<Value>,
    {
        let f = Rc::new(f);
        let ternary = move |a: Value| {
            let f = f.clone();
            let binary = move |b: Value| {
                let a = a.clone();
                let f = f.clone();
                let unary = move |c: Value| {
                    let a = a.clone();
                    let b = b.clone();
                    Ok(f(a.try_into()?, b.try_into()?, c.try_into()?).into())
                };
                Ok(Value::function_try(unary))
            };
            Ok(Value::function_try(binary))
        };
        Value::function_try(ternary)
    }

    pub(super) fn ternary_try<F, A, AE, B, BE, C, CE, R>(f: F) -> Value
    where
        F: Fn(A, B, C) -> Result<R>,
        F: Sized + 'static,
        Value: TryInto<A, Error = AE>,
        Error: From<AE>,
        A: Clone + 'static,
        Value: TryInto<B, Error = BE>,
        Error: From<BE>,
        B: Clone + 'static,
        Value: TryInto<C, Error = CE>,
        Error: From<CE>,
        R: Into<Value>,
    {
        let f = Rc::new(f);
        let ternary = move |a: Value| {
            let f = f.clone();
            let binary = move |b: Value| {
                let a = a.clone();
                let f = f.clone();
                let unary = move |c: Value| {
                    let a = a.clone();
                    let b = b.clone();
                    Ok(f(a.try_into()?, b.try_into()?, c.try_into()?)?.into())
                };
                Ok(Value::function_try(unary))
            };
            Ok(Value::function_try(binary))
        };
        Value::function_try(ternary)
    }

    #[derive(Debug, Clone, Copy)]
    pub(super) enum Numeric {
        Int(i64),
        UInt(u64),
        Float(f64),
    }

    impl TryFrom<Value> for Numeric {
        type Error = Error;

        fn try_from(mut value: Value) -> Result<Self> {
            match value.reduce()? {
                Value::Int(i) => Ok(Numeric::Int(i)),
                Value::UInt(u) => Ok(Numeric::UInt(u)),
                Value::Float(f) => Ok(Numeric::Float(f)),
                value => Err(NotNumeric(value)),
            }
        }
    }

    impl Into<Value> for Numeric {
        fn into(self) -> Value {
            match self {
                Numeric::Int(i) => Value::Int(i),
                Numeric::UInt(u) => Value::UInt(u),
                Numeric::Float(f) => Value::Float(f),
            }
        }
    }

    impl Numeric {
        pub(crate) fn binary<N: Into<Value>, U: Into<Value>, F: Into<Value>>(
            int: impl Fn(i64, i64) -> N + 'static,
            uint: impl Fn(u64, u64) -> U + 'static,
            float: impl Fn(f64, f64) -> F + 'static,
        ) -> Value {
            use Numeric::*;

            let cast = move |a: Numeric, b: Numeric| {
                match (a, b) {
                    (UInt(a), UInt(b)) => Ok(uint(a, b).into()),
                    (Int(a), Int(b)) => Ok(int(a, b).into()),
                    (Float(a), Float(b)) => Ok(float(a, b).into()),
                    (UInt(_), b) => Err(NotUInt(b.into())),
                    (Int(_), b) => Err(NotInt(b.into())),
                    (Float(_), b) => Err(NotFloat(b.into())),
                }
            };

            binary_try(cast)
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub(super) enum Integer {
        Int(i64),
        UInt(u64),
    }

    impl TryFrom<Value> for Integer {
        type Error = Error;

        fn try_from(mut value: Value) -> Result<Self> {
            match value.reduce()? {
                Value::Int(i) => Ok(Integer::Int(i)),
                Value::UInt(u) => Ok(Integer::UInt(u)),
                value => Err(NotInteger(value)),
            }
        }
    }

    impl Into<Value> for Integer {
        fn into(self) -> Value {
            match self {
                Integer::Int(i) => Value::Int(i),
                Integer::UInt(u) => Value::UInt(u),
            }
        }
    }

    impl Integer {
        pub(crate) fn cast<T>(
            int: impl Fn(i64) -> T + 'static,
            uint: impl Fn(u64) -> T + 'static,
        ) -> Value
        where
            T: Into<Value>,
        {
            use Integer::*;

            let cast = move |a: Integer| {
                match a {
                    UInt(a) => uint(a).into(),
                    Int(a) => int(a).into(),
                }
            };

            unary(cast)
        }

        pub(crate) fn shift<N: Into<Value>, U: Into<Value>>(
            int: impl Fn(usize, i64) -> N + 'static,
            uint: impl Fn(usize, u64) -> U + 'static,
        ) -> Value {
            use Integer::*;

            let cast = move |a: u64, b: Integer| {
                match (a, b) {
                    (shift, UInt(a)) => uint(shift as usize, a).into(),
                    (shift, Int(a)) => int(shift as usize, a).into(),
                }
            };

            binary(cast)
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub(super) enum Binary {
        Int(i64),
        UInt(u64),
        Bool(bool),
    }

    impl TryFrom<Value> for Binary {
        type Error = Error;

        fn try_from(mut value: Value) -> Result<Self> {
            match value.reduce()? {
                Value::Int(i) => Ok(Binary::Int(i)),
                Value::UInt(u) => Ok(Binary::UInt(u)),
                Value::Bool(b) => Ok(Binary::Bool(b)),
                value => Err(NotBinary(value)),
            }
        }
    }

    impl Into<Value> for Binary {
        fn into(self) -> Value {
            match self {
                Binary::Int(i) => Value::Int(i),
                Binary::UInt(u) => Value::UInt(u),
                Binary::Bool(b) => Value::Bool(b),
            }
        }
    }

    impl Binary {
        pub(crate) fn unary<N: Into<Value>, U: Into<Value>, B: Into<Value>>(
            int: impl Fn(i64) -> N + 'static,
            uint: impl Fn(u64) -> U + 'static,
            boolean: impl Fn(bool) -> B + 'static,
        ) -> Value {
            use Binary::*;

            let cast = move |a: Binary| {
                match a {
                    UInt(a) => uint(a).into(),
                    Int(a) => int(a).into(),
                    Bool(a) => boolean(a).into(),
                }
            };

            unary(cast)
        }

        pub(crate) fn binary<N: Into<Value>, U: Into<Value>, B: Into<Value>>(
            int: impl Fn(i64, i64) -> N + 'static,
            uint: impl Fn(u64, u64) -> U + 'static,
            boolean: impl Fn(bool, bool) -> B + 'static,
        ) -> Value {
            use Binary::*;

            let cast = move |a: Binary, b: Binary| {
                match (a, b) {
                    (UInt(a), UInt(b)) => Ok(uint(a, b).into()),
                    (Int(a), Int(b)) => Ok(int(a, b).into()),
                    (Bool(a), Bool(b)) => Ok(boolean(a, b).into()),
                    (UInt(_), b) => Err(NotUInt(b.into())),
                    (Int(_), b) => Err(NotInt(b.into())),
                    (Bool(_), b) => Err(NotBool(b.into())),
                }
            };

            binary_try(cast)
        }
    }
}
