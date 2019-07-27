//! Lamba expressions and evaluation
use crate::data_new::{Cell, Value};
use crate::typing::{Integer, Numeric};

use std::rc::Rc;
use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::fmt;

use failure::{Error, format_err};

/// Errors triggered by invalid operations
#[derive(Debug, Clone)]
enum Fault {
    Raise(Value),
}

impl fmt::Display for Fault {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
    }
}

impl std::error::Error for Fault {}

/// Result of partial or full evaluation
type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Operator {
    /// `Roll : T (rec t. T t) -> rec t. T t`
    Roll,
    /// `Unroll : rec t. T t -> T (rec t. T t)`
    Unroll,
    /// `Raise : forall a, b. a -> b`
    Raise,
    /// `Except : forall a. (forall b. b -> a) -> ExceptWith a`
    Except,
    /// `ExceptWith a : a -> a`
    ExceptWith(Value),
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
    /// `Case : forall a, c. (a -> c) -> LeftCase a c`
    Case,
    /// `LeftCase a c : forall b. (b -> c) -> CaseOf a b c`
    LeftCase(Value),
    /// `CaseOf a b c : (a + b) -> c`
    CaseOf(Value, Value),
    /// `SkipLeft n : forall c. (forall a. a -> c) -> SkipLeftWith n c`
    ///
    /// Allows for quickly matching deeply nested sum values to match a single specific case by
    /// discarding effectively performing n stacked matches on the right.
    SkipLeft(u64),
    /// `SkipLeft n c : forall b. (b -> c) -> SkipLeftMatch n b c`
    SkipLeftWith(u64, Value),
    /// `SkipLeft n b c: forall a[0..n]. (a[0] + (... + (a[n - 1] + b))) -> c`
    SkipLeftMatch(u64, Value, Value),
    /// `Not : Bool -> Bool`
    Not,
    /// `And : Bool -> AndWith`
    And,
    /// `AndWith : Bool -> Bool`
    AndWith(Value),
    /// `Or : Bool -> OrWith`
    Or,
    /// `OrWith : Bool -> Bool`
    OrWith(Value),
    /// `Greater : Numeric n => n -> GreaterThan n`
    Greater(Numeric),
    /// `GreaterThan n : n -> Bool`
    GreaterThan(Value),
    /// `GreaterEqual : Numeric n => n -> n -> Bool`
    GreaterOrEqual(Numeric),
    /// `GreaterThanEqualTo n : n -> Bool`
    GreaterThanOrEqualTo(Value),
    /// `Less : Numeric n => n -> n -> Bool`
    Less(Numeric),
    /// `LessThan n : n -> Bool`
    LessThan(Value),
    /// `LessEqual : Numeric n => n -> n -> Bool`
    LessOrEqual(Numeric),
    /// `LessThanEqualTo n : n -> Bool`
    LessThanOrEqualTo(Value),
    /// `Equal : Numeric n => n -> EqualTo n`
    Equal(Numeric),
    /// `EqualTo n : n -> Bool`
    EqualTo(Value),
    /// `NotEqual : Numeric n => n -> NotEqualTo`
    NotEqual(Numeric),
    /// `NotEqualTo n : n -> Bool`
    NotEqualTo(Value),
    /// `Add : Numeric n => n -> AddTo n`
    Add(Numeric),
    /// `AddTo n : n -> n`
    AddTo(Value),
    /// `Subtract : Numeric n => n -> SubtractFrom n`
    Subtract(Numeric),
    /// `SubtractFrom n : n -> n`
    SubtractFrom(Value),
    /// `Multiply : Numeric n => n -> MultiplyWith n`
    Multiply(Numeric),
    /// `MultiplyWith n : n -> n`
    MultiplyWith(Value),
    /// `Divide : Numeric n => n -> DivideWith n`
    Divide(Numeric),
    /// `DivideWith n : n -> n`
    DivideWith(Value),
    /// `Remainder : Numeric n => n -> RemainderWith n`
    Remainder(Numeric),
    /// `RemainderWith n : n -> n`
    RemainderWith(Value),
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

impl Operator {
    fn apply(&self, operand: Value) -> Result {
        unimplemented!()
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    /// Lambda variable binding
    Lambda(Cell<Expr>),
    /// Application of a lambda
    Apply(Cell<Expr>, Cell<Expr>),
    /// De Bruijin indexed variable reference
    Variable(u64),
    /// Builtin operator
    Operator(Operator),
    /// Builtin value
    Value(Value),
}

impl Expr {
    /// Strict evaluation of the lambda expression to a value.
    pub fn eval(self) -> Result<Value> {
        unimplemented!()
    }

    pub fn reduce(&mut self) -> Result<Value> {
        unimplemented!()
    }

    fn substitute(&mut self, expr: Cell<Expr>, index: i64) {
        unimplemented!()
    }

    fn remove_dead_code(&mut self) {
        unimplemented!()
    }
}
