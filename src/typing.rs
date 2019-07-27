//! Type information and checking

#[derive(Debug, Clone)]
pub enum Type {
    Inferred,
    Numeric,
    Integer,
    Unit,
    Bool,
    Int,
    UInt,
    Float,
    Product(Box<Type>, Box<Type>),
    Sum(Box<Type>, Box<Type>),
    Function(Box<Type>, Box<Type>),
    Recursive(Box<Type>),
    Polymorphic(Box<Type>),
    Variable(i64),
}

/// Types for which numeric operations are valid
#[derive(Debug, Clone)]
pub enum Numeric {
    Inferred,
    Int,
    Uint,
    Float,
}

/// Types for which bitwise operations are valid
#[derive(Debug, Clone)]
pub enum Integer {
    Inferred,
    Int,
    UInt,
}
