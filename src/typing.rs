//! Type information and checking

#[derive(Debug, Clone)]
pub enum Type {
    Inferred,
    Numeric,
    Integer,
    Binary,
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
#[derive(Debug, Clone, Copy)]
pub enum Numeric {
    Inferred,
    Int,
    Uint,
    Float,
}

/// Types for which bitwise operations are valid
#[derive(Debug, Clone, Copy)]
pub enum Integer {
    Inferred,
    Int,
    UInt,
}

/// Types for which bitwise operators are valid
#[derive(Debug, Clone, Copy)]
pub enum Binary {
    Inferred,
    Int,
    UInt,
    Bool,
}
