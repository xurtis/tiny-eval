//! Type information and checking

#[derive(Debug, Clone)]
pub enum Type {
    Inferred,
    Unit,
    Bool,
    Int,
    UInt,
    Float,
    Product(Box<Type>, Box<Type>),
    Sum(Box<Type>, Box<Type>),
    Function(Box<Type>, Box<Type>),
    Recursive(String, Box<Type>),
    Polymorphic(String, Box<Type>),
    Variable(String),
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

/// Type passed to error handlers (could be any lower-level type).
pub fn meta_type() -> Type {
    use Type::*;

    let meta = Box::new(Variable("M".to_owned()));

    fn sum(a: Type, b: Type) -> Type {
        Sum(Box::new(a), Box::new(b))
    }

    Recursive(
        "M".to_owned(),
        Box::new(sum(
            Unit,
            sum(
                Bool,
                sum(
                    Int,
                    sum(
                        UInt,
                        sum(
                            Float,
                            sum(
                                Product(
                                    meta.clone(),
                                    meta.clone(),
                                ),
                                sum(
                                    Sum(
                                        meta.clone(),
                                        meta.clone(),
                                    ),
                                    Function(
                                        meta.clone(),
                                        meta.clone(),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        )),
    )
}
