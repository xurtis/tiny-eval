mod data;
mod builtin;
mod expr;

pub type Result<T> = ::std::result::Result<T, ::failure::Error>;

pub use expr::construct::*;
pub use expr::{Expr, Context};
pub use builtin::*;


fn main() -> Result<()> {
    let mut context = Context::new();

    context.extend("a", val(6.2))?;
    context.extend("b", val(9.8))?;
    context.extend("plus", lambda("x", apply(Add, Expr::Variable("x"))))?;

    let expr = bind(
        "inc", apply(Add, val(1)),
        bind(
            "double", apply(Multiply, val(2)),
            apply("double", apply(apply("plus", apply("inc", "a")), "b"))
        ),
    );

    println!("Context: {}", context);
    println!("Expr: {}", expr);
    println!("Evaluated: {}", context.extend("result", expr)?);
    println!("Context: {}", context);

    Ok(())
}
