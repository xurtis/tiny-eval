use tiny_eval::*;

fn main() -> Result<()> {
    let mut context = Context::new();

    context.extend("a", val(6.2))?;
    context.extend("b", val(9.8))?;
    context.extend("plus", lambda("x", lambda("y", add("x", "y"))))?;

    apply_expr!(plus(a, b));
    apply_expr!(inc(a));
    apply_expr!(double(a));

    let expr = bind(
        "inc", apply(Builtin::Add, val(1)),
        bind(
            "double", apply(Builtin::Multiply, val(2)),
            double(plus(inc("a"), "b")),
        ),
    );

    println!("Context: {}", context);
    println!("Expr: {}", expr);
    println!("Evaluated: {}", context.extend("result", expr)?);
    println!("Context: {}", context);

    Ok(())
}
