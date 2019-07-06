use tiny_eval::*;

type Expr = tiny_eval::Expr<&'static str>;

fn main() -> Result<()> {
    fn index(i: impl Into<Expr>, xs: impl Into<Expr>) -> Expr {
        apply(apply("index", i), xs)
    };
    let expr = bind(
        "fib", bind(
            "fib", lambda("a", lambda("b", bind(
                "next", add("a", "b"),
                pair("next", apply(apply("fib", "b"), "next")),
            ))),
            pair(val(1), pair(val(1), apply(apply("fib", val(1)), val(1)))),
        ),
        bind(
            "index", lambda("i", lambda("xs",
                condition(
                    equal("i", val(0)),
                    first("xs"),
                    index(subtract("i", val(1)), second("xs")),
                )
            )),
            pair(
                index(val(0), "fib"),
                pair(
                    index(val(1), "fib"),
                    pair(
                        index(val(2), "fib"),
                        pair(
                            index(val(3), "fib"),
                            pair(
                                index(val(4), "fib"),
                                index(val(5), "fib"),
                            ),
                        ),
                    ),
                ),
            )
        )
    );

    print!("{}", expr);
    println!(" = {}", eval(expr)?.finalise_recursive()?);
    Ok(())
}
