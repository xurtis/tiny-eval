use tiny_eval::*;

type Expr = tiny_eval::Expr<&'static str>;

fn main() -> Result<()> {
    fn index(i: impl Into<Expr>, xs: impl Into<Expr>) -> Expr {
        apply(apply("index", i), xs)
    };

    // let
    //   fib := let
    //     fib := a -> b -> let
    //       next := (+ a b)
    //       in
    //         (pair next (fib b next))
    //     in
    //       (pair 1 (pair 1 (fib 1 1)))
    //   in let
    //     index := i -> xs ->
    //       if (= i 0)
    //         (first xs)
    //         (index (- i 1) (second xs))
    //   in
    //     (pair
    //       (index 0 fib)
    //       (pair
    //         (index 1 fib)
    //         (pair
    //           (index 2 fib)
    //           (pair
    //             (index 3 fib)
    //             (pair
    //               (index 4 fib)
    //               (index 5 fib)
    //             )
    //           )
    //         )
    //       )
    //     )
    // = (1 * 1 * 2 * 3 * 5 * 8)

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
