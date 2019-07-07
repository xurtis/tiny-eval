use tiny_eval::*;

type Result<T> = ::std::result::Result<T, Box<dyn ::std::error::Error>>;

fn main() -> Result<()> {
    apply_expr!(index(i, xs));

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
            pair(value(1), pair(value(1), apply(apply("fib", value(1)), value(1)))),
        ),
        bind(
            "index", lambda("i", lambda("xs",
                condition(
                    equal("i", value(0)),
                    first("xs"),
                    index(subtract("i", value(1)), second("xs")),
                )
            )),
            pair(
                index(value(0), "fib"),
                pair(
                    index(value(1), "fib"),
                    pair(
                        index(value(2), "fib"),
                        pair(
                            index(value(3), "fib"),
                            pair(
                                index(value(4), "fib"),
                                pair(
                                    index(value(5), "fib"),
                                    value(()),
                                ),
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
