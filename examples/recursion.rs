use tiny_eval::*;

type Result<T> = ::std::result::Result<T, Box<dyn ::std::error::Error>>;

fn main() -> Result<()> {
    apply_expr!(take(n, xs));

    // let fib :=
    //   let fib := a -> b ->
    // 	let next := (+ a b)
    // 	  in (pair next (fib b next))
    //     in (pair 1 (pair 1 (fib 1 1)))
    //   in let take := count -> list ->
    //     if (= count 0)
    // 	  ()
    // 	  (pair (first list) (take (- count 1) (second list)))
    //     in (take 35 fib)

    let expr = bind(
        "fib", bind(
            "fib", lambda("a", lambda("b", bind(
                "next", add("a", "b"),
                pair("next", apply(apply("fib", "b"), "next")),
            ))),
            pair(value(1), pair(value(1), apply(apply("fib", value(1)), value(1)))),
        ),
        bind(
            "take", lambda("count", lambda("list",
                condition(
                    equal("count", value(0)),
                    value(()),
                    pair(
                        first("list"),
                        take(subtract("count", value(1)), second("list")),
                    ),
                )
            )),
            take(value(35), "fib"),
        )
    );

    print!("{}", expr);
    println!(" = {}", eval(expr)?.finalise_recursive()?);
    Ok(())
}
