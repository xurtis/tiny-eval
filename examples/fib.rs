use tiny_eval::*;

type Result<T> = ::std::result::Result<T, Box<dyn ::std::error::Error>>;

use std::io::{stdout, Stdout, Write};
use std::fmt::{Write as FmtWrite, self};

struct FlushFormatter(Stdout);

impl FlushFormatter {
    fn new() -> Self {
        FlushFormatter(stdout())
    }
}

impl FmtWrite for FlushFormatter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0.write_all(s.as_bytes()).ok();
        self.0.flush().ok();
        Ok(())
    }
}

apply_expr!(fib(a, b));

fn main() -> Result<()> {
    let fib = bind(
        "fib", lambda("a", lambda("b", bind(
            "next", add("a", "b"),
            pair("next", fib("b", "next")),
        ))),
        pair(value(1), pair(value(1), fib(value(1), value(1)))),
    );
    let mut stdout = FlushFormatter::new();

    write!(stdout, "{}", fib)?;
    writeln!(stdout, " = {}", eval(fib)?)?;

    Ok(())
}
