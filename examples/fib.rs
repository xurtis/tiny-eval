use tiny_eval::*;

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

fn main() -> Result<()> {
    let fib = bind(
        "fib", lambda("a", lambda("b", bind(
            "next", add("a", "b"),
            pair("next", apply(apply("fib", "b"), "next")),
        ))),
        pair(val(1), pair(val(1), apply(apply("fib", val(1)), val(1)))),
    );
    let mut stdout = FlushFormatter::new();

    write!(stdout, "{}", fib)?;
    writeln!(stdout, "= {}", eval(fib)?)?;

    Ok(())
}
