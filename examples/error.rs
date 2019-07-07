use tiny_eval::*;

type Result<T> = ::std::result::Result<T, Box<dyn ::std::error::Error>>;

fn main() -> Result<()> {
    let mut context = Context::new();

    let get_a = multiply(except(val(7), "a"), val(11));

    println!("context = {}", context);
    print!("{}", get_a);
    let result = context.eval(&get_a)?;
    println!(" = {}", result);
    match result {
        Value::Int(77) => {},
        result => panic!("{} != 77", result),
    }

    context.extend("a", val(5))?;

    println!("context = {}", context);
    print!("{}", get_a);
    let result = context.eval(&get_a)?;
    println!(" = {}", result);
    match result {
        Value::Int(55) => {},
        result => panic!("{} != 55", result),
    }

    context.extend("a", raise(val(12)))?;

    println!("context = {}", context);
    print!("{}", get_a);
    let result = context.eval(&get_a)?;
    println!(" = {}", result);
    match result {
        Value::Int(77) => {},
        result => panic!("{} != 77", result),
    }

    Ok(())
}
