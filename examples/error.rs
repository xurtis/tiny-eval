use tiny_eval::*;

type Result<T> = ::std::result::Result<T, Box<dyn ::std::error::Error>>;

fn main() -> Result<()> {
    let mut context = Context::new();

    let get_a = multiply(except(value(7), catch(lambda("x", divide("x", value(4))), "a")), value(11));

    println!("context = {}", context);
    print!("{}", get_a);
    let result = context.eval(&get_a)?;
    println!(" = {}", result);
    match result {
        Value::Int(77) => {},
        result => panic!("{} != 77", result),
    }

    context.extend("a", value(5))?;

    println!("context = {}", context);
    print!("{}", get_a);
    let result = context.eval(&get_a)?;
    println!(" = {}", result);
    match result {
        Value::Int(55) => {},
        result => panic!("{} != 55", result),
    }

    context.extend("a", raise(value(12)))?;

    println!("context = {}", context);
    print!("{}", get_a);
    let result = context.eval(&get_a)?;
    println!(" = {}", result);
    match result {
        Value::Int(33) => {},
        result => panic!("{} != 33", result),
    }

    Ok(())
}
