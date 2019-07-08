use tiny_eval::*;
use std::fmt;
use std::rc::Rc;

type Result<T> = ::std::result::Result<T, Box<dyn ::std::error::Error>>;

#[derive(Debug)]
struct PowError;

impl fmt::Display for PowError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Could not take power")
    }
}

impl ::std::error::Error for PowError {}

fn main() -> Result<()> {
    let pow = function("pow", |base: Value<&'static str>| {
        Ok(Value::Function(Rc::new(move |exponent: Value<&'static str>| {
            match (base.clone(), exponent) {
                (Value::Int(mut base), Value::UInt(mut exponent)) => {
                    let mut power = 1;
                    while exponent != 0 {
                        if exponent % 2 == 0 {
                            base *= 2;
                            exponent /= 2;
                        } else {
                            power *= base;
                            exponent -= 1;
                        }
                    }
                    Ok(Value::Int(power))
                }
                (Value::UInt(mut base), Value::UInt(mut exponent)) => {
                    let mut power = 1;
                    while exponent != 0 {
                        if exponent % 2 == 0 {
                            base *= 2;
                            exponent /= 2;
                        } else {
                            power *= base;
                            exponent -= 1;
                        }
                    }
                    Ok(Value::UInt(power))
                }
                _ => Err(Error::External(Rc::new(PowError)))
            }
        })))
    });

    let expr: Expr<&'static str> = apply(apply(pow, value(17)), value(13u64));
    print!("{}", expr);
    println!(" = {}", eval(expr)?);

    Ok(())
}
