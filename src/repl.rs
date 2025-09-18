use std::io::{self, BufRead, Write};

use crate::{env::Env, eval::eval, infer, parser::parse, value::Value};

fn print(line: &str) {
    print!("{}", line);
    io::stdout().flush().unwrap();
}

fn get_type(env: &mut infer::Env, value: &Value) -> Result<String, infer::Error> {
    let ty = env.infer_value(value)?;
    env.ty_to_string(&ty)
}

pub fn run() {
    let mut env = Env::primitive_bindings();
    print("orlo>>> ");
    let stdin = io::stdin();
    let mut type_env = infer::Env::primitive_bindings();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let input = line.trim_end();
        if input == "quit" {
            return;
        }
        match parse(input) {
            Ok(value) => {
                let ty = match get_type(&mut type_env, &value) {
                    Ok(ty) => ty,
                    Err(e) => {
                        println!("Type error: {:?}", e);
                        String::from("error")
                    }
                };
                match eval(&mut env, &value) {
                    Ok(value) => println!("(the {} {})", ty, value),
                    Err(e) => println!("Eval error: {}", e),
                }
            }
            Err(e) => println!("Parse error: {:?}", e),
        }
        print("orlo>>> ");
    }
}
