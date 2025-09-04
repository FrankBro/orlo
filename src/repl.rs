use std::io::{self, BufRead, Write};

use crate::{env::Env, eval::eval, infer, parser::parse};

fn print(line: &str) {
    print!("{}", line);
    io::stdout().flush().unwrap();
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
                let ty = type_env.infer_value(&value).unwrap();
                let ty = type_env.ty_to_string(&ty).unwrap();
                match eval(&mut env, &value) {
                    Ok(value) => println!("{} :: {}", value, ty),
                    Err(e) => println!("Eval error: {}", e),
                }
            }
            Err(e) => println!("Parse error: {:?}", e),
        }
        print("orlo>>> ");
    }
}
