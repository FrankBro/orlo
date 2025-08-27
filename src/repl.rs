use std::io::{self, BufRead, Write};

use crate::{env::Env, eval::eval, infer, parser::parse};

fn print(line: &str) {
    print!("{}", line);
    io::stdout().flush().unwrap();
}

pub fn run() {
    let mut env = Env::primitive_bindings();
    print("Lisp>>> ");
    let stdin = io::stdin();
    let mut type_env = infer::Env::default();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let input = line.trim_end();
        if input == "quit" {
            return;
        }
        match parse(input) {
            Ok(value) => {
                type_env.infer_value(&value).unwrap();
                match eval(&mut env, &value) {
                    Ok(value) => println!("{}", value),
                    Err(e) => println!("Eval error: {}", e),
                }
            }
            Err(e) => println!("Parse error: {:?}", e),
        }
        print("Lisp>>> ");
    }
}
