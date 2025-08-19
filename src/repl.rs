use std::io::{self, BufRead, Write};

use crate::{env::Env, eval::eval, parser::parse_expr};

fn print(line: &str) {
    print!("{}", line);
    io::stdout().flush().unwrap();
}

pub fn run() {
    let mut env = Env::primitive_bindings();
    print("Lisp>>> ");
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let input = line.trim_end();
        if input == "quit" {
            return;
        }
        match parse_expr(input) {
            Ok(value) => match eval(&mut env, &value) {
                Ok(value) => println!("{}", value),
                Err(e) => println!("Eval error: {}", e),
            },
            Err(e) => println!("Parse error: {}", e),
        }
        print("Lisp>>> ");
    }
}
