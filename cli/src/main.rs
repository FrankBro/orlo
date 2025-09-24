use std::io::{self, BufRead, Write};

use orlo::{env::Env, eval::eval, parser::parse, repl::Repl};

fn run_arg(arg: &str) {
    match parse(arg) {
        Ok(value) => {
            let mut env = Env::primitive_bindings();
            match eval(&mut env, &value) {
                Ok(value) => println!("{}", value),
                Err(e) => println!("Eval error: {}", e),
            }
        }
        Err(e) => println!("Parse error: {:?}", e),
    }
}

fn print(line: &str) {
    print!("{}", line);
    io::stdout().flush().unwrap();
}

fn run() {
    let mut repl = Repl::default();
    print("orlo>>> ");
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let input = line.trim_end();
        if input == "quit" {
            return;
        }
        match repl.handle_input(input) {
            Ok(the) => println!("{}", the),
            Err(e) => {
                match e {
                    orlo::repl::Error::Parse => {
                        println!("Parse error for {}", input);
                    }
                    orlo::repl::Error::Infer(e) => {
                        println!("Type error: {:?} for {}", e, input);
                    }
                    orlo::repl::Error::Eval(e) => {
                        println!("Eval error: {}", e);
                    }
                }
                print("orlo>>> ");
                continue;
            }
        };
        print("orlo>>> ");
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match &args[..] {
        [_program] => run(),
        [_program, arg] => run_arg(arg),
        _ => println!("Pass no argument for repl, one argument for eval"),
    }
}
