use env::Env;
use eval::eval;
use parser::parse_expr;
use repl::run;

mod env;
mod error;
mod eval;
mod lexer;
mod parser;
mod primitive;
mod repl;
mod util;
mod value;

fn run_arg(arg: &str) {
    match parse_expr(arg) {
        Ok(value) => {
            let mut env = Env::primitive_bindings();
            match eval(&mut env, &value) {
                Ok(value) => println!("{}", value),
                Err(e) => println!("Eval error: {}", e),
            }
        }
        Err(e) => println!("Parse error: {}", e),
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
