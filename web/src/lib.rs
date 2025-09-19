use std::sync::{Mutex, OnceLock};

use orlo::{env, eval::eval, infer, parser::parse, value::Value};
use wasm_bindgen::prelude::*;

struct Repl {
    infer: infer::Env,
    eval: env::Env,
}

impl Default for Repl {
    fn default() -> Self {
        Self {
            infer: infer::Env::primitive_bindings(),
            eval: env::Env::primitive_bindings(),
        }
    }
}

impl Repl {
    fn get_type(&mut self, value: &Value) -> Result<String, infer::Error> {
        let ty = self.infer.infer_value(value)?;
        self.infer.ty_to_string(&ty)
    }
}

static REPL: OnceLock<Mutex<Repl>> = OnceLock::new();

// pub fn run() {
//     let mut env = Env::primitive_bindings();
//     print("orlo>>> ");
//     let stdin = io::stdin();
//     let mut type_env = infer::Env::primitive_bindings();
//     for line in stdin.lock().lines() {
//         let line = line.unwrap();
//         let input = line.trim_end();
//         if input == "quit" {
//             return;
//         }
//         match parse(input) {
//             Ok(value) => {
//                 let ty = match get_type(&mut type_env, &value) {
//                     Ok(ty) => ty,
//                     Err(e) => {
//                         println!("Type error: {:?} for {}", e, input);
//                         String::from("error")
//                     }
//                 };
//                 match eval(&mut env, &value) {
//                     Ok(value) => println!("(the {} {})", ty, value),
//                     Err(e) => println!("Eval error: {}", e),
//                 }
//             }
//             Err(e) => println!("Parse error: {:?}", e),
//         }
//         print("orlo>>> ");
//     }
// }

#[wasm_bindgen]
pub fn repl(input: &str) -> String {
    let repl = REPL.get_or_init(|| Mutex::new(Repl::default()));
    let mut repl = repl.lock().unwrap();
    let input = input.trim();
    match parse(input) {
        Ok(value) => {
            let ty = match repl.get_type(&value) {
                Ok(ty) => ty,
                Err(e) => {
                    return format!("Type error: {:?} for {}", e, input);
                }
            };
            match eval(&mut repl.eval, &value) {
                Ok(value) => return format!("(the {} {})", ty, value),
                Err(e) => return format!("Eval error: {}", e),
            }
        }
        Err(e) => return format!("Parse error: {:?}", e),
    }
}
