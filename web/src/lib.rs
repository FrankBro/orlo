use std::sync::{Mutex, OnceLock};

use orlo::repl::Repl;
use wasm_bindgen::prelude::*;

static REPL: OnceLock<Mutex<Repl>> = OnceLock::new();

#[wasm_bindgen]
pub fn repl(input: &str) -> String {
    let repl = REPL.get_or_init(|| Mutex::new(Repl::default()));
    let mut repl = repl.lock().unwrap();
    let input = input.trim();
    match repl.handle_input(input) {
        Ok(the) => the.to_string(),
        Err(e) => match e {
            orlo::repl::Error::Parse => format!("Parse error for {}", input),
            orlo::repl::Error::Infer(e) => format!("Type error: {:?} for {}", e, input),
            orlo::repl::Error::Eval(e) => format!("Eval error: {}", e),
        },
    }
}
