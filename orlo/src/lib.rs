pub mod env;
pub mod error;
pub mod eval;
pub mod infer;
pub mod parser;
pub mod primitive;
pub mod repl;
pub mod typing;
pub mod util;
pub mod value;

#[cfg(test)]
#[path = "tests/let_tests.rs"]
mod let_tests;

#[cfg(test)]
#[path = "tests/array_tests.rs"]
mod array_tests;

#[cfg(test)]
#[path = "tests/for_tests.rs"]
mod for_tests;
