pub mod env;
pub mod error;
pub mod eval;
pub mod expander;
pub mod identifier;
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

#[cfg(test)]
#[path = "tests/while_tests.rs"]
mod while_tests;

#[cfg(test)]
#[path = "tests/list_tests.rs"]
mod list_tests;

#[cfg(test)]
#[path = "tests/quasiquote_tests.rs"]
mod quasiquote_tests;

#[cfg(test)]
#[path = "tests/macro_tests.rs"]
mod macro_tests;

#[cfg(test)]
#[path = "tests/record_tests.rs"]
mod record_tests;

#[cfg(test)]
#[path = "tests/variant_tests.rs"]
mod variant_tests;
