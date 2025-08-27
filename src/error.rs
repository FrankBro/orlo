use std::{fmt::Display, io};

use crate::{util::intersperse, value::Value};

#[derive(Debug, PartialEq)]
pub enum Error {
    NumArgs(usize, Vec<Value>),
    TypeMismatch(String, Value),
    BadSpecialForm(String, Value),
    NotFunction(Value),
    UnboundVar(String, String),
    EmptyBody,
    IO(io::ErrorKind),
    Port(String),
    Parser,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnboundVar(msg, name) => write!(f, "{}: {}", msg, name),
            Error::BadSpecialForm(msg, form) => write!(f, "{}: {}", msg, form),
            Error::NotFunction(form) => write!(f, "Not a function: {}", form),
            Error::NumArgs(expected, found) => write!(
                f,
                "Expected {} args; found values {}",
                expected,
                intersperse(found)
            ),
            Error::TypeMismatch(expected, found) => {
                write!(f, "Invalid type: expected {}, found {}", expected, found)
            }
            Error::EmptyBody => write!(f, "Function has empty body"),
            Error::IO(e) => write!(f, "IO error: {}", e),
            Error::Port(msg) => write!(f, "Port error: {}", msg),
            Error::Parser => write!(f, "Parse error"),
        }
    }
}
