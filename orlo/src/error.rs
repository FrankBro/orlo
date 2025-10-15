use std::{fmt::Display, io};

use crate::{identifier, util::intersperse, value::Value};

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
    NoSuchField(String, Value),
    IndexOutOfBounds(i64, Value),
    Form(identifier::Error),
}

impl From<identifier::Error> for Error {
    fn from(error: identifier::Error) -> Self {
        Error::Form(error)
    }
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
            Error::NoSuchField(field, value) => write!(f, "Field {field} not found in {value}"),
            Error::IndexOutOfBounds(index, value) => {
                write!(f, "Index {index} out of bound for {value}")
            }
            Error::Form(error) => write!(f, "Form '{}' error: {}", error.name, error.description),
        }
    }
}
