use std::{fmt::Display, io};

use crate::{lexer::Token, util::intersperse, value::Value};

#[derive(Debug, PartialEq, Eq)]
pub enum ParserError {
    Lexer,
    NoMoreTokens,
    UnexpectedToken(Token),
    ExpectedToken(Token, Token),
    TokensLeft(Vec<Token>),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::Lexer => write!(f, "Lexer error"),
            ParserError::NoMoreTokens => write!(f, "No more tokens"),
            ParserError::UnexpectedToken(t) => write!(f, "Unexpected token: {}", t),
            ParserError::ExpectedToken(expected, found) => {
                write!(f, "Expected token {}, found {}", expected, found)
            }
            ParserError::TokensLeft(tokens) => write!(f, "Tokens left: {:?}", tokens),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    NumArgs(usize, Vec<Value>),
    TypeMismatch(String, Value),
    Parser(ParserError),
    BadSpecialForm(String, Value),
    NotFunction(Value),
    UnboundVar(String, String),
    EmptyBody,
    IO(io::Error),
    Port(String),
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
            Error::Parser(e) => write!(f, "Parse error at {}", e),
            Error::EmptyBody => write!(f, "Function has empty body"),
            Error::IO(e) => write!(f, "IO error: {}", e),
            Error::Port(msg) => write!(f, "Port error: {}", msg),
        }
    }
}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::NumArgs(l0, l1), Self::NumArgs(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::TypeMismatch(l0, l1), Self::TypeMismatch(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Parser(l0), Self::Parser(r0)) => l0 == r0,
            (Self::BadSpecialForm(l0, l1), Self::BadSpecialForm(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::NotFunction(l0), Self::NotFunction(r0)) => l0 == r0,
            (Self::UnboundVar(l0, l1), Self::UnboundVar(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::IO(l0), Self::IO(r0)) => l0.kind() == r0.kind(),
            (Self::Port(l0), Self::Port(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}
