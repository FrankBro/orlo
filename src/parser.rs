use std::iter::Peekable;

use crate::{
    error::ParserError,
    lexer::{self, Token},
    value::{FALSE, QUOTE, TRUE, Value},
};

type Result<T> = std::result::Result<T, ParserError>;

fn expect_token<T: Iterator<Item = Token>>(
    expected: Token,
    tokens: &mut Peekable<T>,
) -> Result<()> {
    let token = tokens.next().ok_or(ParserError::NoMoreTokens)?;
    if token != expected {
        return Err(ParserError::ExpectedToken(expected, token));
    }
    Ok(())
}

fn check_tokens_left<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Result<()> {
    let tokens_left: Vec<Token> = tokens.collect();
    if !tokens_left.is_empty() {
        return Err(ParserError::TokensLeft(tokens_left));
    }
    Ok(())
}

fn parse_string<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Result<Value> {
    match tokens.next() {
        Some(Token::String(string)) => Ok(Value::String(string)),
        Some(token) => Err(ParserError::UnexpectedToken(token)),
        None => Err(ParserError::NoMoreTokens),
    }
}

fn parse_atom<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Result<Value> {
    match tokens.next() {
        Some(Token::Atom(atom)) => match atom.as_str() {
            atom if atom == TRUE => Ok(Value::Bool(true)),
            atom if atom == FALSE => Ok(Value::Bool(false)),
            _ => Ok(Value::Atom(atom)),
        },
        Some(token) => Err(ParserError::UnexpectedToken(token)),
        None => Err(ParserError::NoMoreTokens),
    }
}

fn parse_number<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Result<Value> {
    match tokens.next() {
        Some(Token::Number(number)) => Ok(Value::Number(number)),
        Some(token) => Err(ParserError::UnexpectedToken(token)),
        None => Err(ParserError::NoMoreTokens),
    }
}

fn parse_quoted<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Result<Value> {
    expect_token(Token::Quote, tokens)?;
    let expr = parse_expr_impl(tokens)?;
    Ok(Value::List(vec![Value::Atom(QUOTE.to_owned()), expr]))
}

fn parse_any_list<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Result<Value> {
    expect_token(Token::LParen, tokens)?;
    let mut values = Vec::new();
    loop {
        match tokens.peek() {
            Some(Token::RParen) => {
                expect_token(Token::RParen, tokens)?;
                return Ok(Value::List(values));
            }
            Some(Token::Dot) => {
                expect_token(Token::Dot, tokens)?;
                let last = parse_expr_impl(tokens)?;
                expect_token(Token::RParen, tokens)?;
                return Ok(Value::DottedList(values, Box::new(last)));
            }
            Some(_) => {
                let value = parse_expr_impl(tokens)?;
                values.push(value);
            }
            None => {
                return Err(ParserError::NoMoreTokens);
            }
        }
    }
}

fn parse_expr_impl<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Result<Value> {
    match tokens.peek() {
        Some(Token::Atom(_)) => parse_atom(tokens),
        Some(Token::String(_)) => parse_string(tokens),
        Some(Token::Number(_)) => parse_number(tokens),
        Some(Token::Quote) => parse_quoted(tokens),
        Some(Token::LParen) => parse_any_list(tokens),
        Some(token) => Err(ParserError::UnexpectedToken(token.clone())),
        None => Err(ParserError::NoMoreTokens),
    }
}

pub fn parse_expr(input: &str) -> Result<Value> {
    let mut tokens = lexer::lex(input)
        .map_err(ParserError::Lexer)?
        .into_iter()
        .peekable();
    let value = parse_expr_impl(&mut tokens)?;
    check_tokens_left(&mut tokens)?;
    Ok(value)
}

pub fn parse_exprs(input: &str) -> Result<Vec<Value>> {
    let mut tokens = lexer::lex(input)
        .map_err(ParserError::Lexer)?
        .into_iter()
        .peekable();
    let mut vals = Vec::new();
    loop {
        if None == tokens.peek() {
            break;
        }
        let val = parse_expr_impl(&mut tokens)?;
        vals.push(val);
    }
    Ok(vals)
}

#[cfg(test)]
mod tests {
    use crate::{error::ParserError, value::Value};

    #[test]
    fn parse() {
        let cases = vec![
            (
                "(a test)",
                Ok(Value::List(vec![
                    Value::Atom("a".to_owned()),
                    Value::Atom("test".to_owned()),
                ])),
            ),
            (
                "(a (nested) test)",
                Ok(Value::List(vec![
                    Value::Atom("a".to_owned()),
                    Value::List(vec![Value::Atom("nested".to_owned())]),
                    Value::Atom("test".to_owned()),
                ])),
            ),
            (
                "(a (dotted . list) test)",
                Ok(Value::List(vec![
                    Value::Atom("a".to_owned()),
                    Value::DottedList(
                        vec![Value::Atom("dotted".to_owned())],
                        Box::new(Value::Atom("list".to_owned())),
                    ),
                    Value::Atom("test".to_owned()),
                ])),
            ),
            (
                "(a '(quoted (dotted . list)) test)",
                Ok(Value::List(vec![
                    Value::Atom("a".to_owned()),
                    Value::List(vec![
                        Value::Atom("quote".to_owned()),
                        Value::List(vec![
                            Value::Atom("quoted".to_owned()),
                            Value::DottedList(
                                vec![Value::Atom("dotted".to_owned())],
                                Box::new(Value::Atom("list".to_owned())),
                            ),
                        ]),
                    ]),
                    Value::Atom("test".to_owned()),
                ])),
            ),
            ("(a '(imbalanced parens)", Err(ParserError::NoMoreTokens)),
        ];
        for (input, expected) in cases {
            let actual = super::parse_expr(input);
            assert_eq!(expected, actual);
        }
    }
}
