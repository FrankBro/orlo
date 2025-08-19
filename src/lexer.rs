use std::fmt::Display;

use logos::{Lexer, Logos};

fn lex_string(lex: &mut Lexer<Token>) -> Option<String> {
    let slice = lex.slice();
    let string = slice[1..slice.len() - 1].to_owned();
    Some(string)
}

fn lex_atom(lex: &mut Lexer<Token>) -> Option<String> {
    let slice = lex.slice();
    let atom = slice[..slice.len()].to_owned();
    Some(atom)
}

fn lex_number(lex: &mut Lexer<Token>) -> Option<i64> {
    let slice = lex.slice();
    let number: i64 = slice[..slice.len()].parse().ok()?;
    Some(number)
}

#[derive(Logos, Clone, Debug, PartialEq, Eq)]
#[logos(subpattern symbol = r"[!#$%&|*+\-/:<=>?@^_~]")]
pub enum Token {
    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#, lex_string)]
    String(String),
    #[regex(r#"([a-z]|(?&symbol))([a-z0-9]|(?&symbol))*"#, lex_atom)]
    Atom(String),
    #[regex(r#"[0-9]+"#, lex_number)]
    Number(i64),
    #[token("'")]
    Quote,
    #[token(".")]
    Dot,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Atom(a) => write!(f, "{}", a),
            Token::Number(n) => write!(f, "{}", n),
            Token::Quote => write!(f, "'"),
            Token::Dot => write!(f, "."),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
        }
    }
}

pub fn lex(input: &str) -> Result<Vec<Token>, ()> {
    let lex = Token::lexer(input);
    let mut tokens = Vec::new();
    for token in lex {
        tokens.push(token?);
    }
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::{Token, lex};

    #[test]
    fn string() {
        let input = "\"this is a test\"";
        let expected = vec![Token::String("this is a test".to_owned())];
        let actual = lex(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn atom() {
        let cases = vec![
            ("a", vec![Token::Atom("a".to_owned())]),
            ("#e", vec![Token::Atom("#e".to_owned())]),
            ("@", vec![Token::Atom("@".to_owned())]),
            (
                "(a test)",
                vec![
                    Token::LParen,
                    Token::Atom("a".to_owned()),
                    Token::Atom("test".to_owned()),
                    Token::RParen,
                ],
            ),
        ];
        for (input, expected) in cases {
            let actual = lex(input).unwrap();
            assert_eq!(expected, actual);
        }
    }
}
