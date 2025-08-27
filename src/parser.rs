use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;

use crate::value::Value;

#[derive(Logos, Clone, Debug, PartialEq, Eq)]
#[logos(subpattern symbol = r"[!#$%&|*+\-/:<=>?@^_~]")]
#[logos(skip r"[ \t\f\r\n]+")]
pub enum Token<'a> {
    Error,
    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#, |lex| {
        let slice = lex.slice();
        &slice[1..slice.len() - 1]
    })]
    String(&'a str),
    #[regex(r#"([a-z]|(?&symbol))([a-z0-9]|(?&symbol))*"#)]
    Atom(&'a str),
    #[regex(r#"[0-9]+"#, |lex| lex.slice().parse::<i64>().unwrap())]
    Number(i64),
    #[token("'")]
    Quote,
    #[token(".")]
    Dot,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("#t")]
    True,
    #[token("#f")]
    False,
}

fn parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Value, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|sexpr| {
        let atom = select! { Token::Atom(s) => Value::Atom(s.to_string()) }.labelled("atom");
        let string =
            select! { Token::String(s) => Value::String(s.to_string()) }.labelled("string");
        let number = select! { Token::Number(n) => Value::Number(n) }.labelled("number");
        let bool = select! {
            Token::True => Value::Bool(true),
            Token::False => Value::Bool(false),
        };
        let quoted = just(Token::Quote)
            .ignore_then(sexpr.clone())
            .map(|v| Value::List(vec![Value::Atom("quote".to_string()), v]))
            .labelled("quoted expression");
        let list = sexpr
            .clone()
            .repeated()
            .at_least(0)
            .collect::<Vec<_>>()
            .then(
                just(Token::Dot)
                    .ignore_then(sexpr.clone())
                    .or_not()
                    .map(|opt| opt.map(Box::new)),
            )
            .map(|(head, tail)| match tail {
                Some(t) => Value::DottedList(head, t),
                None => Value::List(head),
            })
            .labelled("list")
            .delimited_by(just(Token::LParen), just(Token::RParen));
        bool.or(atom).or(string).or(number).or(quoted).or(list)
    })
}

pub fn parse<'a>(input: &'a str) -> Result<Value, Vec<Rich<'a, Token<'a>>>> {
    let token_iter = Token::lexer(input).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::Error, span.into()),
    });
    let token_stream =
        Stream::from_iter(token_iter).map((0..input.len()).into(), |(t, s): (_, _)| (t, s));
    parser().parse(token_stream).into_result()
}

pub fn parse_multiple<'a>(input: &'a str) -> Result<Vec<Value>, Vec<Rich<'a, Token<'a>>>> {
    let token_iter = Token::lexer(input).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::Error, span.into()),
    });
    let token_stream =
        Stream::from_iter(token_iter).map((0..input.len()).into(), |(t, s): (_, _)| (t, s));
    parser()
        .repeated()
        .at_least(0)
        .collect::<Vec<_>>()
        .parse(token_stream)
        .into_result()
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{parse, parse_multiple},
        value::Value,
    };

    fn atom(name: &str) -> Value {
        Value::Atom(name.to_string())
    }

    fn list(values: Vec<Value>) -> Value {
        Value::List(values)
    }

    fn dotted_list(head: Vec<Value>, tail: Value) -> Value {
        Value::DottedList(head, Box::new(tail))
    }

    fn string(str: &str) -> Value {
        Value::String(str.to_string())
    }

    fn bool(value: bool) -> Value {
        Value::Bool(value)
    }

    #[test]
    fn parse_multiple_test() {
        let cases = vec![
            ("a b c", vec![atom("a"), atom("b"), atom("c")]),
            (
                "(a b) (c d)",
                vec![
                    list(vec![atom("a"), atom("b")]),
                    list(vec![atom("c"), atom("d")]),
                ],
            ),
            (
                "'a 'b 'c",
                vec![
                    list(vec![atom("quote"), atom("a")]),
                    list(vec![atom("quote"), atom("b")]),
                    list(vec![atom("quote"), atom("c")]),
                ],
            ),
            (
                "(a . b) (c . d)",
                vec![
                    dotted_list(vec![atom("a")], atom("b")),
                    dotted_list(vec![atom("c")], atom("d")),
                ],
            ),
        ];
        for (case, expected) in cases {
            let actual = match parse_multiple(case) {
                Ok(values) => values,
                Err(e) => panic!("Failed to parse '{}': {:?}", case, e),
            };
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn parse_test() {
        let cases = vec![
            ("#f", bool(false)),
            ("#t", bool(true)),
            ("a", atom("a")),
            ("#e", atom("#e")),
            ("@", atom("@")),
            ("(a test)", list(vec![atom("a"), atom("test")])),
            (
                "(a (nested) list)",
                list(vec![atom("a"), list(vec![atom("nested")]), atom("list")]),
            ),
            ("(a . b)", dotted_list(vec![atom("a")], atom("b"))),
            (
                "(a b . c)",
                dotted_list(vec![atom("a"), atom("b")], atom("c")),
            ),
            (
                "(a (nested) . b)",
                dotted_list(vec![atom("a"), list(vec![atom("nested")])], atom("b")),
            ),
            (
                "'(a b c)",
                list(vec![
                    atom("quote"),
                    list(vec![atom("a"), atom("b"), atom("c")]),
                ]),
            ),
            ("'a", list(vec![atom("quote"), atom("a")])),
            ("\"this is a test\"", string("this is a test")),
            ("12345", Value::Number(12345)),
        ];
        for (case, expected) in cases {
            let actual = parse(case).unwrap();
            assert_eq!(expected, actual);
        }
    }
}
