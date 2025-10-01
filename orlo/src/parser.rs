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
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("`")]
    Quasiquote,
    #[token(",")]
    Unquote,
    #[token(",@")]
    UnquoteSplicing,
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
        let quasiquoted = just(Token::Quasiquote)
            .ignore_then(sexpr.clone())
            .map(|v| Value::List(vec![Value::Atom("quasiquote".to_string()), v]))
            .labelled("quasiquoted expression");
        let unquoted = just(Token::Unquote)
            .ignore_then(sexpr.clone())
            .map(|v| Value::List(vec![Value::Atom("unquote".to_string()), v]))
            .labelled("unquoted expression");
        let unquote_splicing = just(Token::UnquoteSplicing)
            .ignore_then(sexpr.clone())
            .map(|v| Value::List(vec![Value::Atom("unquote-splicing".to_string()), v]))
            .labelled("unquote-splicing expression");
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
        let array = sexpr
            .clone()
            .repeated()
            .at_least(0)
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .map(Value::Array)
            .labelled("array");
        bool.or(atom)
            .or(string)
            .or(number)
            .or(quoted)
            .or(quasiquoted)
            .or(unquoted)
            .or(unquote_splicing)
            .or(list)
            .or(array)
    })
}

pub fn parse<'a>(input: &'a str) -> Result<Value, Vec<Rich<'a, Token<'a>>>> {
    let token_iter = Token::lexer(input).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::Error, span.into()),
    });
    let token_stream =
        Stream::from_iter(token_iter).map((0..input.len()).into(), |(t, s): (_, _)| (t, s));
    parser()
        .parse(token_stream)
        .into_result()
        .map(|v| expand_quasi_quote(&v))
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
        .map(|vals| vals.into_iter().map(|v| expand_quasi_quote(&v)).collect())
}

fn expand_quasi_quote(form: &Value) -> Value {
    match form {
        Value::List(items) if !items.is_empty() => {
            if let Value::Atom(s) = &items[0] {
                if s == "quasiquote" && items.len() == 2 {
                    // Top-level quasiquote
                    return expand_quasi_quote_inner(&items[1], 1);
                }
            }
            Value::List(items.iter().map(expand_quasi_quote).collect())
        }
        Value::DottedList(head, tail) => {
            let head = head.into_iter().map(expand_quasi_quote).collect();
            let tail = Box::new(expand_quasi_quote(tail));
            Value::DottedList(head, tail)
        }
        _ => form.clone(),
    }
}

fn expand_quasi_quote_inner(form: &Value, depth: u64) -> Value {
    match form {
        Value::Atom(_) => form.quote(),
        // TODO: Might need to do something special for arrays
        Value::Array(_) | Value::Number(_) | Value::String(_) | Value::Bool(_) => form.clone(),

        Value::List(items) if items.is_empty() => form.quote(),
        Value::List(items) => {
            if let Value::Atom(s) = &items[0] {
                match s.as_str() {
                    "quasiquote" if items.len() == 2 => {
                        let inner = expand_quasi_quote_inner(&items[1], depth + 1);
                        Value::List(vec![Value::Atom("quasiquote".to_owned()), inner])
                    }
                    "unquote" if items.len() == 2 && depth == 1 => items[1].clone(),
                    "unquote-splicing" if items.len() == 2 && depth == 1 => {
                        panic!("unquote-splicing invalid outside of list context");
                    }
                    _ => expand_list_items(items, depth),
                }
            } else {
                expand_list_items(items, depth)
            }
        }
        Value::DottedList(head, tail) => {
            let head = head
                .iter()
                .map(|v| expand_quasi_quote_inner(v, depth))
                .collect();
            let tail = Box::new(expand_quasi_quote_inner(tail, depth));
            Value::DottedList(head, tail)
        }

        Value::PrimitiveFunc(_) | Value::Func { .. } | Value::IOFunc(_) | Value::Port(_) => {
            unreachable!()
        }
    }
}

fn expand_list_items(items: &[Value], depth: u64) -> Value {
    let mut result = Vec::new();

    for item in items {
        match item {
            Value::List(inner) if !inner.is_empty() => {
                if let Value::Atom(s) = &inner[0] {
                    if s == "unquote-splicing" && inner.len() == 2 && depth == 1 {
                        if !result.is_empty() {
                            let accumulated = Value::List(vec![
                                Value::Atom("list".to_owned()),
                                Value::List(result.clone()),
                            ]);
                            let appended = Value::List(vec![
                                Value::Atom("append".to_owned()),
                                inner[1].clone(),
                                accumulated,
                            ]);
                            return appended;
                        } else {
                            return Value::List(vec![
                                Value::Atom("append".to_owned()),
                                inner[1].clone(),
                                Value::List(vec![Value::Atom("list".to_owned())]),
                            ]);
                        }
                    }
                }
            }
            _ => {}
        }

        result.push(expand_quasi_quote_inner(item, depth));
    }
    Value::List(
        std::iter::once(Value::Atom("list".to_owned()))
            .chain(result.into_iter())
            .collect(),
    )
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{parse, parse_multiple},
        value::Value,
    };

    #[test]
    fn quasiquote_expansion() {
        let cases = vec![
            ("`a", "(quote a)"),
            ("`(1 2 3)", "(list 1 2 3)"),
            ("(let ((x 10)) `(1 ,x 3))", "(let ((x 10)) (list 1 x 3))"),
            (
                "(let ((x 1) (y 2)) ``(a ,x ,(list ,y 3)))",
                "(let ((x 1) (y 2)) (list (quote a) x (list y 3)))",
            ),
            (
                "(let ((lst '(2 3))) `(1 ,@lst 4))",
                "(let ((lst (quote (2 3)))) (append lst (list 1 4)))",
            ),
        ];
        for (input, expected) in cases {
            let expanded = parse(input).unwrap();
            assert_eq!(expanded.to_string(), expected);
        }
    }

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
