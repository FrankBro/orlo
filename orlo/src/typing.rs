use std::collections::HashMap;

use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;

pub type Id = usize;
pub type Level = i64;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Const(String),
    App(Box<Type>, Vec<Type>),
    // This is syntactic sugar for App(Const("array"), [ty])
    Array(Box<Type>),
    Arrow(Box<Type>, Box<Type>), // params (list) + vararg + ret
    Var(Id),
    // List
    ListNil,
    ListVarArg(Box<Type>),
    ListCons(Box<Type>, Box<Type>),
    // Row
    Record(Box<Type>),
    RowEmpty,
    RowExtend(Vec<(String, Type)>, Box<Type>),
}

impl Type {
    pub fn arrow(params: Vec<Type>, vararg: Option<Type>, ret: Type) -> Type {
        let init = if let Some(vararg) = vararg {
            Type::ListVarArg(Box::new(vararg))
        } else {
            Type::ListNil
        };
        let params = params.into_iter().rev().fold(init, |acc, param| {
            Type::ListCons(Box::new(param), Box::new(acc))
        });
        Type::Arrow(Box::new(params), Box::new(ret))
    }

    pub fn int() -> Type {
        Type::Const("int".to_owned())
    }

    pub fn bool() -> Type {
        Type::Const("bool".to_owned())
    }

    pub fn string() -> Type {
        Type::Const("string".to_owned())
    }
}

pub fn replace_ty_constants_with_vars(env: &HashMap<String, Type>, ty: Type) -> Type {
    match ty {
        Type::Const(name) => match env.get(&name) {
            Some(ty) => ty.clone(),
            None => Type::Const(name),
        },
        Type::Var(_) => ty,
        Type::App(ty, args) => {
            let ty = Box::new(replace_ty_constants_with_vars(env, *ty));
            let args = args
                .into_iter()
                .map(|arg| replace_ty_constants_with_vars(env, arg))
                .collect();
            Type::App(ty, args)
        }
        Type::Arrow(param, ret) => {
            let param = Box::new(replace_ty_constants_with_vars(env, *param));
            let ret = Box::new(replace_ty_constants_with_vars(env, *ret));
            Type::Arrow(param, ret)
        }
        Type::Array(ty) => {
            let ty = Box::new(replace_ty_constants_with_vars(env, *ty));
            Type::Array(ty)
        }
        Type::ListNil => Type::ListNil,
        Type::ListVarArg(vararg) => {
            let vararg = Box::new(replace_ty_constants_with_vars(env, *vararg));
            Type::ListVarArg(vararg)
        }
        Type::ListCons(head, tail) => {
            let head = Box::new(replace_ty_constants_with_vars(env, *head));
            let tail = Box::new(replace_ty_constants_with_vars(env, *tail));
            Type::ListCons(head, tail)
        }
        Type::Record(row) => Type::Record(replace_ty_constants_with_vars(env, *row).into()),
        Type::RowEmpty => Type::RowEmpty,
        Type::RowExtend(labels, rest) => {
            let labels = labels
                .into_iter()
                .map(|(label, ty)| (label, replace_ty_constants_with_vars(env, ty)))
                .collect();
            let rest = Box::new(replace_ty_constants_with_vars(env, *rest));
            Type::RowExtend(labels, rest)
        }
    }
}

#[derive(Default, Clone, Debug)]
pub struct Constraints {
    constraints: Vec<String>,
}

impl Constraints {
    pub fn union(mut self, other: Self) -> Self {
        for constraint in &other.constraints {
            if !self.constraints.contains(constraint) {
                self.constraints.push(constraint.clone());
            }
        }
        self
    }

    pub fn contains(&self, constraint: &String) -> bool {
        self.constraints.contains(constraint)
    }

    pub fn from_label(label: &str) -> Self {
        Constraints {
            constraints: vec![label.to_string()],
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeVar {
    Unbound(Level),
    UnboundRow(Level, Constraints),
    Link(Type),
    Generic,
    GenericRow(Constraints),
    // For things like a mutable empty array that'll be filled later
    Weak(Level),
}

#[derive(Logos, Clone, Debug, PartialEq, Eq)]
#[logos(skip r"[ \t\f\r\n]+")]
pub enum Token<'a> {
    Error,
    #[regex(r#"[a-z]+"#)]
    Const(&'a str),
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("lambda")]
    Lambda,
    #[token("forall")]
    Forall,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
}

fn parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Type, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    // parse "a", "pair[a, b]", "(lambda (a b . c) d)", "(a b . c)"
    recursive(|ty| {
        let atom = select! { Token::Const(name) => Type::Const(name.to_string()) }.labelled("type");
        let app = atom
            .clone()
            .then(
                just(Token::LBracket)
                    .ignore_then(ty.clone().separated_by(just(Token::Comma)).collect())
                    .then_ignore(just(Token::RBracket)),
            )
            .map(|(ty, args)| Type::App(Box::new(ty), args))
            .labelled("type application");
        let fn_ty = just(Token::Lambda)
            .ignore_then(just(Token::LParen))
            .ignore_then(ty.clone().repeated().at_least(1).collect::<Vec<Type>>())
            .then(just(Token::Dot).ignore_then(ty.clone()).or_not())
            .then_ignore(just(Token::RParen))
            .then(ty.clone())
            .map(|((params, vararg), ret)| {
                let init = if let Some(vararg) = vararg {
                    Type::ListVarArg(Box::new(vararg))
                } else {
                    Type::ListNil
                };
                let params = params.into_iter().rev().fold(init, |acc, param| {
                    Type::ListCons(Box::new(param), Box::new(acc))
                });
                Type::Arrow(Box::new(params), Box::new(ret))
            })
            .labelled("lambda type");
        // TODO: You can't have ( . ())
        let list = ty
            .clone()
            .repeated()
            .at_least(0)
            .collect::<Vec<_>>()
            .then(just(Token::Dot).ignore_then(ty.clone()).or_not())
            .map(|(heads, tail)| {
                if heads.is_empty() && tail.is_none() {
                    return Type::ListNil;
                }
                let tail = tail.unwrap_or(Type::ListNil);
                heads.into_iter().rev().fold(tail, |acc, head| {
                    Type::ListCons(Box::new(head), Box::new(acc))
                })
            })
            .labelled("list type");
        let array = ty
            .clone()
            .map(|ty| Type::Array(Box::new(ty)))
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .labelled("array type");
        let paren = just(Token::LParen)
            .ignore_then(fn_ty.or(list))
            .then_ignore(just(Token::RParen))
            .labelled("parenthesized type");
        array.or(paren).or(app).or(atom)
    })
}

fn forall_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, (Vec<String>, Type), extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    // parse "forall [a b] type" or just "type"
    just(Token::Forall)
        .ignore_then(
            just(Token::LBracket)
                .ignore_then(
                    select! { Token::Const(name) => name.to_string() }
                        .repeated()
                        .at_least(1)
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::RBracket)),
        )
        .then(parser())
        .or(parser().map(|ty| (vec![], ty)))
        .labelled("forall type")
}

#[allow(dead_code)]
pub fn parse<'a>(input: &'a str) -> Result<(Vec<String>, Type), Vec<Rich<'a, Token<'a>>>> {
    let token_iter = Token::lexer(input).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, span.into()),
        Err(()) => (Token::Error, span.into()),
    });
    let token_stream =
        Stream::from_iter(token_iter).map((0..input.len()).into(), |(t, s): (_, _)| (t, s));
    forall_parser().parse(token_stream).into_result()
}

#[cfg(test)]
mod tests {
    use crate::{
        infer::Env,
        typing::{Type, parse},
    };

    fn const_(name: &'static str) -> Type {
        Type::Const(name.to_owned())
    }

    fn int() -> Type {
        const_("int")
    }

    fn list(heads: Vec<Type>, tail: Option<Type>) -> Type {
        if heads.is_empty() && tail.is_none() {
            return Type::ListNil;
        }
        let tail = tail.unwrap_or(Type::ListNil);
        heads.into_iter().rev().fold(tail, |acc, head| {
            Type::ListCons(Box::new(head), Box::new(acc))
        })
    }

    fn arrow(params: Vec<Type>, vararg: Option<Type>, ret: Type) -> Type {
        let init = if let Some(vararg) = vararg {
            Type::ListVarArg(Box::new(vararg))
        } else {
            Type::ListNil
        };
        let params = params.into_iter().rev().fold(init, |acc, param| {
            Type::ListCons(Box::new(param), Box::new(acc))
        });
        Type::Arrow(Box::new(params), Box::new(ret))
    }

    #[test]
    fn parse_type_tests() {
        let cases: Vec<(&str, Type)> = vec![
            (
                "forall[a] (lambda (a) a)",
                arrow(vec![Type::Var(0)], None, Type::Var(0)),
            ),
            ("int", const_("int")),
            (
                "forall[a b] (lambda (a b) pair[a, b])",
                arrow(
                    vec![Type::Var(0), Type::Var(1)],
                    None,
                    Type::App(Box::new(const_("pair")), vec![Type::Var(0), Type::Var(1)]),
                ),
            ),
            (
                "forall[a b] (lambda (a b) b)",
                arrow(vec![Type::Var(0), Type::Var(1)], None, Type::Var(1)),
            ),
            ("bool", const_("bool")),
            (
                "pair[int, bool]",
                Type::App(
                    Box::new(const_("pair")),
                    vec![const_("int"), const_("bool")],
                ),
            ),
            // ("forall[a] (a, a) -> bool"),
            // ("forall[a] list[a -> a]"),
            (
                "list[(lambda (int) int)]",
                Type::App(
                    Box::new(const_("list")),
                    vec![arrow(vec![const_("int")], None, const_("int"))],
                ),
            ),
            ("()", Type::ListNil),
            ("(int int)", list(vec![int(), int()], None)),
            ("[int]", Type::Array(Box::new(int()))),
            // ("forall[a b] ((a -> a) -> b) -> b"),
            // ("forall[a b] (a -> a -> b) -> a -> b"),
            // ("forall[a b] (a -> b) -> a -> b"),
            // ("forall[a b] a -> b -> a"),
            // ("forall[a b c] ((a -> b) -> c) -> (a -> b) -> a -> b"),
            // ("forall[a b] (a -> b) -> (a -> b, a) -> bool"),
            // ("forall[a b] (a -> b, a) -> b"),
        ];
        let env = Env::default();
        for (input, expected) in cases {
            let (vars, ty) = parse(input).expect(&format!("input: {}", input));
            let mut env = env.clone();
            let actual = env.replace_ty_constants_with_vars(vars, ty);
            assert_eq!(expected, actual, "input: {}", input);
        }
    }
}
