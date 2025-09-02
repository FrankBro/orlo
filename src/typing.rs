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
    Arrow(Vec<Type>, Option<Box<Type>>, Box<Type>), // args, vararg, ret
    Var(Id),
    // List
    ListNil,
    ListExtend(Vec<Type>, Box<Type>),
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
        Type::Arrow(params, vararg, ret) => {
            let params = params
                .into_iter()
                .map(|param| replace_ty_constants_with_vars(env, param))
                .collect();
            let vararg = vararg.map(|vararg| replace_ty_constants_with_vars(env, *vararg));
            let ret = Box::new(replace_ty_constants_with_vars(env, *ret));
            Type::Arrow(params, vararg.map(Box::new), ret)
        }
        Type::ListNil => Type::ListNil,
        Type::ListExtend(heads, tail) => {
            let heads = heads
                .into_iter()
                .map(|head| replace_ty_constants_with_vars(env, head))
                .collect();
            let tail = Box::new(replace_ty_constants_with_vars(env, *tail));
            Type::ListExtend(heads, tail)
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeVar {
    Unbound(Level),
    Link(Type),
    Generic,
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
    #[token("->")]
    Arrow,
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
    #[token("λ")]
    Lambda,
}

fn parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Type, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    // parse "a", "pair[a, b]", "(λ a b . c -> d)", "(a b . c)"
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
            .ignore_then(ty.clone().repeated().at_least(1).collect::<Vec<_>>())
            .then(just(Token::Dot).ignore_then(ty.clone()).or_not())
            .then(just(Token::Arrow).ignore_then(ty.clone()))
            .map(|((params, vararg), ret)| Type::Arrow(params, vararg.map(Box::new), Box::new(ret)))
            .labelled("function type");
        let list = ty
            .clone()
            .repeated()
            .at_least(0)
            .collect::<Vec<_>>()
            .then(
                just(Token::Dot)
                    .ignore_then(ty.clone())
                    .or_not()
                    .map(|opt| opt.map(Box::new)),
            )
            .map(|(heads, tail)| match tail {
                Some(tail) => Type::ListExtend(heads, tail),
                None => Type::ListNil,
            })
            .labelled("list type");
        let paren = just(Token::LParen)
            .ignore_then(fn_ty.or(list))
            .then_ignore(just(Token::RParen))
            .labelled("parenthesized type");
        paren.or(app).or(atom)
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

    #[test]
    fn parse_type_tests() {
        let cases: Vec<(&str, Type)> = vec![
            (
                "forall[a] (λ a -> a)",
                Type::Arrow(vec![Type::Var(0)], None, Box::new(Type::Var(0))),
            ),
            ("int", const_("int")),
            (
                "forall[a b] (λ a b -> pair[a, b])",
                Type::Arrow(
                    vec![Type::Var(0), Type::Var(1)],
                    None,
                    Box::new(Type::App(
                        Box::new(const_("pair")),
                        vec![Type::Var(0), Type::Var(1)],
                    )),
                ),
            ),
            (
                "forall[a b] (λ a b -> b)",
                Type::Arrow(
                    vec![Type::Var(0), Type::Var(1)],
                    None,
                    Box::new(Type::Var(1)),
                ),
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
                "list[(λ int -> int)]",
                Type::App(
                    Box::new(const_("list")),
                    vec![Type::Arrow(
                        vec![const_("int")],
                        None,
                        Box::new(const_("int")),
                    )],
                ),
            ),
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
