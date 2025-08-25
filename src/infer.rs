use std::collections::HashMap;

use crate::value::{PrimitiveFunc, QUOTE, Value};

pub type Id = usize;
pub type Level = i64;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Const(&'static str),
    App(Box<Type>, Vec<Type>),
    Arrow(Vec<Type>, Box<Type>),
    Var(Id),
}

pub fn replace_ty_constants_with_vars(env: &HashMap<String, Type>, ty: Type) -> Type {
    match ty {
        Type::Const(name) => match env.get(name) {
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
        Type::Arrow(params, ret) => {
            let params = params
                .into_iter()
                .map(|param| replace_ty_constants_with_vars(env, param))
                .collect();
            let ret = Box::new(replace_ty_constants_with_vars(env, *ret));
            Type::Arrow(params, ret)
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeVar {
    Unbound(Level),
    Link(Type),
    Generic,
}

pub static SYMBOL: &str = "symbol";
pub static INT: &str = "int";
pub static BOOL: &str = "bool";
pub static STRING: &str = "string";

#[derive(Debug, PartialEq)]
pub enum Error {
    TypeVarNotFound(Id),
    VarNotFound(String),
    BadSpecialForm(String, Value),
    UnexpectedNumberOfArguments { expected: usize, actual: usize },
    ExpectedAFunction,
    RecursiveType,
    CannotUnify(Type, Type),
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Default, Clone)]
pub struct Env {
    vars: HashMap<String, Type>,
    tvars: Vec<TypeVar>,
}

impl Env {
    pub fn infer_value(&mut self, value: &Value) -> Result<Type> {
        let ty = self.infer(0, value)?;
        self.generalize(-1, &ty)?;
        Ok(ty)
    }

    fn new_unbound_tvar(&mut self, level: Level) -> Type {
        let id = self.tvars.len();
        self.tvars.push(TypeVar::Unbound(level));
        Type::Var(id)
    }

    fn new_generic_tvar(&mut self) -> Type {
        let id = self.tvars.len();
        self.tvars.push(TypeVar::Generic);
        Type::Var(id)
    }

    fn get_var(&self, name: &str) -> Result<Type> {
        self.vars
            .get(name)
            .cloned()
            .ok_or(Error::VarNotFound(name.to_string()))
    }

    fn get_tvar(&self, id: Id) -> Result<&TypeVar> {
        self.tvars.get(id).ok_or(Error::TypeVarNotFound(id))
    }

    fn get_mut_tvar(&mut self, id: Id) -> Result<&mut TypeVar> {
        self.tvars.get_mut(id).ok_or(Error::TypeVarNotFound(id))
    }

    fn link(&mut self, id: Id, ty: Type) -> Result<()> {
        let tvar = self.get_mut_tvar(id)?;
        *tvar = TypeVar::Link(ty);
        Ok(())
    }

    fn occurs_check_adjust_levels(
        &mut self,
        tvar_id: Id,
        tvar_level: Level,
        ty: &Type,
    ) -> Result<()> {
        match ty {
            Type::Var(other_id) => {
                let other_tvar = self.get_mut_tvar(*other_id)?;
                match other_tvar.clone() {
                    TypeVar::Link(ty) => self.occurs_check_adjust_levels(tvar_id, tvar_level, &ty),
                    TypeVar::Generic => panic!(),
                    TypeVar::Unbound(other_level) => {
                        if *other_id == tvar_id {
                            Err(Error::RecursiveType)
                        } else {
                            if other_level > tvar_level {
                                *other_tvar = TypeVar::Unbound(tvar_level);
                            }
                            Ok(())
                        }
                    }
                }
            }
            Type::App(ty, args) => {
                for arg in args {
                    self.occurs_check_adjust_levels(tvar_id, tvar_level, arg)?;
                }
                self.occurs_check_adjust_levels(tvar_id, tvar_level, ty)
            }
            Type::Arrow(params, ret) => {
                for param in params {
                    self.occurs_check_adjust_levels(tvar_id, tvar_level, param)?;
                }
                self.occurs_check_adjust_levels(tvar_id, tvar_level, ret)
            }
            Type::Const(_) => Ok(()),
        }
    }

    fn unify(&mut self, ty1: &Type, ty2: &Type) -> Result<()> {
        if ty1 == ty2 {
            return Ok(());
        }
        match (ty1, ty2) {
            (Type::Const(name1), Type::Const(name2)) if name1 == name2 => Ok(()),
            (Type::App(app_ty1, args1), Type::App(app_ty2, args2)) => {
                if args1.len() != args2.len() {
                    return Err(Error::CannotUnify(ty1.clone(), ty2.clone()));
                }
                for i in 0..args1.len() {
                    let arg1 = &args1[i];
                    let arg2 = &args2[i];
                    self.unify(arg1, arg2)?;
                }
                self.unify(app_ty1, app_ty2)
            }
            (Type::Arrow(params1, ret1), Type::Arrow(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return Err(Error::CannotUnify(ty1.clone(), ty2.clone()));
                }
                for i in 0..params1.len() {
                    let param1 = &params1[i];
                    let param2 = &params2[i];
                    self.unify(param1, param2)?;
                }
                self.unify(ret1, ret2)
            }
            (Type::Var(id1), Type::Var(id2)) if id1 == id2 => {
                panic!("multiple instance of a type variable")
            }
            (Type::Var(id), _) => {
                let tvar = self.get_tvar(*id)?;
                match tvar.clone() {
                    TypeVar::Unbound(level) => {
                        self.occurs_check_adjust_levels(*id, level, ty2)?;
                        self.link(*id, ty2.clone())
                    }
                    TypeVar::Link(ty1) => self.unify(&ty1, ty2),
                    TypeVar::Generic => Err(Error::CannotUnify(ty1.clone(), ty2.clone())),
                }
            }
            (_, Type::Var(id)) => {
                let tvar = self.get_mut_tvar(*id)?;
                match tvar.clone() {
                    TypeVar::Unbound(level) => {
                        self.occurs_check_adjust_levels(*id, level, ty1)?;
                        self.link(*id, ty1.clone())
                    }
                    TypeVar::Link(ty2) => self.unify(ty1, &ty2),
                    TypeVar::Generic => Err(Error::CannotUnify(ty1.clone(), ty2.clone())),
                }
            }
            _ => Err(Error::CannotUnify(ty1.clone(), ty2.clone())),
        }
    }

    fn infer(&mut self, level: Level, val: &Value) -> Result<Type> {
        match val {
            Value::Atom(name) => {
                let ty = self.get_var(name)?;
                self.instantiate(level, ty)
            }
            Value::Number(_) => Ok(Type::Const(INT)),
            Value::String(_) => Ok(Type::Const(STRING)),
            Value::Bool(_) => Ok(Type::Const(BOOL)),
            Value::PrimitiveFunc(f) => match f {
                PrimitiveFunc::Add
                | PrimitiveFunc::Sub
                | PrimitiveFunc::Mul
                | PrimitiveFunc::Div
                | PrimitiveFunc::Rem => Ok(Type::Arrow(
                    vec![Type::Const(INT), Type::Const(INT)],
                    Box::new(Type::Const(INT)),
                )),
                PrimitiveFunc::Eq
                | PrimitiveFunc::Lt
                | PrimitiveFunc::Gt
                | PrimitiveFunc::Ne
                | PrimitiveFunc::Ge
                | PrimitiveFunc::Le => Ok(Type::Arrow(
                    vec![Type::Const(INT), Type::Const(INT)],
                    Box::new(Type::Const(BOOL)),
                )),
                PrimitiveFunc::And | PrimitiveFunc::Or => Ok(Type::Arrow(
                    vec![Type::Const(BOOL), Type::Const(BOOL)],
                    Box::new(Type::Const(BOOL)),
                )),
                PrimitiveFunc::StringEq
                | PrimitiveFunc::StringLt
                | PrimitiveFunc::StringGt
                | PrimitiveFunc::StringLe
                | PrimitiveFunc::StringGe => Ok(Type::Arrow(
                    vec![Type::Const(STRING), Type::Const(STRING)],
                    Box::new(Type::Const(BOOL)),
                )),
                PrimitiveFunc::Car => {
                    let ret = self.new_unbound_tvar(level);
                    todo!();
                    Ok(Type::Arrow(vec![], Box::new(ret)))
                }
                PrimitiveFunc::Cdr => todo!(),
                PrimitiveFunc::Cons => todo!(),
                PrimitiveFunc::Eqv => todo!(),
                PrimitiveFunc::Equal => todo!(),
            },
            Value::List(vals) => match &vals[..] {
                [Value::Atom(atom), val] if atom == QUOTE => todo!(),
                [Value::Atom(atom), pred, conseq, alt] if atom == "if" => todo!(),
                [Value::Atom(atom), Value::Atom(var), form] if atom == "set!" => todo!(),
                [Value::Atom(atom), Value::Atom(var), form] if atom == "define" => todo!(),
                [Value::Atom(atom), Value::List(name_args), body @ ..] if atom == "define" => {
                    todo!()
                }
                [
                    Value::Atom(atom),
                    Value::DottedList(name_args, vararg),
                    body @ ..,
                ] if atom == "define" => {
                    todo!()
                }
                [Value::Atom(atom), Value::List(params), body @ ..] if atom == "lambda" => todo!(),
                [
                    Value::Atom(atom),
                    Value::DottedList(params, vararg),
                    body @ ..,
                ] if atom == "lambda" => {
                    todo!()
                }
                [Value::Atom(atom), Value::Atom(vararg), body @ ..] if atom == "lambda" => todo!(),
                [Value::Atom(atom), Value::String(path)] if atom == "load" => todo!(),
                [func, args @ ..] => {
                    let f_ty = self.infer(level, func)?;
                    let (params, ret) = self.match_fun_ty(args.len(), f_ty)?;
                    for i in 0..args.len() {
                        let arg = &args[i];
                        let arg_ty = self.infer(level, arg)?;
                        let param = &params[i];
                        self.unify(&arg_ty, param)?;
                    }
                    Ok(*ret)
                }
                _ => Err(Error::BadSpecialForm(
                    "unrecognized special form".to_owned(),
                    val.clone(),
                )),
            },
            Value::DottedList(values, value) => todo!(),
            Value::Func {
                params,
                vararg,
                body,
                closure,
            } => todo!(),
            Value::IOFunc(iofunc) => todo!(),
            Value::Port(_) => todo!(),
        }
    }

    fn generalize(&mut self, level: Level, ty: &Type) -> Result<()> {
        match ty {
            Type::Var(id) => {
                let tvar = self.get_mut_tvar(*id)?;
                match tvar.clone() {
                    TypeVar::Unbound(other_level) if other_level > level => {
                        *tvar = TypeVar::Generic;
                        Ok(())
                    }
                    TypeVar::Unbound(_) => Ok(()),
                    TypeVar::Link(ty) => self.generalize(level, &ty),
                    TypeVar::Generic => Ok(()),
                }
            }
            Type::App(ty, args) => {
                for arg in args {
                    self.generalize(level, arg)?;
                }
                self.generalize(level, ty)
            }
            Type::Arrow(params, ret) => {
                for param in params {
                    self.generalize(level, param)?;
                }
                self.generalize(level, ret)
            }
            Type::Const(_) => Ok(()),
        }
    }

    fn instantiate(&mut self, level: Level, ty: Type) -> Result<Type> {
        let mut id_vars = HashMap::new();
        self.instantiate_impl(&mut id_vars, level, ty)
    }

    fn instantiate_impl(
        &mut self,
        id_vars: &mut HashMap<Id, Type>,
        level: Level,
        ty: Type,
    ) -> Result<Type> {
        match ty {
            Type::Const(_) => Ok(ty),
            Type::Var(id) => {
                let tvar = self.get_tvar(id)?;
                match tvar.clone() {
                    TypeVar::Unbound(_) => Ok(ty),
                    TypeVar::Link(ty) => self.instantiate_impl(id_vars, level, ty),
                    TypeVar::Generic => {
                        let ty = id_vars
                            .entry(id)
                            .or_insert_with(|| self.new_unbound_tvar(level));
                        Ok(ty.clone())
                    }
                }
            }
            Type::App(ty, args) => {
                let instantiated_ty = self.instantiate_impl(id_vars, level, *ty)?;
                let mut instantiated_args = Vec::with_capacity(args.len());
                for arg in args {
                    let instantiated_arg = self.instantiate_impl(id_vars, level, arg)?;
                    instantiated_args.push(instantiated_arg);
                }
                Ok(Type::App(Box::new(instantiated_ty), instantiated_args))
            }
            Type::Arrow(params, ret) => {
                let instantiated_ret = self.instantiate_impl(id_vars, level, *ret)?;
                let mut instantiated_params = Vec::with_capacity(params.len());
                for param in params {
                    let instantiated_param = self.instantiate_impl(id_vars, level, param)?;
                    instantiated_params.push(instantiated_param);
                }
                Ok(Type::Arrow(instantiated_params, Box::new(instantiated_ret)))
            }
        }
    }

    fn match_fun_ty(&mut self, num_params: usize, ty: Type) -> Result<(Vec<Type>, Box<Type>)> {
        match ty {
            Type::Arrow(params, ret) => {
                if params.len() != num_params {
                    return Err(Error::UnexpectedNumberOfArguments {
                        expected: params.len(),
                        actual: num_params,
                    });
                }
                Ok((params, ret))
            }
            Type::Var(id) => {
                let tvar = self.get_tvar(id)?;
                match tvar.clone() {
                    TypeVar::Unbound(level) => {
                        let mut params = Vec::with_capacity(num_params);
                        for _ in 0..num_params {
                            let param = self.new_unbound_tvar(level);
                            params.push(param);
                        }
                        let ret = Box::new(self.new_unbound_tvar(level));
                        let ty = Type::Arrow(params.clone(), ret.clone());
                        self.link(id, ty)?;
                        Ok((params, ret))
                    }
                    TypeVar::Link(ty) => self.match_fun_ty(num_params, ty),
                    TypeVar::Generic => Err(Error::ExpectedAFunction),
                }
            }
            _ => Err(Error::ExpectedAFunction),
        }
    }

    pub fn ty_to_string(&self, ty: &Type) -> Result<String> {
        let mut namer = Namer::new();
        self.ty_to_string_impl(&mut namer, ty, false)
    }

    fn ty_to_string_impl(&self, namer: &mut Namer, ty: &Type, is_simple: bool) -> Result<String> {
        match ty {
            Type::Const(name) => Ok(name.to_string()),
            Type::App(ty, args) => {
                let mut ty_str = self.ty_to_string_impl(namer, ty, true)?;
                ty_str.push('[');
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        ty_str.push_str(", ")
                    }
                    let arg = self.ty_to_string_impl(namer, arg, false)?;
                    ty_str.push_str(&arg);
                }
                ty_str.push(']');
                Ok(ty_str)
            }
            Type::Arrow(params, ret) => {
                let mut ty_str = if is_simple {
                    "(".to_owned()
                } else {
                    "".to_owned()
                };
                if params.len() == 1 {
                    let param = self.ty_to_string_impl(namer, &params[0], true)?;
                    let ret = self.ty_to_string_impl(namer, ret, false)?;
                    ty_str.push_str(&param);
                    ty_str.push_str(" -> ");
                    ty_str.push_str(&ret);
                } else {
                    ty_str.push('(');
                    for (i, param) in params.iter().enumerate() {
                        if i != 0 {
                            ty_str.push_str(", ");
                        }
                        let param = self.ty_to_string_impl(namer, param, false)?;
                        ty_str.push_str(&param);
                    }
                    let ret = self.ty_to_string_impl(namer, ret, false)?;
                    ty_str.push_str(") -> ");
                    ty_str.push_str(&ret);
                }
                if is_simple {
                    ty_str.push(')');
                }
                Ok(ty_str)
            }
            Type::Var(id) => {
                let tvar = self.get_tvar(*id)?;
                match tvar {
                    TypeVar::Generic => {
                        let name = namer.get_or_insert(*id);
                        Ok(name.to_string())
                    }
                    TypeVar::Unbound(_) => Ok(format!("_{}", id)),
                    TypeVar::Link(ty) => self.ty_to_string_impl(namer, ty, is_simple),
                }
            }
        }
    }
}

struct Namer {
    next_name: u8,
    names: HashMap<Id, u8>,
}

impl Namer {
    fn new() -> Self {
        let next_name = 97;
        let names = HashMap::new();
        Self { next_name, names }
    }

    fn get_or_insert(&mut self, id: Id) -> char {
        let name = match self.names.get(&id) {
            Some(name) => *name,
            None => {
                let name = self.next_name;
                self.next_name += 1;
                self.names.insert(id, name);
                name
            }
        };
        name as char
    }
}
#[cfg(test)]
mod tests {
    use super::Error;

    enum Expected {
        Pass(String),
        Fail(Error),
    }

    fn pass(sig: &str) -> Expected {
        Expected::Pass(sig.to_owned())
    }

    fn fail(e: Error) -> Expected {
        Expected::Fail(e)
    }

    // #[test]
    // fn infer() {
    //     let cases: Vec<(&str, Type)> = vec![
    //         ("42", Type::Const(INT)),
    //         ("#t", Type::Const(BOOL)),
    //         ("\"hello\"", Type::Const(STRING)),
    //     ];
    //     for (input, expected) in cases {
    //         let mut env = Env::default();
    //         let value = parse_expr(input).unwrap();
    //         let actual = env.infer_value(&value).unwrap();
    //         assert_eq!(actual, expected);
    //     }
    // }
}
