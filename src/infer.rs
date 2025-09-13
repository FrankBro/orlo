use std::collections::HashMap;

use crate::{
    parser::parse_multiple,
    typing::{Id, Level, Type, TypeVar, replace_ty_constants_with_vars},
    value::{IOFunc, PrimitiveFunc, QUOTE, Value},
};

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
    CannotUnifyList(Type, Type),
    FunctionArgNotSymbol(String),
    IO(std::io::ErrorKind),
    Parser,
    DefineFunctionNotSymbol(Value),
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
            Type::Arrow(param, vararg, ret) => {
                self.occurs_check_adjust_levels(tvar_id, tvar_level, param)?;
                if let Some(vararg) = vararg {
                    self.occurs_check_adjust_levels(tvar_id, tvar_level, vararg)?;
                }
                self.occurs_check_adjust_levels(tvar_id, tvar_level, ret)
            }
            Type::ListCons(head, tail) => {
                self.occurs_check_adjust_levels(tvar_id, tvar_level, head)?;
                self.occurs_check_adjust_levels(tvar_id, tvar_level, tail)
            }
            Type::Const(_) | Type::ListNil => Ok(()),
        }
    }

    // TODO: Does this really need to be it's own function?
    fn unify_list(&mut self, ty1: &Type, ty2: &Type) -> Result<()> {
        match (ty1, ty2) {
            (Type::ListNil, Type::ListNil) => Ok(()),
            (Type::ListCons(head1, tail1), Type::ListCons(head2, tail2)) => {
                self.unify(head1, head2)?;
                self.unify_list(tail1, tail2)
            }
            // TODO: Do we need special handling here ala row polymorphism?
            (Type::Var(_), _) | (_, Type::Var(_)) => self.unify(ty1, ty2),
            _ => Err(Error::CannotUnifyList(ty1.clone(), ty2.clone())),
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
            (Type::Arrow(param1, vararg1, ret1), Type::Arrow(param2, vararg2, ret2)) => {
                self.unify(param1, param2)?;
                match (vararg1, vararg2) {
                    (Some(vararg1), Some(vararg2)) => self.unify(vararg1, vararg2)?,
                    (None, None) => {}
                    _ => return Err(Error::CannotUnify(ty1.clone(), ty2.clone())),
                }
                self.unify(ret1, ret2)
            }
            (Type::ListNil, Type::ListNil) => Ok(()),
            (Type::ListCons(_, _), Type::ListCons(_, _)) => self.unify_list(ty1, ty2),
            (Type::ListCons(_, _), Type::ListNil) | (Type::ListNil, Type::ListCons(_, _)) => {
                Err(Error::CannotUnifyList(ty1.clone(), ty2.clone()))
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

    fn define_arrow(
        &mut self,
        level: Level,
        params: &[Value],
        vararg: Option<&Value>,
        body: &[Value],
    ) -> Result<Type> {
        let mut param_tys = Vec::with_capacity(params.len());
        let old_vars = self.vars.clone();
        for param in params {
            let param = match param {
                Value::Atom(name) => name,
                _ => {
                    return Err(Error::FunctionArgNotSymbol(format!(
                        "{:?} is not a symbol",
                        param
                    )));
                }
            };
            let param_ty = self.new_unbound_tvar(level);
            self.vars.insert(param.to_owned(), param_ty.clone());
            param_tys.push(param_ty);
        }
        let vararg_ty = match vararg.as_ref() {
            Some(Value::Atom(name)) => {
                let vararg_ty = self.new_unbound_tvar(level);
                self.vars.insert(name.to_owned(), vararg_ty.clone());
                Some(vararg_ty)
            }

            Some(_) => {
                return Err(Error::FunctionArgNotSymbol(format!(
                    "{:?} is not a symbol",
                    vararg
                )));
            }
            None => None,
        };
        let mut ret_ty = Type::Const("void".to_owned());
        for val in body {
            let body_ty = self.infer(level, val)?;
            ret_ty = body_ty;
        }
        self.vars = old_vars;
        let params = param_tys
            .into_iter()
            .rev()
            .fold(Type::ListNil, |acc, param| {
                Type::ListCons(Box::new(param), Box::new(acc))
            });
        Ok(Type::Arrow(
            Box::new(params),
            vararg_ty.map(Box::new),
            Box::new(ret_ty),
        ))
    }

    fn infer(&mut self, level: Level, val: &Value) -> Result<Type> {
        match val {
            Value::Atom(name) => {
                let ty = self.get_var(name)?;
                self.instantiate(level, ty)
            }
            Value::Number(_) => Ok(Type::Const(INT.to_owned())),
            Value::String(_) => Ok(Type::Const(STRING.to_owned())),
            Value::Bool(_) => Ok(Type::Const(BOOL.to_owned())),
            Value::PrimitiveFunc(_) => unreachable!("will never reach"),
            Value::List(vals) => match &vals[..] {
                [Value::Atom(atom), val] if atom == QUOTE => match val {
                    Value::Atom(_) => Ok(Type::Const(SYMBOL.to_owned())),
                    Value::List(vals) => {
                        let mut ty = Type::ListNil;
                        for val in vals.iter().rev() {
                            let head = self.infer(level, val)?;
                            ty = Type::ListCons(Box::new(head), Box::new(ty));
                        }
                        Ok(ty)
                    }
                    Value::DottedList(vals, tail) => {
                        let mut ty = self.infer(level, tail)?;
                        for val in vals.iter().rev() {
                            let head = self.infer(level, val)?;
                            ty = Type::ListCons(Box::new(head), Box::new(ty));
                        }
                        Ok(ty)
                    }
                    Value::Number(_) => Ok(Type::Const("number".to_owned())),
                    Value::String(_) => Ok(Type::Const("string".to_owned())),
                    Value::Bool(_) => Ok(Type::Const("bool".to_owned())),
                    Value::PrimitiveFunc(_) => Ok(Type::Const("primitive-func".to_owned())),
                    Value::Func { .. } => Ok(Type::Const("func".to_owned())),
                    Value::IOFunc(_) => Ok(Type::Const("io-func".to_owned())),
                    Value::Port(_) => Ok(Type::Const("port".to_owned())),
                },
                [Value::Atom(atom), pred, conseq, alt] if atom == "if" => {
                    let pred_ty = self.infer(level, pred)?;
                    self.unify(&pred_ty, &Type::Const(BOOL.to_owned()))?;
                    let conseq_ty = self.infer(level, conseq)?;
                    let alt_ty = self.infer(level, alt)?;
                    self.unify(&conseq_ty, &alt_ty)?;
                    Ok(conseq_ty)
                }
                [Value::Atom(atom), Value::Atom(var), form] if atom == "set!" => {
                    let var_ty = self.infer(level + 1, form)?;
                    let old_ty = self.get_var(var)?;
                    self.unify(&old_ty, &var_ty)?;
                    Ok(var_ty)
                }
                [Value::Atom(atom), Value::Atom(var), form] if atom == "define" => {
                    let var_ty = self.infer(level + 1, form)?;
                    self.generalize(level, &var_ty)?;
                    self.vars.insert(var.clone(), var_ty.clone());
                    Ok(var_ty)
                }
                [Value::Atom(atom), Value::List(name_args), body @ ..] if atom == "define" => {
                    let func_name = match name_args.first() {
                        Some(Value::Atom(name)) => name,
                        other => {
                            return Err(Error::DefineFunctionNotSymbol(
                                other.cloned().unwrap_or(Value::List(vec![])),
                            ));
                        }
                    };
                    let ty = self.define_arrow(level, &name_args[1..], None, body)?;
                    self.vars.insert(func_name.to_owned(), ty.clone());
                    Ok(ty)
                }
                [
                    Value::Atom(atom),
                    Value::DottedList(name_args, vararg),
                    body @ ..,
                ] if atom == "define" => {
                    let func_name = match name_args.first() {
                        Some(Value::Atom(name)) => name,
                        other => {
                            return Err(Error::DefineFunctionNotSymbol(
                                other.cloned().unwrap_or(Value::List(vec![])),
                            ));
                        }
                    };
                    let ty = self.define_arrow(level, &name_args[1..], Some(vararg), body)?;
                    self.vars.insert(func_name.to_owned(), ty.clone());
                    Ok(ty)
                }
                [Value::Atom(atom), Value::List(params), body @ ..] if atom == "lambda" => {
                    self.define_arrow(level, params, None, body)
                }
                [
                    Value::Atom(atom),
                    Value::DottedList(params, vararg),
                    body @ ..,
                ] if atom == "lambda" => self.define_arrow(level, params, Some(vararg), body),
                [Value::Atom(atom), vararg @ Value::Atom(_), body @ ..] if atom == "lambda" => {
                    self.define_arrow(level, &[], Some(vararg), body)
                }
                [Value::Atom(atom), Value::String(path)] if atom == "load" => {
                    let lines = std::fs::read_to_string(path).map_err(|e| Error::IO(e.kind()))?;
                    let vals = parse_multiple(&lines).map_err(|_| Error::Parser)?;
                    let mut ret = None;
                    for val in vals {
                        let body_ty = self.infer(level, &val)?;
                        ret = Some(body_ty);
                    }
                    Ok(ret.unwrap_or(Type::Const("void".to_owned())))
                }
                [func, args @ ..] => {
                    let f_ty = self.infer(level, func)?;
                    let (params, vararg, ret) = self.match_fun_ty(args.len(), f_ty)?;
                    for i in 0..args.len() {
                        let arg = &args[i];
                        let arg_ty = self.infer(level, arg)?;
                        let param = if i < params.len() {
                            &params[i]
                        } else if let Some(vararg) = &vararg {
                            vararg
                        } else {
                            unreachable!("match_fun_ty should've taken care of this")
                        };
                        self.unify(&arg_ty, param)?;
                    }
                    Ok(ret)
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
            Type::Arrow(param, vararg, ret) => {
                self.generalize(level, param)?;
                if let Some(vararg) = vararg {
                    self.generalize(level, vararg)?;
                }
                self.generalize(level, ret)
            }
            Type::ListCons(head, tail) => {
                self.generalize(level, head)?;
                self.generalize(level, tail)
            }
            Type::Const(_) | Type::ListNil => Ok(()),
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
            Type::Arrow(param, vararg, ret) => {
                let param = self.instantiate_impl(id_vars, level, *param)?;
                let vararg = match vararg {
                    Some(vararg) => Some(Box::new(self.instantiate_impl(id_vars, level, *vararg)?)),
                    None => None,
                };
                let ret = self.instantiate_impl(id_vars, level, *ret)?;
                Ok(Type::Arrow(Box::new(param), vararg, Box::new(ret)))
            }
            Type::ListNil => Ok(Type::ListNil),
            Type::ListCons(head, tail) => {
                let head = self.instantiate_impl(id_vars, level, *head)?;
                let tail = self.instantiate_impl(id_vars, level, *tail)?;
                Ok(Type::ListCons(Box::new(head), Box::new(tail)))
            }
        }
    }

    fn match_fun_ty(
        &mut self,
        num_params: usize,
        ty: Type,
    ) -> Result<(Vec<Type>, Option<Type>, Type)> {
        match ty {
            Type::Arrow(param, vararg, ret) => {
                let mut params = Vec::new();
                let mut current = *param;

                loop {
                    match current {
                        Type::ListCons(head, tail) => {
                            params.push(*head);
                            current = *tail;
                        }
                        Type::ListNil => break,
                        _ => {
                            return Err(Error::ExpectedAFunction);
                        }
                    }
                }

                if num_params < params.len() && vararg.is_none() {
                    return Err(Error::UnexpectedNumberOfArguments {
                        expected: params.len(),
                        actual: num_params,
                    });
                }

                Ok((params, vararg.map(|vararg| *vararg), *ret))
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
                        // TODO: This can't be correct to always assume it's not a vararg
                        let vararg: Option<Type> = None;
                        let ret = self.new_unbound_tvar(level);
                        let params_ty = params
                            .clone()
                            .into_iter()
                            .rev()
                            .fold(Type::ListNil, |acc, param| {
                                Type::ListCons(Box::new(param), Box::new(acc))
                            });
                        let ty = Type::Arrow(
                            Box::new(params_ty),
                            vararg.clone().map(Box::new),
                            Box::new(ret.clone()),
                        );
                        self.link(id, ty)?;
                        Ok((params, vararg, ret))
                    }
                    TypeVar::Link(ty) => self.match_fun_ty(num_params, ty),
                    TypeVar::Generic => Err(Error::ExpectedAFunction),
                }
            }
            _ => Err(Error::ExpectedAFunction),
        }
    }

    pub fn replace_ty_constants_with_vars(&mut self, vars: Vec<String>, ty: Type) -> Type {
        if vars.is_empty() {
            return ty;
        }
        let env: HashMap<String, Type> = vars
            .into_iter()
            .map(|name| (name, self.new_generic_tvar()))
            .collect();
        replace_ty_constants_with_vars(&env, ty)
    }

    pub fn ty_to_string(&self, ty: &Type) -> Result<String> {
        let mut namer = Namer::new();
        self.ty_to_string_impl(&mut namer, ty)
    }

    fn ty_to_string_impl(&self, namer: &mut Namer, ty: &Type) -> Result<String> {
        match ty {
            Type::Const(name) => Ok(name.to_string()),
            Type::App(ty, args) => {
                let mut ty_str = self.ty_to_string_impl(namer, ty)?;
                ty_str.push('[');
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        ty_str.push_str(", ")
                    }
                    let arg = self.ty_to_string_impl(namer, arg)?;
                    ty_str.push_str(&arg);
                }
                ty_str.push(']');
                Ok(ty_str)
            }
            Type::Arrow(params, vararg, ret) => {
                let mut ty_str = "(lambda (".to_owned();
                let mut i = 0;
                let mut current = params;
                loop {
                    match current.as_ref() {
                        Type::ListCons(head, tail) => {
                            if i != 0 {
                                ty_str.push(' ');
                            }
                            let head = self.ty_to_string_impl(namer, head)?;
                            ty_str.push_str(&head);
                            current = tail;
                        }
                        Type::ListNil => break,
                        _ => {
                            return Err(Error::ExpectedAFunction);
                        }
                    }
                    i += 1;
                }
                if let Some(vararg) = vararg {
                    ty_str.push_str(" . ");
                    let vararg = self.ty_to_string_impl(namer, vararg)?;
                    ty_str.push_str(&vararg);
                }
                let ret = self.ty_to_string_impl(namer, ret)?;
                ty_str.push_str(") ");
                ty_str.push_str(&ret);
                ty_str.push(')');
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
                    TypeVar::Link(ty) => self.ty_to_string_impl(namer, ty),
                }
            }
            Type::ListNil => Ok("()".to_owned()),
            Type::ListCons(head, tail) => {
                let mut ty_str = "(".to_owned();
                let mut i = 0;
                let mut curr_head = head;
                let mut curr_tail = tail;
                loop {
                    if i != 0 {
                        ty_str.push(' ');
                    }
                    let head = self.ty_to_string_impl(namer, curr_head)?;
                    ty_str.push_str(&head);
                    if self.is_nil(curr_tail) {
                        break;
                    }
                    match curr_tail.as_ref() {
                        Type::ListCons(next_head, next_tail) => {
                            curr_head = next_head;
                            curr_tail = next_tail;
                        }
                        _ => {
                            ty_str.push_str(" . ");
                            let tail = self.ty_to_string_impl(namer, curr_tail)?;
                            ty_str.push_str(&tail);
                            break;
                        }
                    }
                    i += 1;
                }
                ty_str.push(')');
                Ok(ty_str)
            }
        }
    }

    fn is_nil(&self, tail: &Type) -> bool {
        match tail {
            Type::ListNil => true,
            Type::Var(id) => {
                let tvar = self.get_tvar(*id).unwrap();
                match tvar {
                    TypeVar::Link(ty) => self.is_nil(ty),
                    _ => false,
                }
            }
            _ => false,
        }
    }

    pub fn primitive_bindings() -> Self {
        let mut env = Env::default();
        fn define_primitive_func(env: &mut Env, name: &str, func: PrimitiveFunc) {
            let ty = env.infer_primitive_function(&func);
            env.vars.insert(name.to_owned(), ty);
        }
        fn define_io_func(env: &mut Env, name: &str, func: IOFunc) {
            let ty = match func {
                IOFunc::Apply => {
                    // let ret = env.new_generic_tvar();
                    // let args = env.new_generic_tvar();
                    // Type::arrow(
                    //     vec![Type::ListCons(Type::arrow(), Box::new(args))],
                    //     None,
                    //     ret,
                    // )
                    todo!()
                }
                _ => todo!(),
                // IOFunc::MakeReadPort | IOFunc::MakeWritePort => Type::Arrow(
                //     vec![Type::Const(STRING.to_owned())],
                //     None,
                //     Box::new(Type::Const("port".to_owned())),
                // ),
                // IOFunc::ClosePort => Type::Arrow(
                //     vec![Type::Const("port".to_owned())],
                //     None,
                //     Box::new(Type::Const(BOOL.to_owned())),
                // ),
                // IOFunc::Read => Type::Arrow(
                //     vec![Type::Const("port".to_owned())],
                //     None,
                //     Box::new(Type::Const("any".to_owned())),
                // ),
                // IOFunc::Write => Type::Arrow(
                //     vec![Type::Const("any".to_owned()), Type::Const("port".to_owned())],
                //     None,
                //     Box::new(Type::Const("void".to_owned())),
                // ),
                // IOFunc::ReadContents => Type::Arrow(
                //     vec![Type::Const("port".to_owned())],
                //     None,
                //     Box::new(Type::Const(STRING.to_owned())),
                // ),
                // IOFunc::ReadAll => Type::Arrow(
                //     vec![Type::Const("port".to_owned())],
                //     None,
                //     Box::new(Type::ListNil),
                // ),
            };
            env.vars.insert(name.to_owned(), ty);
        }
        define_primitive_func(&mut env, "+", PrimitiveFunc::Add);
        define_primitive_func(&mut env, "-", PrimitiveFunc::Sub);
        define_primitive_func(&mut env, "*", PrimitiveFunc::Mul);
        define_primitive_func(&mut env, "/", PrimitiveFunc::Div);
        define_primitive_func(&mut env, "mod", PrimitiveFunc::Rem);
        define_primitive_func(&mut env, "quotient", PrimitiveFunc::Div);
        define_primitive_func(&mut env, "remainder", PrimitiveFunc::Rem);
        define_primitive_func(&mut env, "=", PrimitiveFunc::Eq);
        define_primitive_func(&mut env, "<", PrimitiveFunc::Lt);
        define_primitive_func(&mut env, ">", PrimitiveFunc::Gt);
        define_primitive_func(&mut env, "/=", PrimitiveFunc::Ne);
        define_primitive_func(&mut env, ">=", PrimitiveFunc::Ge);
        define_primitive_func(&mut env, "<=", PrimitiveFunc::Le);
        define_primitive_func(&mut env, "&&", PrimitiveFunc::And);
        define_primitive_func(&mut env, "||", PrimitiveFunc::Or);
        define_primitive_func(&mut env, "string=?", PrimitiveFunc::StringEq);
        define_primitive_func(&mut env, "string<?", PrimitiveFunc::StringLt);
        define_primitive_func(&mut env, "string>?", PrimitiveFunc::StringGt);
        define_primitive_func(&mut env, "string<=?", PrimitiveFunc::StringLe);
        define_primitive_func(&mut env, "string>=?", PrimitiveFunc::StringGe);
        define_primitive_func(&mut env, "car", PrimitiveFunc::Car);
        define_primitive_func(&mut env, "cdr", PrimitiveFunc::Cdr);
        define_primitive_func(&mut env, "cons", PrimitiveFunc::Cons);
        define_primitive_func(&mut env, "eq?", PrimitiveFunc::Eqv);
        define_primitive_func(&mut env, "eqv?", PrimitiveFunc::Eqv);
        define_primitive_func(&mut env, "equal?", PrimitiveFunc::Equal);
        // define_io_func(&mut env, "apply", IOFunc::Apply);
        // define_io_func(&mut env, "open-input-file", IOFunc::MakeReadPort);
        // define_io_func(&mut env, "open-output-file", IOFunc::MakeWritePort);
        // define_io_func(&mut env, "close-input-port", IOFunc::ClosePort);
        // define_io_func(&mut env, "close-output-port", IOFunc::ClosePort);
        // define_io_func(&mut env, "read", IOFunc::Read);
        // define_io_func(&mut env, "write", IOFunc::Write);
        // define_io_func(&mut env, "read-contents", IOFunc::ReadContents);
        // define_io_func(&mut env, "read-all", IOFunc::ReadAll);
        env
    }

    fn infer_primitive_function(&mut self, f: &PrimitiveFunc) -> Type {
        match f {
            PrimitiveFunc::Add
            | PrimitiveFunc::Sub
            | PrimitiveFunc::Mul
            | PrimitiveFunc::Div
            | PrimitiveFunc::Rem => Type::arrow(vec![], Some(Type::int()), Type::int()),
            PrimitiveFunc::Eq
            | PrimitiveFunc::Lt
            | PrimitiveFunc::Gt
            | PrimitiveFunc::Ne
            | PrimitiveFunc::Ge
            | PrimitiveFunc::Le => Type::arrow(vec![], Some(Type::int()), Type::bool()),
            PrimitiveFunc::And | PrimitiveFunc::Or => {
                Type::arrow(vec![], Some(Type::bool()), Type::bool())
            }
            PrimitiveFunc::StringEq
            | PrimitiveFunc::StringLt
            | PrimitiveFunc::StringGt
            | PrimitiveFunc::StringLe
            | PrimitiveFunc::StringGe => Type::arrow(vec![], Some(Type::string()), Type::bool()),
            PrimitiveFunc::Car => {
                let head = self.new_generic_tvar();
                let tail = self.new_generic_tvar();
                Type::arrow(
                    vec![Type::ListCons(Box::new(head.clone()), Box::new(tail))],
                    None,
                    head,
                )
            }
            PrimitiveFunc::Cdr => {
                let head = self.new_generic_tvar();
                let tail = self.new_generic_tvar();
                Type::arrow(
                    vec![Type::ListCons(Box::new(head), Box::new(tail.clone()))],
                    None,
                    tail,
                )
            }
            PrimitiveFunc::Cons => {
                let a = self.new_generic_tvar();
                let b = self.new_generic_tvar();
                Type::arrow(
                    vec![a.clone(), b.clone()],
                    None,
                    Type::ListCons(Box::new(a), Box::new(b)),
                )
            }
            PrimitiveFunc::Eqv => {
                let a = self.new_generic_tvar();
                Type::arrow(vec![a.clone(), a], None, Type::bool())
            }
            PrimitiveFunc::Equal => {
                let a = self.new_generic_tvar();
                Type::arrow(vec![a.clone(), a], None, Type::bool())
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
    use crate::{
        infer::{BOOL, Env, INT, STRING},
        parser::parse,
        typing::Type,
    };

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

    fn run(input: &str, expected: &str, mut env: Env) {
        let (vars, ty) = crate::typing::parse(expected).unwrap();
        let expected = env.replace_ty_constants_with_vars(vars, ty);
        let value = parse(input).unwrap();
        let actual = env
            .infer_value(&value)
            .expect(&format!("input: {}, value: {:?}", input, value));
        env.generalize(-1, &actual).unwrap();
        let expected = env.ty_to_string(&expected).unwrap();
        let actual = env.ty_to_string(&actual).unwrap();
        assert_eq!(actual, expected, "input: {}, value: {}", input, value);
    }

    #[test]
    fn cdr_test() {
        let env = Env::primitive_bindings();
        run("(cdr '(1 2 3))", "(int int)", env);
    }

    #[test]
    fn infer() {
        let cases: Vec<(&str, &str)> = vec![
            ("'atom", "symbol"),
            ("2", "int"),
            ("\"a string\"", "string"),
            ("(+ 2 2)", "int"),
            ("(+ 2 (- 4 1))", "int"),
            ("(- (+ 4 6 3) 3 5 2)", "int"),
            ("(< 2 3)", "bool"),
            ("(> 2 3)", "bool"),
            ("(>= 3 3)", "bool"),
            ("(string=? \"test\" \"test\")", "bool"),
            ("(string<? \"abc\" \"bba\")", "bool"),
            ("(if (> 2 3) \"no\" \"yes\")", "string"),
            ("'()", "()"),
            ("'(1 2)", "(int int)"),
            ("(car '(1 2))", "int"),
            ("(cdr '(1 2))", "(int)"),
            ("'(\"a\" 1 #t)", "(string int bool)"),
            ("(car '(\"a\" 1 #t))", "string"),
            ("(cdr '(\"a\" 1 #t))", "(int bool)"),
            // ("(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")", Ok("9")),
            // ("(cdr '(a simple test))", Ok("(simple test)")),
            // ("(car (cdr '(a simple test)))", Ok("simple")),
            // ("(car '((this is) a test))", Ok("(this is)")),
            // ("(cons '(this is) 'test)", Ok("((this is) . test)")),
            // ("(cons '(this is) '())", Ok("((this is))")),
            // ("(eqv? 1 3)", Ok("#f")),
            // ("(eqv? 3 3)", Ok("#t")),
            // ("(eqv? 'atom 'atom)", Ok("#t")),
            // ("(define x 3)", Ok("3")),
            // ("(+ x 2)", Ok("5")),
            // (
            //     "(+ y 2)",
            //     Err(Error::UnboundVar(
            //         "Getting an unbound variable".to_owned(),
            //         "y".to_owned(),
            //     )),
            // ),
            // ("(define y 5)", Ok("5")),
            // ("(+ x (- y 2))", Ok("6")),
            // ("(define str \"A string\")", Ok("\"A string\"")),
            // (
            //     "(< str \"The string\")",
            //     Err(Error::TypeMismatch(
            //         "number".to_owned(),
            //         Value::String("A string".to_owned()),
            //     )),
            // ),
            // ("(string<? str \"The string\")", Ok("#t")),
            // ("(define (f x y) (+ x y))", Ok("(lambda (x y) ...)")),
            // ("(f 1 2)", Ok("3")),
            // (
            //     "(f 1 2 3)",
            //     Err(Error::NumArgs(
            //         2,
            //         vec![Value::Number(1), Value::Number(2), Value::Number(3)],
            //     )),
            // ),
            // ("(f 1)", Err(Error::NumArgs(2, vec![Value::Number(1)]))),
            // (
            //     "(define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))",
            //     Ok("(lambda (x) ...)"),
            // ),
            // ("(factorial 10)", Ok("3628800")),
            // (
            //     "(define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))",
            //     Ok("(lambda (inc) ...)"),
            // ),
            // ("(define my-count (counter 5))", Ok("(lambda (x) ...)")),
            // ("(my-count 3)", Ok("8")),
            // ("(my-count 6)", Ok("14")),
            // ("(my-count 5)", Ok("19")),
            // ("(load \"stdlib.scm\")", Ok("(lambda (pred lst) ...)")),
            // ("(map (curry + 2) '(1 2 3 4))", Ok("(3 4 5 6)")),
            // ("(filter even? '(1 2 3 4))", Ok("(2 4)")),
        ];
        let env = Env::primitive_bindings();
        for (input, expected) in cases {
            let env = env.clone();
            run(input, expected, env);
        }
    }
}
