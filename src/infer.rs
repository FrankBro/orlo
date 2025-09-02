use std::{collections::HashMap, f64::consts::TAU};

use crate::{
    typing::{Id, Level, Type, TypeVar, replace_ty_constants_with_vars},
    value::{PrimitiveFunc, QUOTE, Value},
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
            Type::Arrow(params, vararg, ret) => {
                for param in params {
                    self.occurs_check_adjust_levels(tvar_id, tvar_level, param)?;
                }
                if let Some(vararg) = vararg {
                    self.occurs_check_adjust_levels(tvar_id, tvar_level, vararg)?;
                }
                self.occurs_check_adjust_levels(tvar_id, tvar_level, ret)
            }
            Type::ListExtend(heads, tail) => {
                for head in heads {
                    self.occurs_check_adjust_levels(tvar_id, tvar_level, head)?;
                }
                self.occurs_check_adjust_levels(tvar_id, tvar_level, tail)
            }
            Type::Const(_) | Type::ListNil => Ok(()),
        }
    }

    fn match_list_ty(&self, ty: &Type) -> Result<(Vec<Type>, Box<Type>)> {
        match ty {
            Type::ListExtend(heads, tail) => {
                let mut heads = heads.clone();
                let (tail_heads, tail) = self.match_list_ty(&tail)?;
                for head in tail_heads {
                    heads.push(head);
                }
                Ok((heads, tail))
            }
            Type::Var(id) => {
                let tvar = self.get_tvar(*id)?;
                match tvar {
                    TypeVar::Link(ty) => self.match_list_ty(&ty),
                    _ => Ok((Vec::new(), Box::new(ty.clone()))),
                }
            }
            Type::ListNil => Ok((vec![], Box::new(Type::ListNil))),
            _ => Err(Error::CannotUnifyList(ty.clone(), ty.clone())),
        }
    }

    fn unify_list(&mut self, ty1: &Type, ty2: &Type) -> Result<()> {
        let (heads1, tail1) = self.match_list_ty(ty1)?;
        let (heads2, tail2) = self.match_list_ty(ty2)?;

        if heads1.is_empty() && heads2.is_empty() {
            return self.unify(&tail1, &tail2);
        }

        for i in 0..std::cmp::min(heads1.len(), heads2.len()) {
            let head1 = &heads1[i];
            let head2 = &heads2[i];
            self.unify(head1, head2)?;
        }

        if heads1.len() > heads2.len() {
            self.unify(&ty1, &Type::ListExtend(heads1, tail2))
        } else if heads2.len() > heads1.len() {
            self.unify(&ty2, &Type::ListExtend(heads2, tail1))
        } else {
            Ok(())
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
            (Type::Arrow(params1, vararg1, ret1), Type::Arrow(params2, vararg2, ret2)) => {
                if params1.len() != params2.len() {
                    return Err(Error::CannotUnify(ty1.clone(), ty2.clone()));
                }
                for i in 0..params1.len() {
                    let param1 = &params1[i];
                    let param2 = &params2[i];
                    self.unify(param1, param2)?;
                }
                match (vararg1, vararg2) {
                    (Some(vararg1), Some(vararg2)) => self.unify(vararg1, vararg2)?,
                    _ => {
                        return Err(Error::CannotUnify(ty1.clone(), ty2.clone()));
                    }
                }
                self.unify(ret1, ret2)
            }
            (Type::ListNil, Type::ListNil) => Ok(()),
            (Type::ListExtend(_, _), Type::ListExtend(_, _)) => self.unify_list(ty1, ty2),
            (Type::ListExtend(heads, _), Type::ListNil)
            | (Type::ListNil, Type::ListExtend(heads, _)) => {
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

    fn infer(&mut self, level: Level, val: &Value) -> Result<Type> {
        match val {
            Value::Atom(name) => {
                let ty = self.get_var(name)?;
                self.instantiate(level, ty)
            }
            Value::Number(_) => Ok(Type::Const(INT.to_owned())),
            Value::String(_) => Ok(Type::Const(STRING.to_owned())),
            Value::Bool(_) => Ok(Type::Const(BOOL.to_owned())),
            Value::PrimitiveFunc(f) => unreachable!("will never reach"),
            Value::List(vals) => match &vals[..] {
                [Value::Atom(atom), val] if atom == QUOTE => match val {
                    Value::Atom(_) => Ok(Type::Const(SYMBOL.to_owned())),
                    Value::List(vals) => {
                        let mut heads = Vec::with_capacity(vals.len());
                        for val in vals {
                            let head_ty = self.infer(level, val)?;
                            heads.push(head_ty);
                        }
                        Ok(Type::ListExtend(heads, Box::new(Type::ListNil)))
                    }
                    Value::DottedList(heads, tail) => {
                        let mut head_tys = Vec::with_capacity(heads.len());
                        for head in heads {
                            let head_ty = self.infer(level, head)?;
                            head_tys.push(head_ty);
                        }
                        let tail_ty = self.infer(level, tail)?;
                        Ok(Type::ListExtend(head_tys, Box::new(tail_ty)))
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
                [Value::Atom(atom), Value::Atom(var), form] if atom == "set!" => todo!(),
                [Value::Atom(atom), Value::Atom(var), form] if atom == "define" => {
                    let var_ty = self.infer(level + 1, form)?;
                    self.generalize(level, &var_ty)?;
                    self.vars.insert(var.clone(), var_ty.clone());
                    Ok(var_ty)
                }
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
            Type::Arrow(params, vararg, ret) => {
                for param in params {
                    self.generalize(level, param)?;
                }
                if let Some(vararg) = vararg {
                    self.generalize(level, vararg)?;
                }
                self.generalize(level, ret)
            }
            Type::ListExtend(heads, tail) => {
                for head in heads {
                    self.generalize(level, head)?;
                }
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
            Type::Arrow(params, vararg, ret) => {
                let instantiated_ret = self.instantiate_impl(id_vars, level, *ret)?;
                let mut instantiated_params = Vec::with_capacity(params.len());
                for param in params {
                    let instantiated_param = self.instantiate_impl(id_vars, level, param)?;
                    instantiated_params.push(instantiated_param);
                }
                let instantiated_vararg = match vararg {
                    Some(vararg) => Some(Box::new(self.instantiate_impl(id_vars, level, *vararg)?)),
                    None => None,
                };
                Ok(Type::Arrow(
                    instantiated_params,
                    instantiated_vararg,
                    Box::new(instantiated_ret),
                ))
            }
            Type::ListNil => Ok(Type::ListNil),
            Type::ListExtend(heads, tail) => {
                let mut instantiated_heads = Vec::with_capacity(heads.len());
                for head in heads {
                    let instantiated_head = self.instantiate_impl(id_vars, level, head)?;
                    instantiated_heads.push(instantiated_head);
                }
                let tail = self.instantiate_impl(id_vars, level, *tail)?;
                Ok(Type::ListExtend(instantiated_heads, Box::new(tail)))
            }
        }
    }

    fn match_fun_ty(
        &mut self,
        num_params: usize,
        ty: Type,
    ) -> Result<(Vec<Type>, Option<Box<Type>>, Box<Type>)> {
        match ty {
            Type::Arrow(params, vararg, ret) => {
                if num_params < params.len() {
                    return Err(Error::UnexpectedNumberOfArguments {
                        expected: params.len(),
                        actual: num_params,
                    });
                }
                Ok((params, vararg, ret))
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
                        let vararg = None;
                        let ret = Box::new(self.new_unbound_tvar(level));
                        let ty = Type::Arrow(params.clone(), vararg.clone(), ret.clone());
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
                let mut ty_str = "(λ".to_owned();
                for (i, param) in params.iter().enumerate() {
                    ty_str.push_str(" ");
                    let param = self.ty_to_string_impl(namer, param)?;
                    ty_str.push_str(&param);
                }
                if let Some(vararg) = vararg {
                    ty_str.push_str(" . ");
                    let vararg = self.ty_to_string_impl(namer, vararg)?;
                    ty_str.push_str(&vararg);
                }
                let ret = self.ty_to_string_impl(namer, ret)?;
                ty_str.push_str(" -> ");
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
            Type::ListExtend(heads, tail) => {
                let mut ty_str = "(".to_owned();
                for (i, head) in heads.iter().enumerate() {
                    if i != 0 {
                        ty_str.push_str(" ");
                    }
                    let head = self.ty_to_string_impl(namer, head)?;
                    ty_str.push_str(&head);
                }
                if tail.as_ref() == &Type::ListNil {
                    ty_str.push(')');
                    return Ok(ty_str);
                }
                ty_str.push_str(" . ");
                let tail = self.ty_to_string_impl(namer, tail)?;
                ty_str.push_str(&tail);
                ty_str.push(')');
                Ok(ty_str)
            }
        }
    }

    pub fn primitive_bindings() -> Self {
        let mut env = Env::default();
        fn define_primitive_func(env: &mut Env, name: &str, func: PrimitiveFunc) {
            let ty = env.infer_primitive_function(&func);
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
        // define_primitive_func(&mut env, "eq?", PrimitiveFunc::Eqv);
        // define_primitive_func(&mut env, "eqv?", PrimitiveFunc::Eqv);
        // define_primitive_func(&mut env, "equal?", PrimitiveFunc::Equal);
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
            | PrimitiveFunc::Rem => Type::Arrow(
                vec![],
                Some(Box::new(Type::Const(INT.to_owned()))),
                Box::new(Type::Const(INT.to_owned())),
            ),
            PrimitiveFunc::Eq
            | PrimitiveFunc::Lt
            | PrimitiveFunc::Gt
            | PrimitiveFunc::Ne
            | PrimitiveFunc::Ge
            | PrimitiveFunc::Le => Type::Arrow(
                vec![],
                Some(Box::new(Type::Const(INT.to_owned()))),
                Box::new(Type::Const(BOOL.to_owned())),
            ),
            PrimitiveFunc::And | PrimitiveFunc::Or => Type::Arrow(
                vec![],
                Some(Box::new(Type::Const(BOOL.to_owned()))),
                Box::new(Type::Const(BOOL.to_owned())),
            ),
            PrimitiveFunc::StringEq
            | PrimitiveFunc::StringLt
            | PrimitiveFunc::StringGt
            | PrimitiveFunc::StringLe
            | PrimitiveFunc::StringGe => Type::Arrow(
                vec![],
                Some(Box::new(Type::Const(STRING.to_owned()))),
                Box::new(Type::Const(BOOL.to_owned())),
            ),
            PrimitiveFunc::Car => {
                let a = self.new_generic_tvar();
                Type::Arrow(
                    vec![Type::ListExtend(vec![a.clone()], Box::new(Type::Var(1)))],
                    None,
                    Box::new(a),
                )
            }
            PrimitiveFunc::Cdr => {
                let a = self.new_generic_tvar();
                let b = self.new_generic_tvar();
                Type::Arrow(
                    vec![Type::ListExtend(vec![a.clone()], Box::new(b.clone()))],
                    None,
                    Box::new(Type::ListExtend(vec![], Box::new(b))),
                )
            }
            PrimitiveFunc::Cons => {
                let a = self.new_generic_tvar();
                let b = self.new_generic_tvar();
                Type::Arrow(
                    vec![a.clone(), Type::ListExtend(vec![], Box::new(b.clone()))],
                    None,
                    Box::new(Type::ListExtend(vec![a], Box::new(b))),
                )
            }
            PrimitiveFunc::Eqv => todo!(),
            PrimitiveFunc::Equal => todo!(),
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
            let (vars, ty) = crate::typing::parse(expected).unwrap();
            let mut env = env.clone();
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
    }
}
