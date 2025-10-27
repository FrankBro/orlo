use std::collections::HashMap;

use crate::{
    identifier::{self, Access, Form, identify},
    parser::parse_multiple,
    typing::{Id, Level, Type, TypeVar, replace_ty_constants_with_vars},
    value::{IOFunc, PrimitiveFunc, Value},
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
    ExpectedAFunction(Type),
    RecursiveType,
    CannotUnify(Type, Type),
    FunctionArgNotSymbol(String),
    IO(std::io::ErrorKind),
    Parser,
    DefineMacroNotSymbol(Value),
    DefineFunctionNotSymbol(Value),
    MissingLabel(String),
    CannotInjectConstraintsInto(Type),
    ExpectedARow(Type),
    RowConstraintFailed(String),
    RecursiveRowType,
    NoSuchField(String, Value),
    Form(identifier::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::TypeVarNotFound(_) => todo!(),
            Error::VarNotFound(var) => write!(f, "Unbound variable: {var}"),
            Error::BadSpecialForm(text, _value) => write!(f, "{text}"),
            Error::UnexpectedNumberOfArguments { .. } => todo!(),
            Error::ExpectedAFunction(_) => todo!(),
            Error::RecursiveType => todo!(),
            Error::CannotUnify(_, _) => todo!(),
            Error::FunctionArgNotSymbol(_) => todo!(),
            Error::IO(_error_kind) => todo!(),
            Error::Parser => todo!(),
            Error::DefineMacroNotSymbol(_value) => todo!(),
            Error::DefineFunctionNotSymbol(_value) => todo!(),
            Error::MissingLabel(_) => todo!(),
            Error::CannotInjectConstraintsInto(_) => todo!(),
            Error::ExpectedARow(_) => todo!(),
            Error::RowConstraintFailed(_) => todo!(),
            Error::RecursiveRowType => todo!(),
            Error::NoSuchField(_, _value) => todo!(),
            Error::Form(_error) => todo!(),
        }
    }
}

impl From<identifier::Error> for Error {
    fn from(error: identifier::Error) -> Self {
        Error::Form(error)
    }
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

    fn new_weak_tvar(&mut self, level: Level) -> Type {
        let id = self.tvars.len();
        self.tvars.push(TypeVar::Weak(level));
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

    fn prune_type(&self, ty: &Type) -> Result<Type> {
        match ty {
            Type::Var(id) => match self.get_tvar(*id)? {
                TypeVar::Link(ty) => self.prune_type(ty),
                _ => Ok(ty.clone()),
            },
            _ => Ok(ty.clone()),
        }
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
                    TypeVar::Weak(other_level) => {
                        if *other_id == tvar_id {
                            Err(Error::RecursiveType)
                        } else {
                            if other_level > tvar_level {
                                *other_tvar = TypeVar::Weak(tvar_level);
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
            Type::Arrow(param, ret) => {
                self.occurs_check_adjust_levels(tvar_id, tvar_level, param)?;
                self.occurs_check_adjust_levels(tvar_id, tvar_level, ret)
            }
            Type::ListCons(head, tail) => {
                self.occurs_check_adjust_levels(tvar_id, tvar_level, head)?;
                self.occurs_check_adjust_levels(tvar_id, tvar_level, tail)
            }
            Type::ListVarArg(vararg) => {
                self.occurs_check_adjust_levels(tvar_id, tvar_level, vararg)
            }
            Type::Array(ty) => self.occurs_check_adjust_levels(tvar_id, tvar_level, ty),
            Type::Record(row) => self.occurs_check_adjust_levels(tvar_id, tvar_level, row),
            Type::Variant(row) => self.occurs_check_adjust_levels(tvar_id, tvar_level, row),
            Type::RowExtend(labels, rest) => {
                for (_label, ty) in labels {
                    self.occurs_check_adjust_levels(tvar_id, tvar_level, ty)?;
                }
                self.occurs_check_adjust_levels(tvar_id, tvar_level, rest)
            }
            Type::Const(_) | Type::ListNil | Type::RowEmpty => Ok(()),
        }
    }

    fn match_row_ty(&self, ty: &Type) -> Result<(Vec<(String, Type)>, Type)> {
        match ty {
            Type::RowExtend(labels, rest) => {
                let mut labels = labels.clone();
                let (mut rest_labels, rest) = self.match_row_ty(&rest)?;
                labels.append(&mut rest_labels);
                Ok((labels, rest))
            }
            Type::Var(id) => {
                let tvar = self.get_tvar(*id)?;
                match tvar {
                    TypeVar::Link(ty) => self.match_row_ty(ty),
                    _ => Ok((vec![], ty.clone())),
                }
            }
            Type::RowEmpty => Ok((vec![], Type::RowEmpty)),
            _ => Err(Error::ExpectedARow(ty.clone())),
        }
    }

    fn unify(&mut self, ty1: &Type, ty2: &Type) -> Result<()> {
        if ty1 == ty2 {
            return Ok(());
        }
        match (ty1, ty2) {
            (Type::Const(name1), Type::Const(name2)) if name1 == name2 => Ok(()),
            (Type::Array(ty1), Type::Array(ty2)) => self.unify(ty1, ty2),
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
            (Type::Arrow(param1, ret1), Type::Arrow(param2, ret2)) => {
                self.unify(param1, param2)?;
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
                    TypeVar::Weak(level) => {
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
                    TypeVar::Weak(level) => {
                        self.occurs_check_adjust_levels(*id, level, ty1)?;
                        self.link(*id, ty1.clone())
                    }
                    TypeVar::Link(ty2) => self.unify(ty1, &ty2),
                    TypeVar::Generic => Err(Error::CannotUnify(ty1.clone(), ty2.clone())),
                }
            }
            (Type::ListNil, Type::ListNil)
            | (Type::ListVarArg(_), Type::ListNil)
            | (Type::ListNil, Type::ListVarArg(_)) => Ok(()),
            (Type::ListCons(head1, tail1), Type::ListCons(head2, tail2)) => {
                self.unify(head1, head2)?;
                self.unify(tail1, tail2)
            }
            (Type::ListCons(head, tail), Type::ListVarArg(vararg))
            | (Type::ListVarArg(vararg), Type::ListCons(head, tail)) => {
                self.unify(head, vararg)?;
                if tail.as_ref() == &Type::ListNil {
                    return Ok(());
                }
                self.unify(tail, &Type::ListVarArg(vararg.clone()))
            }
            (Type::ListVarArg(vararg), ty) | (ty, Type::ListVarArg(vararg)) => {
                self.unify(vararg, ty)
            }
            (Type::Record(row1), Type::Record(row2)) => self.unify(row1, row2),
            (Type::Variant(row1), Type::Variant(row2)) => self.unify(row1, row2),
            (Type::RowEmpty, Type::RowEmpty) => Ok(()),
            (Type::RowExtend(_, _), Type::RowExtend(_, _)) => self.unify_rows(ty1, ty2),
            (Type::RowExtend(labels, _), Type::RowEmpty)
            | (Type::RowEmpty, Type::RowExtend(labels, _)) => Err(Error::MissingLabel(
                labels.first().map(|(label, _)| label.clone()).unwrap(),
            )),
            _ => Err(Error::CannotUnify(ty1.clone(), ty2.clone())),
        }
    }

    fn find_missing(
        &mut self,
        labels1: &Vec<(String, Type)>,
        labels2: &Vec<(String, Type)>,
    ) -> Result<Vec<(String, Type)>> {
        let mut missing = Vec::new();
        for (label1, ty1) in labels1 {
            if let Some((_, ty2)) = labels2.iter().find(|(label2, _)| label1 == label2) {
                self.unify(ty1, ty2)?;
            } else {
                missing.push((label1.clone(), ty1.clone()));
            }
        }
        Ok(missing)
    }

    fn unify_rows(&mut self, row1: &Type, row2: &Type) -> Result<()> {
        let (labels1, rest1) = self.match_row_ty(row1)?;
        let (labels2, rest2) = self.match_row_ty(row2)?;

        let missing1 = self.find_missing(&labels2, &labels1)?;
        let missing2 = self.find_missing(&labels1, &labels2)?;

        match (missing1.is_empty(), missing2.is_empty()) {
            (true, true) => self.unify(&rest1, &rest2),
            (true, false) => self.unify(&rest2, &Type::RowExtend(missing2, rest1.into())),
            (false, true) => self.unify(&rest1, &Type::RowExtend(missing1, rest2.into())),
            (false, false) => match rest1 {
                Type::RowEmpty => {
                    // TODO: not row? also, level 0?
                    let tvar = self.new_unbound_tvar(0);
                    self.unify(&rest1, &Type::RowExtend(missing1, tvar.into()))
                }
                Type::Var(id) => {
                    let tvar = self.get_tvar(id)?;
                    match tvar.clone() {
                        TypeVar::Unbound(level) => {
                            let rest = self.new_unbound_tvar(level);
                            self.unify(&rest2, &Type::RowExtend(missing2, rest.clone().into()))?;
                            if let TypeVar::Link(_) = self.get_tvar(id)? {
                                return Err(Error::RecursiveRowType);
                            }
                            self.unify(&rest1, &Type::RowExtend(missing1, rest.into()))
                        }
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            },
        }
    }

    fn define_arrow(
        &mut self,
        level: Level,
        params: Vec<&str>,
        vararg: Option<&str>,
        body: &[Value],
    ) -> Result<Type> {
        let mut param_tys = Vec::with_capacity(params.len());
        let old_vars = self.vars.clone();
        for param in params {
            let param_ty = self.new_unbound_tvar(level);
            self.vars.insert(param.to_string(), param_ty.clone());
            param_tys.push(param_ty);
        }
        let vararg_ty = match vararg.as_ref() {
            Some(name) => {
                let vararg_ty = self.new_unbound_tvar(level);
                self.vars.insert(name.to_string(), vararg_ty.clone());
                Some(vararg_ty)
            }
            None => None,
        };
        let mut ret_ty = Type::Const("void".to_owned());
        for val in body {
            let body_ty = self.infer(level, val)?;
            ret_ty = body_ty;
        }
        self.vars = old_vars;
        let init = if let Some(vararg) = vararg_ty {
            Type::ListVarArg(Box::new(vararg))
        } else {
            Type::ListNil
        };
        let params = param_tys.into_iter().rev().fold(init, |acc, param| {
            Type::ListCons(Box::new(param), Box::new(acc))
        });
        Ok(Type::Arrow(Box::new(params), Box::new(ret_ty)))
    }

    fn infer_array(&mut self, level: Level, vals: &Vec<Value>) -> Result<Type> {
        let mut ty = None;
        for val in vals {
            let val_ty = self.infer(level, val)?;
            match ty {
                Some(ref existing_ty) => {
                    self.unify(existing_ty, &val_ty)?;
                }
                None => {
                    ty = Some(val_ty);
                }
            }
        }
        let ty = ty.unwrap_or_else(|| self.new_weak_tvar(level));
        Ok(Type::Array(Box::new(ty)))
    }

    fn infer_record(
        &mut self,
        level: Level,
        labels: &Vec<(String, Value)>,
        rest: Option<&Value>,
    ) -> Result<Type> {
        let rest = match rest {
            Some(rest) => match self.infer(level, rest)? {
                Type::Record(row) => *row,
                ty => {
                    return Err(Error::BadSpecialForm(
                        format!("rest of dotted record must be a record: {:?}", ty),
                        rest.clone(),
                    ));
                }
            },
            None => Type::RowEmpty,
        };
        let mut label_tys = Vec::with_capacity(labels.len());
        for (label, val) in labels {
            let val_ty = self.infer(level, val)?;
            label_tys.push((label.clone(), val_ty));
        }
        Ok(Type::Record(
            Type::RowExtend(label_tys, Box::new(rest)).into(),
        ))
    }

    fn infer_variant(&mut self, level: Level, label: &str, val: &Value) -> Result<Type> {
        let rest = self.new_unbound_tvar(level);
        let variant = self.new_unbound_tvar(level);
        let ret = Type::Variant(
            Type::RowExtend(vec![(label.to_owned(), variant.clone())], rest.into()).into(),
        );
        let val = self.infer(level, val)?;
        self.unify(&variant, &val)?;
        Ok(ret)
    }

    fn infer(&mut self, level: Level, val: &Value) -> Result<Type> {
        match val {
            Value::Datum(_) => Ok(Type::Const("datum".to_owned())),
            Value::Atom(name) => {
                let ty = self.get_var(name)?;
                self.instantiate(level, ty)
            }
            Value::Number(_) => Ok(Type::Const(INT.to_owned())),
            Value::String(_) => Ok(Type::Const(STRING.to_owned())),
            Value::Bool(_) => Ok(Type::Const(BOOL.to_owned())),
            Value::PrimitiveFunc(_) => unreachable!("will never reach"),
            Value::Array(vals) => self.infer_array(level, vals),
            Value::Record(vals) => self.infer_record(level, vals, None),
            Value::DottedRecord(vals, rest) => self.infer_record(level, vals, Some(rest)),
            Value::Variant(label, val) => self.infer_variant(level, label, val),
            Value::DottedVariant(_, _) => unreachable!(),
            Value::List(vals) => match identify(vals)? {
                Form::The(expected, val) => {
                    let val_ty = self.infer(level, val)?;
                    let expected = expected.to_string();
                    // replace unbound and weak type vars
                    let actual = self
                        .ty_to_string(&val_ty)?
                        .replace("_", "")
                        .replace("~", "");
                    if expected != actual {
                        return Err(Error::BadSpecialForm(
                            format!(
                                "the expected type '{}' does not match the inferred type '{}'",
                                expected, actual
                            ),
                            val.clone(),
                        ));
                    }
                    Ok(val_ty)
                }
                Form::Append(vals) => {
                    let mut ty = Type::ListNil;

                    fn append_list(acc: Type, ty: Type) -> Result<Type> {
                        match ty {
                            Type::ListCons(head, tail) => {
                                let acc = append_list(acc, *tail)?;
                                Ok(Type::ListCons(head, Box::new(acc)))
                            }
                            Type::ListNil => Ok(acc),
                            other => Err(Error::BadSpecialForm(
                                "append requires list arguments".to_owned(),
                                Value::Atom(format!("{:?}", other)),
                            )),
                        }
                    }

                    for val in vals.iter().rev() {
                        let arg_ty = self.infer(level, val)?;
                        let arg_ty = self.prune_type(&arg_ty)?;
                        ty = append_list(ty, arg_ty)?;
                    }
                    Ok(ty)
                }
                Form::List(vals) => {
                    let mut ty = Type::ListNil;
                    for val in vals.iter().rev() {
                        let head = self.infer(level, val)?;
                        ty = Type::ListCons(Box::new(head), Box::new(ty));
                    }
                    Ok(ty)
                }
                Form::Access(container, accesses) => {
                    let mut ty = self.infer(level, container)?;
                    for access in accesses {
                        match access {
                            Access::Index(_) => {
                                let value = self.new_unbound_tvar(level);
                                let arr = Type::Array(Box::new(value.clone()));
                                self.unify(&arr, &ty)?;
                                ty = value;
                            }
                            Access::Field(label) => {
                                let rest = self.new_unbound_tvar(level);
                                let value = self.new_unbound_tvar(level);
                                let rec = Type::Record(
                                    Type::RowExtend(
                                        vec![(label.to_owned(), value.clone())],
                                        Box::new(rest),
                                    )
                                    .into(),
                                );
                                self.unify(&rec, &ty)?;
                                ty = value;
                            }
                        }
                    }
                    Ok(ty.clone())
                }
                Form::If {
                    cond,
                    then_branch,
                    else_branch,
                } => {
                    let cond = self.infer(level, cond)?;
                    self.unify(&cond, &Type::Const(BOOL.to_owned()))?;
                    let then_branch = self.infer(level, then_branch)?;
                    let else_branch = self.infer(level, else_branch)?;
                    self.unify(&then_branch, &else_branch)?;
                    Ok(then_branch)
                }
                Form::Set { var, val } => {
                    let val = self.infer(level, val)?;
                    let var = self.get_var(var)?;
                    self.unify(&var, &val)?;
                    Ok(var)
                }
                Form::Push { var, val } => {
                    let name = var;
                    let val = self.infer(level, val)?;
                    let var = self.get_var(var)?;

                    match self.prune_type(&var)? {
                        Type::Array(elem) => {
                            self.unify(&elem, &val)?;
                        }
                        _ => {
                            return Err(Error::BadSpecialForm(
                                "push! requires an array variable".to_owned(),
                                Value::Atom(name.to_owned()),
                            ));
                        }
                    }
                    Ok(var)
                }
                Form::DefineValue { name, val } => {
                    let val = self.infer(level + 1, val)?;
                    self.generalize(level, &val)?;
                    self.vars.insert(name.to_owned(), val.clone());
                    Ok(val)
                }
                Form::DefineFunction {
                    name,
                    params,
                    vararg,
                    body,
                } => {
                    let ty = self.define_arrow(level, params, vararg, body)?;
                    self.generalize(level, &ty)?;
                    self.vars.insert(name.to_owned(), ty.clone());
                    Ok(ty)
                }
                Form::Quote(val) => match val {
                    Value::Atom(_) => Ok(Type::Const(SYMBOL.to_owned())),
                    Value::List(vals) => {
                        let mut ty = Type::ListNil;
                        for val in vals.iter().rev() {
                            let head = match val {
                                // Atoms in quoted lists are symbols, not variables to look up
                                Value::Atom(_) => Type::Const(SYMBOL.to_owned()),
                                other => self.infer(level, other)?,
                            };
                            ty = Type::ListCons(Box::new(head), Box::new(ty));
                        }
                        Ok(ty)
                    }
                    val => self.infer(level, val),
                },
                Form::Begin(vals) => {
                    let mut ret = Type::Const("void".to_owned());
                    for val in vals {
                        let val = self.infer(level, val)?;
                        ret = val;
                    }
                    Ok(ret)
                }
                Form::DefineMacro { name, .. } => {
                    let ty = Type::Const(SYMBOL.to_owned());
                    self.vars.insert(name.to_owned(), ty.clone());
                    Ok(ty)
                }
                Form::Lambda {
                    params,
                    vararg,
                    body,
                } => self.define_arrow(level, params, vararg, body),
                Form::Load { filename } => {
                    let lines =
                        std::fs::read_to_string(filename).map_err(|e| Error::IO(e.kind()))?;
                    let vals = parse_multiple(&lines).map_err(|_| Error::Parser)?;
                    let mut ret = None;
                    for val in vals {
                        let body_ty = self.infer(level, &val)?;
                        ret = Some(body_ty);
                    }
                    Ok(ret.unwrap_or(Type::Const("void".to_owned())))
                }
                Form::Let {
                    name,
                    bindings,
                    body,
                } => {
                    let old_vars = self.vars.clone();
                    let mut vars = Vec::with_capacity(bindings.len());
                    let mut vals = Vec::with_capacity(bindings.len());

                    for (param, val) in bindings {
                        vars.push(param.to_owned());
                        let val = self.infer(level + 1, val)?;
                        self.generalize(level, &val)?;
                        vals.push(val.clone());
                        self.vars.insert(param.to_owned(), val);
                    }

                    let params = vals.iter().rev().fold(Type::ListNil, |acc, param| {
                        Type::ListCons(param.clone().into(), acc.into())
                    });
                    let arrow_ret = self.new_unbound_tvar(level);
                    let arrow = Type::Arrow(params.into(), arrow_ret.clone().into());

                    let name = name.unwrap_or("_");
                    self.vars.insert(name.to_owned(), arrow.clone());

                    let mut ret = Type::Const("void".to_owned());
                    for val in body {
                        let body = self.infer(level, val)?;
                        ret = body;
                    }

                    self.unify(&arrow_ret, &ret)?;

                    self.vars = old_vars;

                    Ok(ret)
                }
                Form::While { cond, body } => {
                    let cond = self.infer(level, cond)?;
                    self.unify(&cond, &Type::Const(BOOL.to_owned()))?;
                    for val in body {
                        self.infer(level, val)?;
                    }
                    Ok(Type::Const("void".to_owned()))
                }
                Form::For { bindings, body } => {
                    // Save the old variables
                    let old_vars = self.vars.clone();

                    // Process each binding
                    for (var, val) in bindings {
                        // Ensure the right side is an array type
                        let array_ty = self.infer(level + 1, val)?;

                        // Extract the element type from the array
                        let element_ty = match array_ty {
                            Type::Array(elem_ty) => *elem_ty,
                            _ => {
                                return Err(Error::BadSpecialForm(
                                    "for: right side of binding must be an array".to_owned(),
                                    val.clone(),
                                ));
                            }
                        };

                        // Add the variable with the element type
                        self.vars.insert(var.to_string(), element_ty);
                    }

                    // Infer the types of the body for correctness, but ignore the return type
                    for val in body {
                        self.infer(level, val)?;
                    }

                    // Restore the old variables
                    self.vars = old_vars;

                    // Always return void type for 'for' loops
                    Ok(Type::Const("void".to_owned()))
                }
                Form::Call { func, args } => {
                    let f_ty = self.infer(level, func)?;
                    let (params, ret) = self.match_fun_ty(args.len(), f_ty)?;
                    let mut current = &params;
                    for i in 0..args.len() {
                        let arg = &args[i];
                        let arg_ty = self.infer(level, arg)?;
                        match current {
                            Type::ListCons(head, tail) => {
                                self.unify(&arg_ty, head)?;
                                current = tail;
                            }
                            Type::ListVarArg(vararg) => {
                                self.unify(&arg_ty, vararg)?;
                            }
                            Type::ListNil => {
                                return Err(Error::UnexpectedNumberOfArguments {
                                    expected: i,
                                    actual: args.len(),
                                });
                            }
                            _ => unreachable!("match_fun_ty should've taken care of this"),
                        }
                    }
                    Ok(ret)
                }
                Form::Match { val, arms, def } => {
                    let val = self.infer(level, val)?;
                    let (ret, rest) = match def {
                        None => (self.new_unbound_tvar(level), Type::RowEmpty),
                        Some((var, body)) => {
                            let old_vars = self.vars.clone();
                            let row = self.new_unbound_tvar(level);
                            self.vars
                                .insert(var.to_owned(), Type::Variant(row.clone().into()));
                            let mut ret = Type::Const("void".to_owned());
                            for val in body {
                                let body_ty = self.infer(level, val)?;
                                ret = body_ty;
                            }
                            self.vars = old_vars;
                            (ret, row)
                        }
                    };
                    // arms
                    let mut labels: Vec<(String, Type)> = Vec::new();
                    for (label, var, body) in arms {
                        let val = self.new_unbound_tvar(level);
                        let old_vars = self.vars.clone();
                        self.vars.insert(var.to_owned(), val.clone());
                        let mut arm_ret = Type::Const("void".to_owned());
                        for val in body {
                            let val = self.infer(level, val)?;
                            arm_ret = val;
                        }
                        self.vars = old_vars;
                        self.unify(&ret, &arm_ret)?;
                        labels.push((label.to_owned(), val));
                    }
                    let row = Type::RowExtend(labels, rest.into());
                    self.unify(&val, &Type::Variant(row.into()))?;
                    Ok(ret)
                }
            },
            Value::DottedList(_values, _value) => todo!(),
            Value::Func {
                params: _,
                vararg: _,
                body: _,
                closure: _,
            } => todo!(),
            Value::IOFunc(_iofunc) => todo!(),
            Value::Port(_) => todo!(),
        }
    }

    pub fn generalize(&mut self, level: Level, ty: &Type) -> Result<()> {
        match ty {
            Type::Var(id) => {
                let tvar = self.get_mut_tvar(*id)?;
                match tvar.clone() {
                    TypeVar::Unbound(other_level) if other_level > level => {
                        *tvar = TypeVar::Generic;
                        Ok(())
                    }
                    TypeVar::Link(ty) => self.generalize(level, &ty),
                    TypeVar::Unbound(_) | TypeVar::Weak(_) | TypeVar::Generic => Ok(()),
                }
            }
            Type::App(ty, args) => {
                for arg in args {
                    self.generalize(level, arg)?;
                }
                self.generalize(level, ty)
            }
            Type::Arrow(param, ret) => {
                self.generalize(level, param)?;
                self.generalize(level, ret)
            }
            Type::ListCons(head, tail) => {
                self.generalize(level, head)?;
                self.generalize(level, tail)
            }
            Type::ListVarArg(vararg) => self.generalize(level, vararg),
            Type::Array(ty) => self.generalize(level, ty),
            Type::Record(row) => self.generalize(level, row),
            Type::Variant(row) => self.generalize(level, row),
            Type::RowExtend(labels, rest) => {
                for (_, ty) in labels {
                    self.generalize(level, ty)?;
                }
                self.generalize(level, rest)
            }
            Type::Const(_) | Type::ListNil | Type::RowEmpty => Ok(()),
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
                    TypeVar::Generic => {
                        let ty = id_vars
                            .entry(id)
                            .or_insert_with(|| self.new_unbound_tvar(level));
                        Ok(ty.clone())
                    }
                    TypeVar::Link(ty) => self.instantiate_impl(id_vars, level, ty),
                    TypeVar::Unbound(_) | TypeVar::Weak(_) => Ok(ty),
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
            Type::Arrow(param, ret) => {
                let param = self.instantiate_impl(id_vars, level, *param)?;
                let ret = self.instantiate_impl(id_vars, level, *ret)?;
                Ok(Type::Arrow(Box::new(param), Box::new(ret)))
            }
            Type::ListNil => Ok(Type::ListNil),
            Type::ListVarArg(vararg) => {
                let vararg = self.instantiate_impl(id_vars, level, *vararg)?;
                Ok(Type::ListVarArg(Box::new(vararg)))
            }
            Type::ListCons(head, tail) => {
                let head = self.instantiate_impl(id_vars, level, *head)?;
                let tail = self.instantiate_impl(id_vars, level, *tail)?;
                Ok(Type::ListCons(Box::new(head), Box::new(tail)))
            }
            Type::Array(ty) => {
                let ty = self.instantiate_impl(id_vars, level, *ty)?;
                Ok(Type::Array(Box::new(ty)))
            }
            Type::Record(row) => Ok(Type::Record(
                self.instantiate_impl(id_vars, level, *row)?.into(),
            )),
            Type::Variant(row) => Ok(Type::Variant(
                self.instantiate_impl(id_vars, level, *row)?.into(),
            )),
            Type::RowEmpty => Ok(Type::RowEmpty),
            Type::RowExtend(labels, rest) => {
                let mut new_labels = Vec::with_capacity(labels.len());
                for (label, ty) in labels {
                    let ty = self.instantiate_impl(id_vars, level, ty)?;
                    new_labels.push((label, ty));
                }
                let rest = self.instantiate_impl(id_vars, level, *rest)?;
                Ok(Type::RowExtend(new_labels, Box::new(rest)))
            }
        }
    }

    fn match_fun_ty(&mut self, num_params: usize, ty: Type) -> Result<(Type, Type)> {
        match ty {
            Type::Arrow(params, ret) => Ok((*params, *ret)),
            Type::Var(id) => {
                let tvar = self.get_tvar(id)?;
                match tvar.clone() {
                    TypeVar::Unbound(level) | TypeVar::Weak(level) => {
                        let params = (0..num_params)
                            .map(|_| self.new_unbound_tvar(level))
                            .rev()
                            .fold(Type::ListNil, |acc, param| {
                                Type::ListCons(Box::new(param), Box::new(acc))
                            });
                        let ret = self.new_unbound_tvar(level);
                        let ty = Type::Arrow(Box::new(params.clone()), Box::new(ret.clone()));
                        self.link(id, ty)?;
                        Ok((params, ret))
                    }
                    TypeVar::Link(ty) => self.match_fun_ty(num_params, ty),
                    TypeVar::Generic => Err(Error::ExpectedAFunction(ty)),
                }
            }
            _ => Err(Error::ExpectedAFunction(ty)),
        }
    }

    #[allow(dead_code)]
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
        let mut namer = Namers::new();
        self.ty_to_string_impl(&mut namer, ty)
    }

    fn ty_to_string_impl(&self, namer: &mut Namers, ty: &Type) -> Result<String> {
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
            Type::Arrow(params, ret) => {
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
                        Type::ListVarArg(vararg) => {
                            if i != 0 {
                                ty_str.push(' ');
                            }
                            let vararg = self.ty_to_string_impl(namer, vararg)?;
                            ty_str.push_str(&format!(" . {}", vararg));
                            break;
                        }
                        Type::ListNil => break,
                        ty => {
                            return Err(Error::ExpectedAFunction(ty.clone()));
                        }
                    }
                    i += 1;
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
                    TypeVar::Link(ty) => self.ty_to_string_impl(namer, ty),
                    TypeVar::Generic | TypeVar::Unbound(_) | TypeVar::Weak(_) => {
                        let name = namer.get_or_insert(*id, tvar);
                        Ok(name.to_string())
                    }
                }
            }
            Type::Array(ty) => {
                let ty = self.ty_to_string_impl(namer, ty)?;
                Ok(format!("[{}]", ty))
            }
            Type::ListNil => Ok("()".to_owned()),
            Type::ListVarArg(ty) => {
                let ty = self.ty_to_string_impl(namer, ty)?;
                Ok(format!("( . {} )", ty))
            }
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
            Type::Record(row) => {
                let row_str = self.ty_to_string_impl(namer, row)?;
                Ok(format!("{{{}}}", row_str))
            }
            Type::Variant(row) => {
                let row_str = self.ty_to_string_impl(namer, row)?;
                Ok(format!("({})", row_str))
            }
            Type::RowEmpty => Ok("".to_owned()),
            Type::RowExtend(_, _) => {
                let (labels, rest) = self.match_row_ty(ty)?;
                let mut output = String::new();
                let mut sep = "";
                for (label, ty) in labels {
                    let ty_str = self.ty_to_string_impl(namer, &ty)?;
                    output.push_str(&format!("{sep}.{label} {}", ty_str));
                    sep = " ";
                }
                let rest = self.ty_to_string_impl(namer, &rest)?;
                if !rest.is_empty() {
                    output.push_str(&format!(" . {}", rest));
                }
                Ok(output)
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
                    let params = env.new_generic_tvar();
                    let ret = env.new_generic_tvar();

                    let func_type = Type::Arrow(Box::new(params.clone()), Box::new(ret.clone()));

                    Type::Arrow(
                        Box::new(Type::ListCons(
                            Box::new(func_type),
                            Box::new(Type::ListCons(Box::new(params), Box::new(Type::ListNil))),
                        )),
                        Box::new(ret),
                    )
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
        define_io_func(&mut env, "apply", IOFunc::Apply);
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

struct Namers {
    unbound: Namer,
    generic: Namer,
    weak: Namer,
}

impl Namers {
    fn new() -> Self {
        let unbound = Namer::new();
        let generic = Namer::new();
        let weak = Namer::new();
        Self {
            unbound,
            generic,
            weak,
        }
    }

    fn get_or_insert(&mut self, id: Id, var: &TypeVar) -> String {
        match var {
            TypeVar::Unbound(_) => format!("_{}", self.unbound.get_or_insert(id)),
            TypeVar::Generic => format!("{}", self.generic.get_or_insert(id)),
            TypeVar::Weak(_) => format!("~{}", self.weak.get_or_insert(id)),
            TypeVar::Link(_) => unreachable!("Link should be resolved before naming"),
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
    use crate::{infer::Env, parser::parse};

    fn run(input: &str, expected: &str, env: &mut Env) {
        let value = parse(input).unwrap();
        let actual = env
            .infer_value(&value)
            .expect(&format!("input: {}, value: {:?}", input, value));
        env.generalize(-1, &actual).unwrap();
        if !expected.is_empty() {
            let (vars, ty) = crate::typing::parse(expected).unwrap();
            let expected = env.replace_ty_constants_with_vars(vars, ty);
            let expected = env.ty_to_string(&expected).unwrap();
            let actual = env.ty_to_string(&actual).unwrap();
            assert_eq!(actual, expected, "input: {}, value: {}", input, value);
        }
    }

    #[test]
    fn cdr_test() {
        let mut env = Env::primitive_bindings();
        run("(cdr '(1 2 3))", "(int int)", &mut env);
    }

    #[test]
    fn apply_test() {
        let mut env = Env::primitive_bindings();
        run("(apply + '(1 2 3))", "int", &mut env);
        run("(apply + '(1 2))", "int", &mut env);
        run("(apply + '(1))", "int", &mut env);
        run("(define (add a b) (+ a b))", "", &mut env);
        run("(apply add '(1 2))", "int", &mut env);
        run("(define (seq a b) (string=? a b))", "", &mut env);
        run("(apply seq '(\"a\" \"a\"))", "bool", &mut env);
        run("(define (test a b) b)", "", &mut env);
        run("(test 1 \"a\")", "string", &mut env);
        run("(test \"a\" 1)", "int", &mut env);
        run("(apply test '(1 \"a\"))", "string", &mut env);
        run("(apply test '(\"a\" 1))", "int", &mut env);
    }

    #[test]
    fn vararg_test() {
        let mut env = Env::primitive_bindings();
        run(
            "(define (add a b) (+ a b))",
            "(lambda (int int) int)",
            &mut env,
        );
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
        let mut env = Env::primitive_bindings();
        for (input, expected) in cases {
            run(input, expected, &mut env);
        }
    }
}
