use crate::value::Value;

#[derive(Debug, PartialEq)]
pub struct Error {
    pub name: &'static str,
    pub description: String,
}

type Result<T, E = Error> = std::result::Result<T, E>;

pub enum Access<'a> {
    Index(i64),
    Field(&'a str),
}

pub enum Form<'a> {
    Append(&'a [Value]),
    List(&'a [Value]),
    Access(&'a Value, Vec<Access<'a>>),
    If {
        cond: &'a Value,
        then_branch: &'a Value,
        else_branch: &'a Value,
    },
    Set {
        var: &'a str,
        val: &'a Value,
    },
    Push {
        var: &'a str,
        val: &'a Value,
    },
    DefineValue {
        name: &'a str,
        val: &'a Value,
    },
    DefineFunction {
        name: &'a str,
        params: Vec<&'a str>,
        vararg: Option<&'a str>,
        body: &'a [Value],
    },
    Quote(&'a Value),
    Begin(&'a [Value]),
    DefineMacro {
        name: &'a str,
        params: Vec<&'a str>,
        vararg: Option<&'a str>,
        body: &'a Value,
    },
    Lambda {
        params: Vec<&'a str>,
        vararg: Option<&'a str>,
        body: &'a [Value],
    },
    Load {
        filename: &'a str,
    },
    Let {
        name: Option<&'a str>,
        bindings: Vec<(&'a str, &'a Value)>,
        body: &'a [Value],
    },
    While {
        cond: &'a Value,
        body: &'a [Value],
    },
    For {
        bindings: Vec<(&'a str, &'a Value)>,
        body: &'a [Value],
    },
    Call {
        func: &'a Value,
        args: &'a [Value],
    },
    Match {
        val: &'a Value,
        arms: Vec<(&'a str, &'a str, &'a [Value])>,
        def: Option<(&'a str, &'a [Value])>,
    },
}

pub fn identify<'a>(vals: &'a [Value]) -> Result<Form<'a>> {
    match vals {
        [Value::Atom(atom), val] if atom == "quote" => Ok(Form::Quote(val)),
        [Value::Atom(atom), body @ ..] if atom == "begin" => Ok(Form::Begin(body)),
        [Value::Atom(atom), Value::List(params), body] if atom == "define-macro" => {
            define_macro(params, None, body)
        }
        [Value::Atom(atom), Value::DottedList(params, vararg), body] if atom == "define-macro" => {
            define_macro(params, Some(vararg), body)
        }
        [Value::Atom(atom), body @ ..] if atom == "append" => Ok(Form::Append(body)),
        [Value::Atom(atom), container, Value::List(accesses)] if atom == "access" => {
            let mut access_vec: Vec<Access> = Vec::new();
            for access in accesses {
                match access {
                    Value::Number(n) => access_vec.push(Access::Index(*n)),
                    Value::Atom(s) => access_vec.push(Access::Field(s.as_str())),
                    _ => {
                        return Err(Error {
                            name: "access",
                            description: format!("Invalid access: {:?}", access),
                        });
                    }
                }
            }
            Ok(Form::Access(container, access_vec))
        }
        [Value::Atom(atom), body @ ..] if atom == "list" => Ok(Form::List(body)),
        [Value::Atom(atom), cond, then_branch, else_branch] if atom == "if" => Ok(Form::If {
            cond,
            then_branch,
            else_branch,
        }),
        [Value::Atom(atom), Value::Atom(var), val] if atom == "set!" => Ok(Form::Set {
            var: var.as_str(),
            val,
        }),
        [Value::Atom(atom), Value::Atom(var), val] if atom == "push!" => Ok(Form::Push {
            var: var.as_str(),
            val,
        }),
        [Value::Atom(atom), Value::Atom(var), val] if atom == "define" => Ok(Form::DefineValue {
            name: var.as_str(),
            val,
        }),
        [Value::Atom(atom), Value::List(name_args), body @ ..] if atom == "define" => {
            define_function(name_args, None, body)
        }
        [
            Value::Atom(atom),
            Value::DottedList(name_args, vararg),
            body @ ..,
        ] if atom == "define" => define_function(name_args, Some(vararg), body),
        [Value::Atom(atom), Value::List(args), body @ ..] if atom == "lambda" => {
            lambda(args, None, body)
        }
        [
            Value::Atom(atom),
            Value::DottedList(args, vararg),
            body @ ..,
        ] if atom == "lambda" => lambda(args, Some(vararg), body),
        [Value::Atom(atom), vararg @ Value::Atom(_), body @ ..] if atom == "lambda" => {
            lambda(&[], Some(vararg), body)
        }
        [Value::Atom(atom), Value::String(path)] if atom == "load" => Ok(Form::Load {
            filename: path.as_str(),
        }),
        [Value::Atom(atom), Value::List(bindings), body @ ..] if atom == "let" => {
            let_form(None, bindings, body)
        }
        [
            Value::Atom(atom),
            Value::Atom(name),
            Value::List(bindings),
            body @ ..,
        ] if atom == "let" => let_form(Some(name.as_str()), bindings, body),
        [Value::Atom(atom), cond, body @ ..] if atom == "while" => Ok(Form::While { cond, body }),
        [Value::Atom(atom), Value::List(bindings), body @ ..] if atom == "for" => {
            let mut binding_vec: Vec<(&str, &Value)> = Vec::new();
            for binding in bindings {
                match binding {
                    Value::List(pair) => match &pair[..] {
                        [Value::Atom(name), val] => {
                            binding_vec.push((name.as_str(), val));
                        }
                        _ => {
                            return Err(Error {
                                name: "for",
                                description: format!("Invalid binding: {:?}", binding),
                            });
                        }
                    },
                    _ => {
                        return Err(Error {
                            name: "for",
                            description: format!("Invalid binding: {:?}", binding),
                        });
                    }
                }
            }
            Ok(Form::For {
                bindings: binding_vec,
                body,
            })
        }
        [Value::Atom(atom), val, arms @ ..] if atom == "match" => {
            let mut arm_vec: Vec<(&str, &str, &[Value])> = Vec::new();
            let mut default: Option<(&str, &[Value])> = None;
            for arm in arms {
                match arm {
                    Value::List(arm_list) => match &arm_list[..] {
                        [Value::Variant(label, val), body @ ..] => {
                            let var = match val.as_ref() {
                                Value::Atom(var) => var,
                                _ => {
                                    return Err(Error {
                                        name: "match",
                                        description: format!(
                                            "Pattern variable is not a symbol: {:?}",
                                            val
                                        ),
                                    });
                                }
                            };
                            arm_vec.push((label.as_str(), var.as_str(), body))
                        }
                        [Value::Atom(def), body @ ..] => {
                            if default.is_some() {
                                return Err(Error {
                                    name: "match",
                                    description: "Multiple default arms".to_string(),
                                });
                            }
                            default = Some((def.as_str(), body));
                        }
                        _ => {
                            return Err(Error {
                                name: "match",
                                description: format!("Invalid arm: {:?}", arm),
                            });
                        }
                    },
                    _ => {
                        return Err(Error {
                            name: "match",
                            description: format!("Invalid arm: {:?}", arm),
                        });
                    }
                }
            }
            Ok(Form::Match {
                val,
                arms: arm_vec,
                def: default,
            })
        }
        [func, args @ ..] => Ok(Form::Call { func, args }),
        [] => Err(Error {
            name: "empty input",
            description: "No input provided".to_string(),
        }),
    }
}

fn let_form<'a>(name: Option<&'a str>, vals: &'a [Value], body: &'a [Value]) -> Result<Form<'a>> {
    let mut bindings: Vec<(&str, &Value)> = Vec::new();
    for binding in vals {
        match binding {
            Value::List(pair) => match &pair[..] {
                [Value::Atom(name), val] => {
                    bindings.push((name.as_str(), val));
                }
                _ => {
                    return Err(Error {
                        name: "let",
                        description: format!("Invalid binding: {:?}", binding),
                    });
                }
            },
            _ => {
                return Err(Error {
                    name: "let",
                    description: format!("Invalid binding: {:?}", binding),
                });
            }
        }
    }
    Ok(Form::Let {
        name,
        bindings,
        body,
    })
}

fn lambda<'a>(args: &'a [Value], vararg: Option<&'a Value>, body: &'a [Value]) -> Result<Form<'a>> {
    let mut params: Vec<&str> = Vec::new();
    for param in args {
        if let Value::Atom(s) = param {
            params.push(s.as_str())
        } else {
            return Err(Error {
                name: "lambda",
                description: format!("Lambda parameter is not a symbol: {:?}", param),
            });
        }
    }
    let vararg = if let Some(vararg) = vararg {
        if let Value::Atom(s) = vararg {
            Some(s.as_str())
        } else {
            return Err(Error {
                name: "lambda",
                description: format!("Lambda vararg parameter is not a symbol: {:?}", vararg),
            });
        }
    } else {
        None
    };
    Ok(Form::Lambda {
        params,
        vararg,
        body,
    })
}

fn define_function<'a>(
    name_args: &'a [Value],
    vararg: Option<&'a Value>,
    body: &'a [Value],
) -> Result<Form<'a>> {
    let (name, params) = match &name_args[..] {
        [Value::Atom(name), params @ ..] => {
            let mut param_names: Vec<&str> = Vec::new();
            for param in params {
                if let Value::Atom(s) = param {
                    param_names.push(s.as_str())
                } else {
                    return Err(Error {
                        name: "define",
                        description: format!("Function parameter is not a symbol: {:?}", param),
                    });
                }
            }
            (name.as_str(), param_names)
        }
        [] => panic!("Function definition must have a name"),
        [value, ..] => panic!("Function name is not a symbol: {:?}", value),
    };
    let vararg = if let Some(vararg) = vararg {
        if let Value::Atom(s) = vararg {
            Some(s.as_str())
        } else {
            return Err(Error {
                name: "define",
                description: format!("Function vararg parameter is not a symbol: {:?}", vararg),
            });
        }
    } else {
        None
    };
    Ok(Form::DefineFunction {
        name,
        params,
        vararg,
        body,
    })
}

fn define_macro<'a>(
    params: &'a Vec<Value>,
    vararg: Option<&'a Value>,
    body: &'a Value,
) -> Result<Form<'a>> {
    let (name, params) = match &params[..] {
        [Value::Atom(name), params @ ..] => {
            let mut param_names: Vec<&str> = Vec::new();
            for param in params {
                if let Value::Atom(s) = param {
                    param_names.push(s.as_str())
                } else {
                    return Err(Error {
                        name: "define-macro",
                        description: format!("Macro parameter is not a symbol: {:?}", param),
                    });
                }
            }
            (name.as_str(), param_names)
        }
        [] => panic!("Macro definition must have a name"),
        [value, ..] => panic!("Macro name is not a symbol: {:?}", value),
    };
    let vararg = if let Some(vararg) = vararg {
        if let Value::Atom(s) = vararg {
            Some(s.as_str())
        } else {
            panic!("Macro vararg parameter is not a symbol: {:?}", vararg);
        }
    } else {
        None
    };
    Ok(Form::DefineMacro {
        name,
        params,
        vararg,
        body,
    })
}
