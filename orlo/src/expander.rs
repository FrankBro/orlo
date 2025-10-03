use std::collections::HashMap;

use crate::{env::Env, error, eval::eval, value::Value};

#[derive(Debug, Clone)]
pub struct Expander {
    macros: HashMap<String, Macro>,
}

#[derive(Debug, Clone)]
struct Macro {
    params: Vec<String>,
    vararg: Option<String>,
    body: Value,
}

#[derive(Debug)]
pub enum Error {
    MacroDefinitionEmpty,
    MacroNameNotSymbol(Value),
    MacroParamNotSymbol(Value),
    MacroTooFewArguments {
        name: String,
        minimum: usize,
        got: usize,
    },
    MacroExpansion(error::Error),
}

type Result<T, E = Error> = std::result::Result<T, E>;

impl Expander {
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
        }
    }

    fn define_macro(
        &mut self,
        param_values: &Vec<Value>,
        vararg_value: Option<&Value>,
        body: &Value,
    ) -> Result<()> {
        let (macro_name, param_values) = match &param_values[..] {
            [Value::Atom(name), params @ ..] => (name.clone(), params),
            [] => return Err(Error::MacroDefinitionEmpty),
            [value, ..] => return Err(Error::MacroNameNotSymbol(value.clone())),
        };
        let mut params: Vec<String> = Vec::with_capacity(param_values.len());
        for param_value in param_values {
            if let Value::Atom(s) = param_value {
                params.push(s.clone());
            } else {
                return Err(Error::MacroParamNotSymbol(param_value.clone()));
            }
        }
        let vararg = match vararg_value {
            Some(Value::Atom(s)) => Some(s.clone()),
            Some(value) => return Err(Error::MacroParamNotSymbol(value.clone())),
            None => None,
        };
        let body = body.clone();
        self.macros.insert(
            macro_name,
            Macro {
                params,
                vararg,
                body,
            },
        );
        Ok(())
    }

    // Also register macros
    pub fn expand(&mut self, expr: &Value) -> Result<Value> {
        match expr {
            Value::List(vals) => match &vals[..] {
                [Value::Atom(atom), Value::List(params), body] if atom == "define-macro" => {
                    self.define_macro(params, None, body)?;
                    return Ok(expr.clone());
                }
                [Value::Atom(atom), Value::DottedList(params, vararg), body]
                    if atom == "define-macro" =>
                {
                    let vararg = Some(vararg.as_ref());
                    self.define_macro(params, vararg, body)?;
                    return Ok(expr.clone());
                }
                [Value::Atom(name), args @ ..] if self.macros.contains_key(name) => {
                    let macro_def = self.macros.get(name).expect("Macro should exist");

                    if args.len() < macro_def.params.len() {
                        return Err(Error::MacroTooFewArguments {
                            name: name.to_owned(),
                            minimum: macro_def.params.len(),
                            got: args.len(),
                        });
                    }

                    let mut env = Env::default();
                    let args = match macro_def.vararg.as_ref() {
                        Some(name) => {
                            let (fixed_args, vararg_vals) = args.split_at(macro_def.params.len());
                            let vararg_vals = Value::List(vararg_vals.to_vec());
                            env.define_var(name.to_owned(), vararg_vals);
                            fixed_args
                        }
                        None => args,
                    };
                    for (param, arg) in macro_def.params.iter().zip(args.iter()) {
                        env.define_var(param.clone(), arg.clone());
                    }

                    // Evaluate the macro body to get the expansion
                    let expanded =
                        eval(&mut env, &macro_def.body).map_err(Error::MacroExpansion)?;

                    // Recursively expand the result
                    return self.expand(&expanded);
                }
                _ => {
                    let expanded: Result<Vec<_>, _> = vals.iter().map(|v| self.expand(v)).collect();
                    Ok(Value::List(expanded?))
                }
            },
            Value::DottedList(head, tail) => {
                let expanded_head: Result<Vec<_>, _> =
                    head.iter().map(|v| self.expand(v)).collect();
                let expanded_tail = self.expand(tail)?;
                Ok(Value::DottedList(expanded_head?, Box::new(expanded_tail)))
            }
            Value::Array(vals) => {
                let expanded: Result<Vec<_>, _> = vals.iter().map(|v| self.expand(v)).collect();
                Ok(Value::Array(expanded?))
            }
            _ => Ok(expr.clone()),
        }
    }
}
