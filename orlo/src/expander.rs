use std::collections::HashMap;

use crate::{env::Env, eval::eval, value::Value};

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

impl Expander {
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
        }
    }

    pub fn define_macro(
        &mut self,
        name: String,
        params: Vec<String>,
        vararg: Option<String>,
        body: Value,
    ) {
        self.macros.insert(
            name,
            Macro {
                params,
                vararg,
                body,
            },
        );
    }

    // Also register macros
    pub fn expand(&mut self, expr: &Value) -> Result<Value, String> {
        match expr {
            Value::List(vals) => match &vals[..] {
                [Value::Atom(atom), Value::List(params), body] if atom == "define-macro" => {
                    let (macro_name, params) = match &params[..] {
                        [Value::Atom(name), params @ ..] => (name.clone(), params),
                        _ => {
                            return Err("Invalid macro definition".to_string());
                        }
                    };
                    let mut param_names: Vec<String> = Vec::with_capacity(params.len());
                    for param in params {
                        if let Value::Atom(s) = param {
                            param_names.push(s.clone());
                        } else {
                            return Err("Invalid parameter in macro definition".to_string());
                        }
                    }
                    self.define_macro(macro_name.clone(), param_names, None, body.clone());
                    // Return the macro definition as-is for the evaluator to handle
                    return Ok(expr.clone());
                }
                [Value::Atom(atom), Value::DottedList(params, vararg), body]
                    if atom == "define-macro" =>
                {
                    let (macro_name, params) = match &params[..] {
                        [Value::Atom(name), params @ ..] => (name.clone(), params),
                        _ => {
                            return Err("Invalid macro definition".to_string());
                        }
                    };
                    let mut param_names: Vec<String> = Vec::with_capacity(params.len());
                    for param in params {
                        if let Value::Atom(s) = param {
                            param_names.push(s.clone());
                        } else {
                            return Err("Invalid parameter in macro definition".to_string());
                        }
                    }
                    let vararg = match &**vararg {
                        Value::Atom(s) => Some(s.clone()),
                        _ => {
                            return Err("Invalid vararg in macro definition".to_string());
                        }
                    };
                    self.define_macro(macro_name.clone(), param_names, vararg, body.clone());
                    // Return the macro definition as-is for the evaluator to handle
                    return Ok(expr.clone());
                }
                [Value::Atom(name), args @ ..] if self.macros.contains_key(name) => {
                    let macro_def = self.macros.get(name).expect("Macro should exist");
                    // Apply the macro
                    if args.len() < macro_def.params.len() {
                        return Err(format!(
                            "Macro {} expects at least {} arguments, got {}",
                            name,
                            macro_def.params.len(),
                            args.len()
                        ));
                    }

                    // Create an environment with parameter bindings
                    let mut env = Env::default();
                    let (args, vararg) = if macro_def.vararg.is_some() {
                        let (fixed_args, vararg_vals) = args.split_at(macro_def.params.len());
                        (
                            fixed_args.to_vec(),
                            Some(Value::List(vararg_vals.to_vec()).quote()),
                        )
                    } else {
                        (args.to_vec(), None)
                    };
                    for (param, arg) in macro_def.params.iter().zip(args.iter()) {
                        env.define_var(param.clone(), arg.clone());
                    }
                    match (vararg, macro_def.vararg.as_ref()) {
                        (Some(vararg_val), Some(vararg_name)) => {
                            env.define_var(vararg_name.clone(), vararg_val);
                        }
                        (None, None) => {}
                        (_, _) => {
                            return Err("Macro vararg mismatch".to_string());
                        }
                    }

                    // Evaluate the macro body to get the expansion
                    let expanded = eval(&mut env, &macro_def.body)
                        .map_err(|e| format!("Macro expansion error: {:?}", e))?;

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
