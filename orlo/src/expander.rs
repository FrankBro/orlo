use std::collections::HashMap;

use crate::{env::Env, eval::eval, value::Value};

#[derive(Debug, Clone)]
pub struct Expander {
    macros: HashMap<String, Macro>,
}

#[derive(Debug, Clone)]
struct Macro {
    params: Vec<String>,
    body: Value,
}

impl Expander {
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
        }
    }

    pub fn define_macro(&mut self, name: String, params: Vec<String>, body: Value) {
        self.macros.insert(name, Macro { params, body });
    }

    // Also register macros
    pub fn expand(&mut self, expr: &Value) -> Result<Value, String> {
        match expr {
            Value::List(vals) => match &vals[..] {
                [
                    Value::Atom(atom),
                    Value::Atom(name),
                    Value::List(params),
                    body,
                ] if atom == "define-macro" => {
                    let param_names: Vec<String> = params
                        .iter()
                        .filter_map(|p| match p {
                            Value::Atom(s) => Some(s.clone()),
                            _ => None,
                        })
                        .collect();
                    self.define_macro(name.clone(), param_names, body.clone());
                    // Return the macro definition as-is for the evaluator to handle
                    return Ok(expr.clone());
                }
                [Value::Atom(name), args @ ..] if self.macros.contains_key(name) => {
                    let macro_def = self.macros.get(name).expect("Macro should exist");
                    // Apply the macro
                    if args.len() != macro_def.params.len() {
                        return Err(format!(
                            "Macro {} expects {} arguments, got {}",
                            name,
                            macro_def.params.len(),
                            args.len()
                        ));
                    }

                    // Create an environment with parameter bindings
                    let mut env = Env::default();
                    for (param, arg) in macro_def.params.iter().zip(args.iter()) {
                        env.define_var(param.clone(), arg.clone());
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
