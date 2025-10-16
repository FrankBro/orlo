use crate::{
    env::Env,
    error::Error,
    identifier::{Access, Form, identify},
    primitive::{self, load},
    value::{IOFunc, PrimitiveFunc, Value},
};

type Result<T> = std::result::Result<T, Error>;

pub fn apply(env: &mut Env, val: &Value, args: &[Value]) -> Result<Value> {
    match val {
        Value::PrimitiveFunc(func) => match func {
            PrimitiveFunc::Add => primitive::numeric_binop(args, |acc, val| acc + val),
            PrimitiveFunc::Sub => primitive::numeric_binop(args, |acc, val| acc - val),
            PrimitiveFunc::Mul => primitive::numeric_binop(args, |acc, val| acc * val),
            PrimitiveFunc::Div => primitive::numeric_binop(args, |acc, val| acc / val),
            PrimitiveFunc::Rem => primitive::numeric_binop(args, |acc, val| acc % val),
            PrimitiveFunc::Eq => primitive::numeric_bool_binop(args, |lhs, rhs| lhs == rhs),
            PrimitiveFunc::Lt => primitive::numeric_bool_binop(args, |lhs, rhs| lhs < rhs),
            PrimitiveFunc::Gt => primitive::numeric_bool_binop(args, |lhs, rhs| lhs > rhs),
            PrimitiveFunc::Ne => primitive::numeric_bool_binop(args, |lhs, rhs| lhs != rhs),
            PrimitiveFunc::Ge => primitive::numeric_bool_binop(args, |lhs, rhs| lhs >= rhs),
            PrimitiveFunc::Le => primitive::numeric_bool_binop(args, |lhs, rhs| lhs <= rhs),
            PrimitiveFunc::And => primitive::bool_bool_binop(args, |lhs, rhs| lhs && rhs),
            PrimitiveFunc::Or => primitive::bool_bool_binop(args, |lhs, rhs| lhs || rhs),
            PrimitiveFunc::StringEq => primitive::string_bool_binop(args, |lhs, rhs| lhs == rhs),
            PrimitiveFunc::StringLt => primitive::string_bool_binop(args, |lhs, rhs| lhs < rhs),
            PrimitiveFunc::StringGt => primitive::string_bool_binop(args, |lhs, rhs| lhs > rhs),
            PrimitiveFunc::StringLe => primitive::string_bool_binop(args, |lhs, rhs| lhs <= rhs),
            PrimitiveFunc::StringGe => primitive::string_bool_binop(args, |lhs, rhs| lhs >= rhs),
            PrimitiveFunc::Car => primitive::car(args),
            PrimitiveFunc::Cdr => primitive::cdr(args),
            PrimitiveFunc::Cons => primitive::cons(args),
            PrimitiveFunc::Eqv => primitive::eqv(args),
            PrimitiveFunc::Equal => primitive::equal(args),
        },
        Value::IOFunc(func) => match func {
            IOFunc::Apply => primitive::apply_proc(env, args),
            IOFunc::MakeReadPort => primitive::make_read_port(env, args),
            IOFunc::MakeWritePort => primitive::make_write_port(env, args),
            IOFunc::ClosePort => primitive::close_port(env, args),
            IOFunc::Read => primitive::read_proc(env, args),
            IOFunc::Write => primitive::write_proc(env, args),
            IOFunc::ReadContents => primitive::read_contents(args),
            IOFunc::ReadAll => primitive::read_all(args),
        },
        Value::Func {
            params,
            vararg,
            body,
            closure,
        } => {
            if params.len() != args.len() && vararg == &None {
                return Err(Error::NumArgs(params.len(), args.to_vec()));
            }
            env.with_closure(closure);
            let mut last = 0;
            for i in 0..params.len() {
                last = i;
                let param = &params[i];
                let arg = &args[i];
                env.define_var(param.to_owned(), arg.clone());
            }
            if let Some(vararg) = vararg {
                env.define_var(vararg.to_owned(), Value::List(args[last..].to_vec()));
            }
            let mut ret = None;
            for val in body {
                ret = Some(eval(env, val)?);
            }
            ret.ok_or(Error::EmptyBody)
        }
        _ => Err(Error::NotFunction(val.clone())),
    }
}

pub fn eval(env: &mut Env, val: &Value) -> Result<Value> {
    match val {
        Value::Datum(_) => Ok(val.clone()),
        Value::String(_) => Ok(val.clone()),
        Value::Number(_) => Ok(val.clone()),
        Value::Bool(_) => Ok(val.clone()),
        Value::Atom(id) => env.get_var(id).cloned(),
        Value::Array(_) => Ok(val.clone()),
        Value::Variant(_, _) => Ok(val.clone()),
        Value::DottedVariant(_, _) => unreachable!(),
        Value::Record(_) => Ok(val.clone()),
        Value::DottedRecord(vals, rest) => {
            let mut fields = match eval(env, &rest)? {
                Value::Record(fields) => fields.clone(),
                _ => {
                    return Err(Error::BadSpecialForm(
                        "dotted record requires a record".to_string(),
                        rest.as_ref().clone(),
                    ));
                }
            };
            for (key, value) in vals {
                if let Some(val) = fields.iter_mut().find(|(k, _)| k == key) {
                    *val = (key.clone(), value.clone());
                } else {
                    fields.push((key.clone(), value.clone()));
                }
            }
            Ok(Value::Record(fields))
        }
        Value::List(vals) => match identify(vals)? {
            Form::Append(vals) => {
                let mut result = Vec::new();
                for val in vals {
                    match eval(env, val)? {
                        Value::List(mut lst) => result.append(&mut lst),
                        other => {
                            return Err(Error::TypeMismatch("list".to_string(), other.clone()));
                        }
                    }
                }
                Ok(Value::List(result))
            }
            Form::List(vals) => {
                let mut xs = Vec::new();
                for val in vals {
                    xs.push(eval(env, val)?);
                }
                Ok(Value::List(xs))
            }
            Form::Access(container, accesses) => {
                let mut value = eval(env, container)?;
                for access in accesses {
                    match (value, access) {
                        (Value::Record(fields), Access::Field(field_name)) => {
                            match fields.iter().find(|(name, _)| name == field_name) {
                                Some((_, field_value)) => {
                                    value = field_value.clone();
                                }
                                None => {
                                    return Err(Error::NoSuchField(
                                        field_name.to_owned(),
                                        Value::Record(fields.clone()),
                                    ));
                                }
                            }
                        }
                        (Value::Array(elements), Access::Index(index)) => {
                            if index >= 0 {
                                let index = index as usize;
                                if index >= elements.len() {
                                    return Err(Error::IndexOutOfBounds(
                                        index as i64,
                                        Value::Array(elements.clone()),
                                    ));
                                }
                                value = elements[index].clone();
                            } else {
                                let index = elements.len() as i64 + index;
                                if index < 0 {
                                    return Err(Error::IndexOutOfBounds(
                                        index,
                                        Value::Array(elements.clone()),
                                    ));
                                }
                                value = elements[index as usize].clone();
                            }
                        }
                        (_, _) => {
                            return Err(Error::BadSpecialForm(
                                "access only works on record + field name or array + index"
                                    .to_owned(),
                                val.clone(),
                            ));
                        }
                    }
                }
                Ok(value)
            }
            Form::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let result = eval(env, cond)?;
                match result {
                    Value::Bool(true) => eval(env, then_branch),
                    Value::Bool(false) => eval(env, else_branch),
                    _ => Err(Error::BadSpecialForm(
                        "if condition must evaluate to a boolean".to_owned(),
                        cond.clone(),
                    )),
                }
            }
            Form::Set { var, val } => {
                let val = eval(env, val)?;
                env.set_var(var, val)
            }
            Form::Push { var, val } => {
                let val = eval(env, val)?;
                let arr = env.get_var(var)?;
                match arr {
                    Value::Array(arr) => {
                        let mut new_arr = arr.clone();
                        new_arr.push(val);
                        env.set_var(var, Value::Array(new_arr))
                    }
                    _ => Err(Error::TypeMismatch("array".to_string(), arr.clone())),
                }
            }
            Form::DefineValue { name, val } => {
                let val = eval(env, val)?;
                Ok(env.define_var(name.to_owned(), val))
            }
            Form::DefineFunction {
                name,
                params,
                vararg,
                body,
            } => {
                let closure = env.make_closure();
                let params = params.into_iter().map(|arg| arg.to_owned()).collect();
                let vararg = vararg.map(|arg| arg.to_owned());
                let body = body.to_vec();
                let func = Value::Func {
                    params,
                    vararg,
                    body,
                    closure,
                };
                Ok(env.define_var(name.to_owned(), func))
            }
            Form::Quote(val) => Ok(val.clone()),
            Form::Begin(body) => {
                let mut ret = None;
                for expr in body {
                    ret = Some(eval(env, expr)?);
                }
                ret.ok_or(Error::EmptyBody)
            }
            Form::DefineMacro { name, .. } => Ok(Value::Atom(name.to_owned())),
            Form::Lambda {
                params,
                vararg,
                body,
            } => {
                let closure = env.make_closure();
                let params = params.into_iter().map(|param| param.to_owned()).collect();
                let vararg = vararg.map(|arg| arg.to_owned());
                let body = body.to_vec();
                Ok(Value::Func {
                    params,
                    vararg,
                    body,
                    closure,
                })
            }
            Form::Load { filename } => {
                let vals = load(filename)?;
                let mut ret = None;
                for val in vals {
                    ret = Some(eval(env, &val)?);
                }
                ret.ok_or(Error::EmptyBody)
            }
            Form::Let {
                name,
                bindings,
                body,
            } => {
                let closure = env.make_closure();
                let mut vars = Vec::with_capacity(bindings.len());
                let mut vals = Vec::with_capacity(bindings.len());

                for (param, val) in bindings {
                    vars.push(param.to_owned());
                    let val = eval(env, val)?;
                    vals.push(val);
                }

                let params = vars.clone();
                let vararg = None;
                let func_body = body.to_vec();
                let func_closure = env.make_closure();
                let func = Value::Func {
                    params,
                    vararg,
                    body: func_body,
                    closure: func_closure.clone(),
                };

                let name = name.unwrap_or("_");
                env.define_var(name.to_owned(), func);

                for (param, val) in vars.iter().zip(vals.iter()) {
                    env.define_var(param.to_owned(), val.clone());
                }

                let mut ret = None;
                for expr in body {
                    ret = Some(eval(env, expr)?);
                }
                env.load_closure(closure);
                ret.ok_or(Error::EmptyBody)
            }
            Form::While { cond, body } => {
                let closure = env.make_closure();
                while match eval(env, cond)? {
                    Value::Bool(true) => true,
                    Value::Bool(false) => false,
                    other => {
                        return Err(Error::TypeMismatch("boolean".to_owned(), other.clone()));
                    }
                } {
                    for expr in body {
                        eval(env, expr)?;
                    }
                }
                env.load_closure(closure);
                Ok(Value::List(vec![]))
            }
            Form::For { bindings, body } => {
                // TODO: AI code needs to be reviewed and rewritten probably
                // Extract bindings
                let mut var_names = Vec::new();
                let mut arrays = Vec::new();

                for (var, array_expr) in bindings {
                    let array_val = eval(env, array_expr)?;

                    match array_val {
                        Value::Array(items) => {
                            var_names.push(var.to_owned());
                            arrays.push(items);
                        }
                        _ => {
                            return Err(Error::BadSpecialForm(
                                "for: right side of binding must be an array".to_owned(),
                                array_val,
                            ));
                        }
                    }
                }

                // Check that all arrays have the same length
                if !arrays.is_empty() {
                    // Check if any arrays are empty
                    for array in &arrays {
                        if array.len() == 0 {
                            // If any array is empty, return () immediately
                            return Ok(Value::List(vec![]));
                        }
                    }

                    // Helper function to recursively execute nested loops
                    fn execute_nested_loops(
                        env: &mut Env,
                        var_names: &[String],
                        arrays: &[Vec<Value>],
                        body: &[Value],
                        current_index: usize,
                        current_bindings: &mut Vec<(String, Value)>,
                    ) -> Result<()> {
                        if current_index == var_names.len() {
                            // We have a complete set of bindings, apply them and run the body
                            env.make_closure();

                            // Define all variables in the environment
                            for (var_name, value) in current_bindings.iter() {
                                env.define_var(var_name.clone(), value.clone());
                            }

                            // Evaluate the body in this environment
                            for expr in body {
                                eval(env, expr)?;
                            }

                            return Ok(());
                        }

                        // Get current variable and array
                        let var_name = &var_names[current_index];
                        let array = &arrays[current_index];

                        // Iterate through the current array
                        for item in array {
                            current_bindings.push((var_name.clone(), item.clone()));
                            execute_nested_loops(
                                env,
                                var_names,
                                arrays,
                                body,
                                current_index + 1,
                                current_bindings,
                            )?;
                            current_bindings.pop();
                        }

                        Ok(())
                    }

                    // Start the nested loops with an empty set of bindings
                    let mut current_bindings = Vec::new();
                    execute_nested_loops(env, &var_names, &arrays, body, 0, &mut current_bindings)?;

                    // Return void/nil equivalent
                    Ok(Value::List(vec![]))
                } else {
                    // Empty bindings (no arrays specified), return void
                    Ok(Value::List(vec![]))
                }
            }
            Form::Call { func, args } => {
                let func = eval(env, func)?;
                let args = args
                    .iter()
                    .map(|arg| eval(env, arg))
                    .collect::<Result<Vec<_>>>()?;
                let closure = env.make_closure();
                let ret = apply(env, &func, &args);
                env.load_closure(closure);
                ret
            }
            Form::Match {
                val,
                arms,
                def: default,
            } => {
                let closure = env.make_closure();
                let (label, val) = match eval(env, val)? {
                    Value::Variant(label, val) => (label, val),
                    other => {
                        return Err(Error::TypeMismatch("variant".to_owned(), other.clone()));
                    }
                };
                for (arm_label, arm_binding, arm_body) in arms {
                    if &label == arm_label {
                        env.define_var(arm_binding.to_owned(), *val);
                        let mut ret = None;
                        for expr in arm_body {
                            ret = Some(eval(env, expr)?);
                        }
                        env.load_closure(closure);
                        return ret.ok_or(Error::EmptyBody);
                    }
                }
                if let Some((default_binding, default_body)) = default {
                    env.define_var(default_binding.to_owned(), Value::Variant(label, val));
                    let mut ret = None;
                    for expr in default_body {
                        ret = Some(eval(env, expr)?);
                    }
                    env.load_closure(closure);
                    return ret.ok_or(Error::EmptyBody);
                }
                unreachable!()
            }
        },
        Value::DottedList(_, _) => todo!(),
        Value::PrimitiveFunc(_) => todo!(),
        Value::Func {
            params: _params,
            vararg: _vararg,
            body: _body,
            closure: _closure,
        } => todo!(),
        Value::IOFunc(_iofunc) => todo!(),
        Value::Port(_) => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::{eval::Env, parser::parse, value::Value};

    use super::Error;

    #[test]
    fn eval() {
        let cases = vec![
            ("'atom", Ok("atom")),
            ("2", Ok("2")),
            ("\"a string\"", Ok("\"a string\"")),
            ("(+ 2 2)", Ok("4")),
            ("(+ 2 (- 4 1))", Ok("5")),
            ("(- (+ 4 6 3) 3 5 2)", Ok("3")),
            ("(< 2 3)", Ok("#t")),
            ("(> 2 3)", Ok("#f")),
            ("(>= 3 3)", Ok("#t")),
            ("(string=? \"test\" \"test\")", Ok("#t")),
            ("(string<? \"abc\" \"bba\")", Ok("#t")),
            ("(if (> 2 3) \"no\" \"yes\")", Ok("\"yes\"")),
            ("(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")", Ok("9")),
            ("(cdr '(a simple test))", Ok("(simple test)")),
            ("(car (cdr '(a simple test)))", Ok("simple")),
            ("(car '((this is) a test))", Ok("(this is)")),
            ("(cons '(this is) 'test)", Ok("((this is) . test)")),
            ("(cons '(this is) '())", Ok("((this is))")),
            ("(eqv? 1 3)", Ok("#f")),
            ("(eqv? 3 3)", Ok("#t")),
            ("(eqv? 'atom 'atom)", Ok("#t")),
            ("(define x 3)", Ok("3")),
            ("(+ x 2)", Ok("5")),
            (
                "(+ y 2)",
                Err(Error::UnboundVar(
                    "Getting an unbound variable".to_owned(),
                    "y".to_owned(),
                )),
            ),
            ("(define y 5)", Ok("5")),
            ("(+ x (- y 2))", Ok("6")),
            ("(define str \"A string\")", Ok("\"A string\"")),
            (
                "(< str \"The string\")",
                Err(Error::TypeMismatch(
                    "number".to_owned(),
                    Value::String("A string".to_owned()),
                )),
            ),
            ("(string<? str \"The string\")", Ok("#t")),
            ("(define (f x y) (+ x y))", Ok("(lambda (x y) 'body)")),
            ("(f 1 2)", Ok("3")),
            (
                "(f 1 2 3)",
                Err(Error::NumArgs(
                    2,
                    vec![Value::Number(1), Value::Number(2), Value::Number(3)],
                )),
            ),
            ("(f 1)", Err(Error::NumArgs(2, vec![Value::Number(1)]))),
            (
                "(define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))",
                Ok("(lambda (x) 'body)"),
            ),
            ("(factorial 10)", Ok("3628800")),
            (
                "(define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))",
                Ok("(lambda (inc) 'body)"),
            ),
            ("(define my-count (counter 5))", Ok("(lambda (x) 'body)")),
            ("(my-count 3)", Ok("8")),
            ("(my-count 6)", Ok("14")),
            ("(my-count 5)", Ok("19")),
            ("(load \"../stdlib.scm\")", Ok("(lambda (pred lst) 'body)")),
            ("(map (curry + 2) '(1 2 3 4))", Ok("(3 4 5 6)")),
            ("(filter even? '(1 2 3 4))", Ok("(2 4)")),
        ];
        let mut env = Env::primitive_bindings();
        for (input, expected) in cases {
            let val = parse(input).unwrap();
            let actual = super::eval(&mut env, &val).map(|val| val.to_string());
            let expected = expected.map(|str| str.to_owned());
            assert_eq!(expected, actual,);
        }
    }
}
