use std::io::{BufRead, Write, stdin};

use crate::{
    env::Env,
    error::Error,
    eval::apply,
    parser::{parse_expr, parse_exprs},
    value::Value,
};

type Result<T> = std::result::Result<T, Error>;

fn as_number(val: &Value) -> Result<i64> {
    match val {
        Value::Number(number) => Ok(*number),
        Value::String(string) => {
            let number: i64 = string
                .parse()
                .map_err(|_| Error::TypeMismatch("number".to_owned(), val.clone()))?;
            Ok(number)
        }
        _ => Err(Error::TypeMismatch("number".to_owned(), val.clone())),
    }
}

fn as_string(val: &Value) -> Result<String> {
    match val {
        Value::String(string) => Ok(string.clone()),
        Value::Number(number) => Ok(number.to_string()),
        Value::Bool(bool) => Ok(bool.to_string()),
        _ => Err(Error::TypeMismatch("string".to_owned(), val.clone())),
    }
}

fn as_bool(val: &Value) -> Result<bool> {
    match val {
        Value::Bool(bool) => Ok(*bool),
        _ => Err(Error::TypeMismatch("bool".to_owned(), val.clone())),
    }
}

pub fn numeric_bool_binop<F>(vals: &[Value], f: F) -> Result<Value>
where
    F: Fn(i64, i64) -> bool,
{
    bool_binop(vals, as_number, f)
}

pub fn bool_bool_binop<F>(vals: &[Value], f: F) -> Result<Value>
where
    F: Fn(bool, bool) -> bool,
{
    bool_binop(vals, as_bool, f)
}

pub fn string_bool_binop<F>(vals: &[Value], f: F) -> Result<Value>
where
    F: Fn(String, String) -> bool,
{
    bool_binop(vals, as_string, f)
}

fn bool_binop<T, C, F>(vals: &[Value], c: C, f: F) -> Result<Value>
where
    C: Fn(&Value) -> Result<T>,
    F: Fn(T, T) -> bool,
{
    match vals {
        [lhs, rhs] => {
            let lhs = c(lhs)?;
            let rhs = c(rhs)?;
            let result = f(lhs, rhs);
            Ok(Value::Bool(result))
        }
        _ => Err(Error::NumArgs(2, vals.to_vec())),
    }
}

pub fn numeric_binop<F>(vals: &[Value], f: F) -> Result<Value>
where
    F: FnMut(i64, i64) -> i64,
{
    match vals {
        [] => Err(Error::NumArgs(2, vec![])),
        [val] => Err(Error::NumArgs(2, vec![val.clone()])),
        _ => {
            let num_vals = vals.iter().map(as_number).collect::<Result<Vec<_>>>()?;
            let result = num_vals
                .into_iter()
                .reduce(f)
                .ok_or_else(|| Error::NumArgs(2, vals.to_vec()))?;
            Ok(Value::Number(result))
        }
    }
}

pub fn car(vals: &[Value]) -> Result<Value> {
    match vals {
        [val @ Value::List(vals)] => match &vals[..] {
            [val, ..] => Ok(val.clone()),
            _ => Err(Error::TypeMismatch("pair".to_owned(), val.clone())),
        },
        [val @ Value::DottedList(vals, _)] => match &vals[..] {
            [val, ..] => Ok(val.clone()),
            _ => Err(Error::TypeMismatch("pair".to_owned(), val.clone())),
        },
        [val] => Err(Error::TypeMismatch("pair".to_owned(), val.clone())),
        _ => Err(Error::NumArgs(1, vals.to_vec())),
    }
}

pub fn cdr(vals: &[Value]) -> Result<Value> {
    match vals {
        [val @ Value::List(vals)] => match &vals[..] {
            [_, vals @ ..] => Ok(Value::List(vals.to_vec())),
            _ => Err(Error::TypeMismatch("pair".to_owned(), val.clone())),
        },
        [val @ Value::DottedList(vals, dval)] => match &vals[..] {
            [_, vals @ ..] => Ok(Value::DottedList(vals.to_vec(), dval.clone())),
            _ => Err(Error::TypeMismatch("pair".to_owned(), val.clone())),
        },
        [val] => Err(Error::TypeMismatch("pair".to_owned(), val.clone())),
        _ => Err(Error::NumArgs(1, vals.to_vec())),
    }
}

pub fn cons(vals: &[Value]) -> Result<Value> {
    match vals {
        [val, Value::List(vals)] => {
            let mut vals = vals.clone();
            vals.insert(0, val.clone());
            Ok(Value::List(vals))
        }
        [val, Value::DottedList(vals, dval)] => {
            let mut vals = vals.clone();
            vals.insert(0, val.clone());
            Ok(Value::DottedList(vals, dval.clone()))
        }
        [val, dval] => Ok(Value::DottedList(vec![val.clone()], Box::new(dval.clone()))),
        _ => Err(Error::NumArgs(2, vals.to_vec())),
    }
}

fn eqv_impl(vals: &[Value]) -> Result<bool> {
    match vals {
        [Value::Bool(val1), Value::Bool(val2)] => Ok(val1 == val2),
        [Value::Number(val1), Value::Number(val2)] => Ok(val1 == val2),
        [Value::String(val1), Value::String(val2)] => Ok(val1 == val2),
        [Value::Atom(val1), Value::Atom(val2)] => Ok(val1 == val2),
        [
            Value::DottedList(vals1, val1),
            Value::DottedList(vals2, val2),
        ] => {
            let mut vals1 = vals1.to_vec();
            vals1.push(*val1.clone());
            let mut vals2 = vals2.to_vec();
            vals2.push(*val2.clone());
            eqv_impl(&[Value::List(vals1), Value::List(vals2)])
        }
        [Value::List(vals1), Value::List(vals2)] => {
            if vals1.len() != vals2.len() {
                return Ok(false);
            }
            let mut result = true;
            for i in 0..vals1.len() {
                let val1 = &vals1[i];
                let val2 = &vals2[i];
                result &= eqv_impl(&[val1.clone(), val2.clone()])?;
            }
            Ok(result)
        }
        [_, _] => Ok(false),
        _ => Err(Error::NumArgs(2, vals.to_vec())),
    }
}

pub fn eqv(vals: &[Value]) -> Result<Value> {
    eqv_impl(vals).map(Value::Bool)
}

pub fn equal(vals: &[Value]) -> Result<Value> {
    match vals {
        [val1, val2] => match (as_number(val1), as_number(val2)) {
            (Ok(val1), Ok(val2)) => Ok(Value::Bool(val1 == val2)),
            _ => match (as_string(val1), as_string(val2)) {
                (Ok(val1), Ok(val2)) => Ok(Value::Bool(val1 == val2)),
                _ => match (as_bool(val1), as_bool(val2)) {
                    (Ok(val1), Ok(val2)) => Ok(Value::Bool(val1 == val2)),
                    _ => Ok(Value::Bool(false)),
                },
            },
        },
        _ => Err(Error::NumArgs(2, vals.to_vec())),
    }
}

pub fn apply_proc(env: &mut Env, vals: &[Value]) -> Result<Value> {
    match vals {
        [func, Value::List(args)] => apply(env, func, args),
        [func, args @ ..] => apply(env, func, args),
        _ => todo!(),
    }
}

pub fn make_read_port(env: &mut Env, vals: &[Value]) -> Result<Value> {
    match vals {
        [Value::String(path)] => env.make_read_port(path),
        _ => todo!(),
    }
}

pub fn make_write_port(env: &mut Env, vals: &[Value]) -> Result<Value> {
    match vals {
        [Value::String(path)] => env.make_write_port(path),
        _ => todo!(),
    }
}

pub fn close_port(env: &mut Env, vals: &[Value]) -> Result<Value> {
    match vals {
        [Value::Port(port_id)] => env.close_port(port_id),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn read_proc(env: &mut Env, vals: &[Value]) -> Result<Value> {
    match vals {
        [] => {
            let mut buf = String::new();
            stdin().read_line(&mut buf).map_err(Error::IO)?;
            parse_expr(&buf).map_err(Error::Parser)
        }
        [Value::Port(port_id)] => {
            let reader = env.get_read_port(port_id)?;
            let mut buf = String::new();
            reader.read_line(&mut buf).map_err(Error::IO)?;
            parse_expr(&buf).map_err(Error::Parser)
        }
        _ => todo!(),
    }
}

pub fn write_proc(env: &mut Env, vals: &[Value]) -> Result<Value> {
    match vals {
        [val] => {
            println!("{}", val);
            Ok(Value::Bool(true))
        }
        [val, Value::Port(port_id)] => {
            let writer = env.get_write_port(port_id)?;
            let buf = val.to_string();
            writer.write_all(buf.as_bytes()).map_err(Error::IO)?;
            Ok(Value::Bool(true))
        }
        _ => todo!(),
    }
}

pub fn read_contents(vals: &[Value]) -> Result<Value> {
    match vals {
        [Value::String(path)] => {
            let lines = std::fs::read_to_string(path).map_err(Error::IO)?;
            Ok(Value::String(lines))
        }
        _ => todo!(),
    }
}

pub fn load(path: &str) -> Result<Vec<Value>> {
    let lines = std::fs::read_to_string(path).map_err(Error::IO)?;
    parse_exprs(&lines).map_err(Error::Parser)
}

pub fn read_all(vals: &[Value]) -> Result<Value> {
    match vals {
        [Value::String(path)] => {
            let vals = load(path)?;
            Ok(Value::List(vals))
        }
        _ => todo!(),
    }
}
