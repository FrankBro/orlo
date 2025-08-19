use std::{
    collections::HashMap,
    fs::File,
    io::{BufReader, BufWriter},
};

use crate::{
    error::Error,
    value::{IOFunc, PrimitiveFunc, Value},
};

type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct Closure {
    vars: HashMap<String, usize>,
}

#[derive(Debug)]
enum Port {
    ReadPort(BufReader<File>),
    WritePort(BufWriter<File>),
}

// TODO: Will grow forever, thought about saving vals.len() and then use vec.truncate
// but I think we'd lose some captured variables that don't live long enough?
#[derive(Default, Debug)]
pub struct Env {
    vals: Vec<Value>,
    vars: HashMap<String, usize>,
    next_port_id: usize,
    ports: HashMap<usize, Port>,
}

impl Env {
    pub fn get_var(&self, var: &str) -> Result<&Value> {
        match self.vars.get(var) {
            Some(i) => Ok(&self.vals[*i]),
            None => Err(Error::UnboundVar(
                "Getting an unbound variable".to_owned(),
                var.to_owned(),
            )),
        }
    }

    pub fn set_var(&mut self, var: &str, val: Value) -> Result<Value> {
        match self.vars.get(var) {
            Some(i) => {
                self.vals[*i] = val.clone();
                Ok(val)
            }
            None => Err(Error::UnboundVar(
                "Setting an unbound var".to_owned(),
                var.to_owned(),
            )),
        }
    }

    pub fn define_var(&mut self, var: String, val: Value) -> Value {
        let i = self.vals.len();
        self.vars.insert(var, i);
        self.vals.push(val.clone());
        val
    }

    pub fn make_closure(&mut self) -> Closure {
        let vars = self.vars.clone();
        Closure { vars }
    }

    pub fn with_closure(&mut self, closure: &Closure) {
        for (var, val) in &closure.vars {
            self.vars.insert(var.to_owned(), *val);
        }
    }

    pub fn load_closure(&mut self, closure: Closure) {
        self.vars = closure.vars;
    }

    pub fn make_read_port(&mut self, path: &str) -> Result<Value> {
        let file = File::open(path).map_err(Error::IO)?;
        let reader = BufReader::new(file);
        let port_id = self.next_port_id;
        self.next_port_id += 1;
        self.ports.insert(port_id, Port::ReadPort(reader));
        Ok(Value::Port(port_id))
    }

    pub fn make_write_port(&mut self, path: &str) -> Result<Value> {
        let file = File::open(path).map_err(Error::IO)?;
        let writer = BufWriter::new(file);
        let port_id = self.next_port_id;
        self.next_port_id += 1;
        self.ports.insert(port_id, Port::WritePort(writer));
        Ok(Value::Port(port_id))
    }

    pub fn close_port(&mut self, port_id: &usize) -> Result<Value> {
        self.ports.remove(port_id);
        Ok(Value::Bool(true))
    }

    pub fn get_read_port(&mut self, port_id: &usize) -> Result<&mut BufReader<File>> {
        if let Some(Port::ReadPort(reader)) = self.ports.get_mut(port_id) {
            return Ok(reader);
        }
        Err(Error::Port(
            "Port was not opened, was closed or is not a read port".to_owned(),
        ))
    }

    pub fn get_write_port(&mut self, port_id: &usize) -> Result<&mut BufWriter<File>> {
        if let Some(Port::WritePort(writer)) = self.ports.get_mut(port_id) {
            return Ok(writer);
        }
        Err(Error::Port(
            "Port was not opened, was closed or is not a write port".to_owned(),
        ))
    }

    pub fn primitive_bindings() -> Self {
        let mut env = Env::default();
        fn define_primitive_func(env: &mut Env, name: &str, func: PrimitiveFunc) {
            env.define_var(name.to_owned(), Value::PrimitiveFunc(func));
        }
        fn define_io_func(env: &mut Env, name: &str, func: IOFunc) {
            env.define_var(name.to_owned(), Value::IOFunc(func));
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
        define_io_func(&mut env, "open-input-file", IOFunc::MakeReadPort);
        define_io_func(&mut env, "open-output-file", IOFunc::MakeWritePort);
        define_io_func(&mut env, "close-input-port", IOFunc::ClosePort);
        define_io_func(&mut env, "close-output-port", IOFunc::ClosePort);
        define_io_func(&mut env, "read", IOFunc::Read);
        define_io_func(&mut env, "write", IOFunc::Write);
        define_io_func(&mut env, "read-contents", IOFunc::ReadContents);
        define_io_func(&mut env, "read-all", IOFunc::ReadAll);
        env
    }
}
