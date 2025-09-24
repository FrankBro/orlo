use crate::{
    env::{self, Env},
    error,
    eval::eval,
    infer,
    parser::parse,
    value::Value,
};

pub enum Error {
    Parse,
    Eval(error::Error),
    Infer(infer::Error),
}

type Result<T, E = Error> = std::result::Result<T, E>;

pub struct The {
    ty: String,
    value: String,
}

impl std::fmt::Display for The {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(the {} {})", self.ty, self.value)
    }
}

pub struct Repl {
    eval: env::Env,
    infer: infer::Env,
}

impl Default for Repl {
    fn default() -> Self {
        Self {
            eval: Env::primitive_bindings(),
            infer: infer::Env::primitive_bindings(),
        }
    }
}

impl Repl {
    fn get_type(&mut self, value: &Value) -> Result<String, infer::Error> {
        let ty = self.infer.infer_value(value)?;
        self.infer.ty_to_string(&ty)
    }

    pub fn handle_input(&mut self, input: &str) -> Result<The, Error> {
        let value = parse(input).map_err(|_| Error::Parse)?;
        let ty = self.get_type(&value).map_err(Error::Infer)?;
        let evaluated = eval(&mut self.eval, &value).map_err(Error::Eval)?;
        Ok(The {
            ty,
            value: evaluated.to_string(),
        })
    }
}
