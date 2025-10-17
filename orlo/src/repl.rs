use crate::{
    env::{self, Env},
    error,
    eval::eval,
    expander::Expander,
    infer,
    parser::{parse, parse_multiple},
    value::Value,
};

#[derive(Debug)]
pub enum Error {
    Parse,
    Eval(error::Error),
    Infer(infer::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Parse => write!(f, "Parse error"),
            Error::Eval(e) => write!(f, "{e}"),
            Error::Infer(e) => write!(f, "{e}"),
        }
    }
}

type Result<T, E = Error> = std::result::Result<T, E>;

pub struct The {
    pub ty: String,
    pub value: String,
}

impl std::fmt::Display for The {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(the {} {})", self.ty, self.value)
    }
}

pub struct Repl {
    eval: env::Env,
    infer: infer::Env,
    expander: Expander,
}

impl Default for Repl {
    fn default() -> Self {
        Self {
            eval: Env::primitive_bindings(),
            infer: infer::Env::primitive_bindings(),
            expander: Expander::new(),
        }
    }
}

impl Repl {
    fn get_type(&mut self, value: &Value) -> Result<String, infer::Error> {
        let ty = self.infer.infer_value(value)?;
        self.infer.generalize(-1, &ty).unwrap();
        self.infer.ty_to_string(&ty)
    }

    fn handle_value(&mut self, value: &Value) -> Result<The, Error> {
        let expanded = self.expander.expand(value).map_err(|_e| Error::Parse)?;
        let ty = self.get_type(&expanded).map_err(Error::Infer)?;
        let evaluated = eval(&mut self.eval, &expanded).map_err(Error::Eval)?;
        Ok(The {
            ty,
            value: evaluated.to_string(),
        })
    }

    pub fn handle_input(&mut self, input: &str) -> Result<The, Error> {
        let value = parse(input).map_err(|_| Error::Parse)?;
        self.handle_value(&value)
    }

    pub fn handle_file(&mut self, path: &str) -> Result<Vec<Result<The, Error>>, Error> {
        let content = std::fs::read_to_string(path).map_err(|_| Error::Parse)?;
        let values = parse_multiple(&content).map_err(|_| Error::Parse)?;
        let mut output = Vec::new();
        for value in values {
            let res = self.handle_value(&value);
            output.push(res);
        }
        Ok(output)
    }
}
