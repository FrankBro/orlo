use std::sync::Arc;

use crate::{
    env::{self, Env},
    error,
    eval::eval,
    expander::Expander,
    infer,
    parser::{parse, parse_multiple},
    typing::Type,
    value::{ForeignFunc, Value},
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

impl std::error::Error for Error {}

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

    /// Create a builder for registering a foreign function
    pub fn register_foreign(&mut self) -> ForeignFuncBuilder<'_> {
        ForeignFuncBuilder::new(self)
    }
}

/// Builder for registering foreign functions in the REPL
pub struct ForeignFuncBuilder<'a> {
    repl: &'a mut Repl,
    name: Option<String>,
    ty: Option<Type>,
    handler: Option<Arc<dyn Fn(&[Value]) -> Result<Value, error::Error> + Send + Sync>>,
}

impl<'a> ForeignFuncBuilder<'a> {
    fn new(repl: &'a mut Repl) -> Self {
        Self {
            repl,
            name: None,
            ty: None,
            handler: None,
        }
    }

    /// Set the name of the foreign function
    pub fn name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Set the type signature by parsing a string
    ///
    /// Example signatures:
    /// - "(lambda (int) int)" - function taking int, returning int
    /// - "(lambda (int int) int)" - function taking two ints, returning int
    /// - "(lambda (string string) string)" - function taking two strings, returning string
    pub fn signature(mut self, sig: &str) -> Result<Self, Error> {
        let ty = crate::typing::parse(sig).map_err(|_| Error::Parse)?;
        let ty = self.repl.infer.replace_ty_constants_with_vars(ty.0, ty.1);
        self.ty = Some(ty);
        Ok(self)
    }

    /// Set the type signature directly
    pub fn ty(mut self, ty: Type) -> Self {
        self.ty = Some(ty);
        self
    }

    /// Set the handler function
    pub fn handler<F>(mut self, handler: F) -> Self
    where
        F: Fn(&[Value]) -> Result<Value, error::Error> + Send + Sync + 'static,
    {
        self.handler = Some(Arc::new(handler));
        self
    }

    /// Build and register the foreign function
    pub fn build(self) -> Result<(), Error> {
        let name = self.name.ok_or(Error::Parse)?;
        let ty = self.ty.ok_or(Error::Parse)?;
        let handler = self.handler.ok_or(Error::Parse)?;

        // Calculate arity from type signature
        let arity = count_params(&ty);

        // Create the foreign function value
        let foreign_fn = Value::ForeignFunc(ForeignFunc {
            name: name.clone(),
            arity,
            handler,
        });

        // Register in eval environment
        self.repl.eval.define_var(name.clone(), foreign_fn);

        // Register type in infer environment
        self.repl.infer.vars.insert(name, ty);

        Ok(())
    }
}

/// Count the number of parameters in a function type
fn count_params(ty: &Type) -> usize {
    match ty {
        Type::Arrow(params, _) => count_list_params(params),
        _ => 0,
    }
}

fn count_list_params(ty: &Type) -> usize {
    match ty {
        Type::ListNil => 0,
        Type::ListVarArg(_) => 0, // varargs don't count toward required params
        Type::ListCons(_, tail) => 1 + count_list_params(tail),
        _ => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_foreign_function_registration() {
        let mut repl = Repl::default();

        // Register a simple add-ten function
        repl.register_foreign()
            .name("add-ten")
            .signature("(lambda (int) int)")
            .unwrap()
            .handler(|args| match &args[0] {
                Value::Number(n) => Ok(Value::Number(n + 10)),
                v => Err(error::Error::TypeMismatch("int".to_string(), v.clone())),
            })
            .build()
            .unwrap();

        // Test calling the foreign function
        let result = repl.handle_input("(add-ten 5)").unwrap();
        assert_eq!(result.value, "15");
        assert_eq!(result.ty, "int");
    }

    #[test]
    fn test_foreign_function_two_params() {
        let mut repl = Repl::default();

        // Register a string concatenation function
        repl.register_foreign()
            .name("string-concat")
            .signature("(lambda (string string) string)")
            .unwrap()
            .handler(|args| match (&args[0], &args[1]) {
                (Value::String(s1), Value::String(s2)) => {
                    Ok(Value::String(format!("{}{}", s1, s2)))
                }
                (v, _) => Err(error::Error::TypeMismatch("string".to_string(), v.clone())),
            })
            .build()
            .unwrap();

        // Test calling the foreign function
        let result = repl
            .handle_input("(string-concat \"Hello, \" \"World!\")")
            .unwrap();
        assert_eq!(result.value, "\"Hello, World!\"");
        assert_eq!(result.ty, "string");
    }

    #[test]
    fn test_foreign_function_with_define() {
        let mut repl = Repl::default();

        // Register add-ten function
        repl.register_foreign()
            .name("add-ten")
            .signature("(lambda (int) int)")
            .unwrap()
            .handler(|args| match &args[0] {
                Value::Number(n) => Ok(Value::Number(n + 10)),
                v => Err(error::Error::TypeMismatch("int".to_string(), v.clone())),
            })
            .build()
            .unwrap();

        // Define a function that uses the foreign function
        repl.handle_input("(define (add-twenty x) (add-ten (add-ten x)))")
            .unwrap();

        // Test calling the defined function
        let result = repl.handle_input("(add-twenty 5)").unwrap();
        assert_eq!(result.value, "25");
        assert_eq!(result.ty, "int");
    }
}
