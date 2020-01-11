use itertools::Itertools;
use std::collections::HashMap;
use std::fmt;

use super::builtins::{builtins_map, BuiltinFn};
use crate::representations::{
    ast::{BlockStatement, Identifier},
    token::keywords,
};

#[derive(Clone, PartialEq, Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Str(String),
    Array(Vec<Object>),
    HashMap(HashMap<String, Object>),
    Function(Box<FunctionObject>), // Box<...> is required to support function recursion, c.f. interpreter code
    BuiltinFunction(BuiltinFunctionObject),
    EarlyReturnedObject(Box<Object>),
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Integer(value) => write!(f, "{}", value),
            Self::Boolean(value) => write!(f, "{}", value),
            Self::Str(string) => write!(f, "\"{}\"", string.replace("\"", "\\\"")),
            Self::Array(array) => write!(f, "[{}]", array.iter().format(", ")),
            Self::HashMap(hashmap) => {
                if hashmap.is_empty() {
                    write!(f, "{{}}")
                } else {
                    write!(
                        f,
                        "{{\n{}\n}}",
                        hashmap
                            .iter()
                            .map(|(key, value)| format!("\t\"{}\" => {}", key, value))
                            .format(",\n")
                    )
                }
            }
            Self::Function(function) => write!(
                f,
                "{}({}) {{ ... }}",
                keywords::FUNCTION,
                function.parameters.iter().format(", "),
            ),
            Self::BuiltinFunction(function) => write!(
                f,
                "{}({}) {{ ... }}",
                keywords::FUNCTION,
                function.parameters.iter().format(", "),
            ),
            Self::EarlyReturnedObject(value) => write!(f, "{}", value),
            Self::Null => write!(f, "null"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BuiltinFunctionObject {
    pub parameters: Vec<Identifier>,
    pub implementation: Box<BuiltinFn>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionObject {
    pub name: Option<Identifier>,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl FunctionObject {
    pub fn new(parameters: Vec<Identifier>, body: BlockStatement, env: Environment) -> Self {
        Self {
            name: None,
            parameters,
            body,
            env,
        }
    }

    pub fn bind_name(&mut self, name: Identifier) {
        self.name = Some(name);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    variables: HashMap<Identifier, Object>,
}

impl Environment {
    pub fn new_with_builtin_functions() -> Self {
        Self {
            variables: builtins_map()
                .into_iter()
                .map(|(identifier, (parameters, function))| {
                    (
                        identifier,
                        Object::BuiltinFunction(BuiltinFunctionObject {
                            parameters,
                            implementation: function,
                        }),
                    )
                })
                .collect(),
        }
    }

    pub fn get(&self, key: &Identifier) -> Option<&Object> {
        self.variables.get(key)
    }

    pub fn set(&mut self, key: Identifier, object: Object) {
        self.variables.insert(key, object);
    }
}
