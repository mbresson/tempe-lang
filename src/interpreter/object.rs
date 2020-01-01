use itertools::Itertools;
use std::collections::HashMap;
use std::fmt;

use crate::representations::{
    ast::{BlockStatement, Identifier},
    token::keywords,
};

#[derive(Clone, PartialEq, Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Function(Box<FunctionObject>), // Box<...> is required to support function recursion, c.f. interpreter code
    EarlyReturnedObject(Box<Object>),
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Function(function) => write!(
                f,
                "{}({}) {{\n{}\n}}",
                keywords::FUNCTION,
                function.parameters.iter().format(", "),
                function.body,
            ),
            Object::EarlyReturnedObject(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
        }
    }
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
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn get(&self, key: &Identifier) -> Option<&Object> {
        self.variables.get(key)
    }

    pub fn set(&mut self, key: Identifier, object: Object) {
        self.variables.insert(key, object);
    }
}
