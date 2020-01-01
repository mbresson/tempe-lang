use std::collections::HashMap;
use std::fmt;

use crate::representations::ast::Identifier;

#[derive(Clone, PartialEq, Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    EarlyReturnedObject(Box<Object>),
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::EarlyReturnedObject(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
        }
    }
}

pub struct Environment {
    variables: HashMap<Identifier, Object>,
}

impl<'env> Environment {
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
