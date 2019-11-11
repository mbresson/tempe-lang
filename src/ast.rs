use crate::token::{keywords, Literal};
use std::fmt::Debug;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Integer(i64),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    token_literal: Literal,
    name: Identifier,
    value: Expression,
}

impl LetStatement {
    pub fn new(name: Identifier, value: Expression) -> LetStatement {
        LetStatement {
            token_literal: Literal(String::from(keywords::LET)),
            name,
            value,
        }
    }
}

impl<'a> Node for LetStatement {
    fn token_literal(&self) -> &Literal {
        &self.token_literal
    }
}

trait Node: Debug + PartialEq {
    fn token_literal(&self) -> &Literal;
}

pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    value: Literal,
}

impl Identifier {
    pub fn new(literal: Literal) -> Identifier {
        Identifier { value: literal }
    }
}

impl<'a> Node for Identifier {
    fn token_literal(&self) -> &Literal {
        &self.value
    }
}
