use crate::token::{keywords, Literal};
use itertools::Itertools;
use std::fmt;

trait Node: fmt::Display + fmt::Debug + PartialEq {
    fn token_literal(&self) -> &Literal;
}

pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}

impl fmt::Display for Program<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{};", self.statements.iter().format(";\n"))
    }
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

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl<'a> Node for Identifier {
    fn token_literal(&self) -> &Literal {
        &self.value
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Integer(i64),
    Operation(OperationExpression<'a>),
}

impl fmt::Display for Expression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer(val) => write!(f, "{}", val),
            Self::Operation(expression) => write!(f, "{}", expression),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct OperationExpression<'a> {
    left_expression: &'a Expression<'a>,
    right_expression: &'a Expression<'a>,
}

impl fmt::Display for OperationExpression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.left_expression, self.right_expression)
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Let(LetStatement<'a>),
    Return(ReturnStatement<'a>),
    Expression(ExpressionStatement<'a>),
}

impl fmt::Display for Statement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Let(statement) => write!(f, "{}", statement),
            Self::Return(statement) => write!(f, "{}", statement),
            Self::Expression(statement) => write!(f, "{}", statement),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct LetStatement<'a> {
    token_literal: Literal,
    name: Identifier,
    value: Expression<'a>,
}

impl LetStatement<'_> {
    pub fn new(name: Identifier, value: Expression) -> LetStatement {
        LetStatement {
            token_literal: Literal(String::from(keywords::LET)),
            name,
            value,
        }
    }
}

impl fmt::Display for LetStatement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} = {}", self.token_literal, self.name, self.value)
    }
}

impl<'a> Node for LetStatement<'_> {
    fn token_literal(&self) -> &Literal {
        &self.token_literal
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement<'a> {
    token_literal: Literal,
    value: Expression<'a>,
}

impl ReturnStatement<'_> {
    pub fn new(value: Expression) -> ReturnStatement {
        ReturnStatement {
            token_literal: Literal(String::from(keywords::RETURN)),
            value,
        }
    }
}

impl fmt::Display for ReturnStatement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.token_literal, self.value)
    }
}

impl Node for ReturnStatement<'_> {
    fn token_literal(&self) -> &Literal {
        &self.token_literal
    }
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement<'a> {
    token_literal: Literal,
    expression: Expression<'a>,
}

impl ExpressionStatement<'_> {
    pub fn new(token_literal: Literal, expression: Expression) -> ExpressionStatement {
        ExpressionStatement {
            token_literal,
            expression,
        }
    }
}

impl fmt::Display for ExpressionStatement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}

impl<'a> Node for ExpressionStatement<'_> {
    fn token_literal(&self) -> &Literal {
        &self.token_literal
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement::new(
                Identifier::new(Literal(String::from("myVar"))),
                Expression::Integer(42),
            ))],
        };

        assert_eq!(format!("{}", program), "diketahui myVar = 42;");
    }
}
