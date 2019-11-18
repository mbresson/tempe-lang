use crate::token::{keywords, Literal};
use itertools::Itertools;
use std::fmt;

#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 0,
    Equals,
    LessOrGreater,
    Sum,
    Product,
    Prefix,
    Call, // function calls have the highest precedence
}

impl From<&ExpressionOperator> for Precedence {
    fn from(operator: &ExpressionOperator) -> Self {
        match operator {
            ExpressionOperator::Bang => Precedence::Prefix,
            ExpressionOperator::Plus | ExpressionOperator::Minus => Precedence::Sum,
            ExpressionOperator::Divide | ExpressionOperator::Multiply => Precedence::Product,
            ExpressionOperator::Equal | ExpressionOperator::NotEqual => Precedence::Equals,
            ExpressionOperator::GreaterThan | ExpressionOperator::LessThan => Precedence::LessOrGreater,
        }
    }
}

trait Node: fmt::Display + fmt::Debug + PartialEq {
    fn token_literal(&self) -> &Literal;
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> &Literal {
        &self.value
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Integer(i64),
    Boolean(bool),
    PrefixOperation(PrefixOperationExpression),
    InfixOperation(InfixOperationExpression),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Boolean(true) => write!(f, "{}", keywords::TRUE),
            Self::Boolean(false) => write!(f, "{}", keywords::FALSE),
            Self::Integer(val) => write!(f, "{}", val),
            Self::PrefixOperation(expression) => write!(f, "{}", expression),
            Self::InfixOperation(expression) => write!(f, "{}", expression),
            Self::Identifier(identifier) => write!(f, "{}", identifier),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ExpressionOperator {
    Bang,
    Minus,
    Plus,
    Multiply,
    Divide,
    GreaterThan,
    LessThan,
    Equal,
    NotEqual,
}

impl fmt::Display for ExpressionOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Bang => write!(f, "!"),
            Self::Minus => write!(f, "-"),
            Self::Plus => write!(f, "+"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::GreaterThan => write!(f, ">"),
            Self::LessThan => write!(f, "<"),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct PrefixOperationExpression {
    prefix_operator: ExpressionOperator,
    right_expression: Box<Expression>,
}

impl PrefixOperationExpression {
    pub fn new(prefix_operator: ExpressionOperator, right_expression: Expression) -> Self {
        PrefixOperationExpression {
            prefix_operator,
            right_expression: Box::new(right_expression),
        }
    }
}

impl fmt::Display for PrefixOperationExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}{})", self.prefix_operator, self.right_expression)
    }
}

#[derive(Debug, PartialEq)]
pub struct InfixOperationExpression {
    infix_operator: ExpressionOperator,
    left_expression: Box<Expression>,
    right_expression: Box<Expression>,
}

impl InfixOperationExpression {
    pub fn new(infix_operator: ExpressionOperator, left_expression: Expression, right_expression: Expression) -> Self {
        InfixOperationExpression {
            infix_operator,
            left_expression: Box::new(left_expression),
            right_expression: Box::new(right_expression),
        }
    }
}

impl fmt::Display for InfixOperationExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left_expression, self.infix_operator, self.right_expression)
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Let(statement) => write!(f, "{}", statement),
            Self::Return(statement) => write!(f, "{}", statement),
            Self::Expression(statement) => write!(f, "{}", statement),
        }
    }
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
            token_literal: Literal(keywords::LET.to_string()),
            name,
            value,
        }
    }
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} = {}", self.token_literal, self.name, self.value)
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> &Literal {
        &self.token_literal
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    token_literal: Literal,
    value: Expression,
}

impl ReturnStatement {
    pub fn new(value: Expression) -> ReturnStatement {
        ReturnStatement {
            token_literal: Literal(keywords::RETURN.to_string()),
            value,
        }
    }
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.token_literal, self.value)
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> &Literal {
        &self.token_literal
    }
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    token_literal: Literal,
    expression: Expression,
}

impl ExpressionStatement {
    pub fn new(expression: Expression) -> ExpressionStatement {
        ExpressionStatement {
            token_literal: Literal(format!("{}", expression)),
            expression,
        }
    }
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}

impl Node for ExpressionStatement {
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
                Identifier::new(Literal("myVar".to_string())),
                Expression::Integer(42),
            ))],
        };

        assert_eq!(format!("{}", program), "diketahui myVar = 42;");
    }
}
