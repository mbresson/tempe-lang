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
}

impl From<&ExpressionOperator> for Precedence {
    fn from(operator: &ExpressionOperator) -> Self {
        match operator {
            ExpressionOperator::Bang => Precedence::Prefix,
            ExpressionOperator::Plus | ExpressionOperator::Minus => Precedence::Sum,
            ExpressionOperator::Divide | ExpressionOperator::Multiply => Precedence::Product,
            ExpressionOperator::Equal | ExpressionOperator::NotEqual => Precedence::Equals,
            ExpressionOperator::GreaterThan | ExpressionOperator::LessThan => {
                Precedence::LessOrGreater
            }
        }
    }
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

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Integer(i64),
    Boolean(bool),
    PrefixOperation(PrefixOperationExpression),
    InfixOperation(InfixOperationExpression),
    Conditional(ConditionalExpression),
    Function(FunctionExpression),
    FunctionCall(FunctionCallExpression),
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
            Self::Conditional(conditional) => write!(f, "{}", conditional),
            Self::Function(function) => write!(f, "{}", function),
            Self::FunctionCall(function_call) => write!(f, "{}", function_call),
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
    pub prefix_operator: ExpressionOperator,
    pub right_expression: Box<Expression>,
}

impl PrefixOperationExpression {
    pub fn new(prefix_operator: ExpressionOperator, right_expression: Expression) -> Self {
        Self {
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
    pub fn new(
        infix_operator: ExpressionOperator,
        left_expression: Expression,
        right_expression: Expression,
    ) -> Self {
        Self {
            infix_operator,
            left_expression: Box::new(left_expression),
            right_expression: Box::new(right_expression),
        }
    }
}

impl fmt::Display for InfixOperationExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "({} {} {})",
            self.left_expression, self.infix_operator, self.right_expression
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct ConditionalExpression {
    condition: Box<Expression>,
    consequence: BlockStatement,
    alternative: Option<BlockStatement>,
}

impl ConditionalExpression {
    pub fn new(
        condition: Expression,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    ) -> Self {
        Self {
            condition: Box::new(condition),
            consequence,
            alternative,
        }
    }
}

impl fmt::Display for ConditionalExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.alternative {
            Some(alternative) => {
                let ifnot = format!("{} {}", keywords::IF, keywords::NOT);
                write!(
                    f,
                    "{}({}) {{\n{}\n}} {} {{\n{}\n}}",
                    keywords::IF,
                    self.condition,
                    self.consequence,
                    ifnot,
                    alternative
                )
            }
            None => write!(
                f,
                "{}({}) {{\n{}\n}}",
                keywords::IF,
                self.condition,
                self.consequence
            ),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionExpression {
    parameters: Vec<Identifier>,
    body: BlockStatement,
}

impl FunctionExpression {
    pub fn new(parameters: Vec<Identifier>, body: BlockStatement) -> Self {
        Self { parameters, body }
    }
}

impl fmt::Display for FunctionExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}({}) {{\n{}\n}}",
            keywords::FUNCTION,
            self.parameters.iter().format(", "),
            self.body,
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionCallExpression {
    function: Box<Expression>,
    arguments: Vec<Expression>,
}

impl FunctionCallExpression {
    pub fn new(function: Expression, arguments: Vec<Expression>) -> Self {
        Self {
            function: Box::new(function),
            arguments,
        }
    }
}

impl fmt::Display for FunctionCallExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}({})",
            self.function,
            self.arguments.iter().format(", "),
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(Expression),
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
    name: Identifier,
    value: Expression,
}

impl LetStatement {
    pub fn new(name: Identifier, value: Expression) -> Self {
        Self { name, value }
    }
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} = {}", keywords::LET, self.name, self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    value: Expression,
}

impl ReturnStatement {
    pub fn new(value: Expression) -> Self {
        Self { value }
    }
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", keywords::RETURN, self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockStatement {
    statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{\n{};\n}}", self.statements.iter().format(";\n"))
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
