use crate::ast::{Expression, ExpressionOperator, PrefixOperationExpression, Statement};
use std::fmt;

#[derive(PartialEq, Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
        }
    }
}

fn eval_bang_operator_expression(value: Object) -> Object {
    match value {
        Object::Boolean(value) => Object::Boolean(!value),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_minus_prefix_operator_expression(value: Object) -> Object {
    match value {
        Object::Integer(value) => Object::Integer(-value),
        object => todo!("minus operation with {}", object),
    }
}

fn eval_prefix_operation(operator: &ExpressionOperator, right_value: Object) -> Object {
    match operator {
        ExpressionOperator::Bang => eval_bang_operator_expression(right_value),
        ExpressionOperator::Minus => eval_minus_prefix_operator_expression(right_value),
        _ => todo!("prefix operation with {}", operator),
    }
}

fn eval_expression(expression: &Expression) -> Object {
    match expression {
        Expression::Boolean(value) => Object::Boolean(*value),
        Expression::Integer(value) => Object::Integer(*value),
        Expression::PrefixOperation(PrefixOperationExpression {
            prefix_operator,
            right_expression,
        }) => {
            let right_value = eval_expression(right_expression);
            eval_prefix_operation(prefix_operator, right_value)
        }
        expression => todo!("expression evaluation for {}", expression),
    }
}

fn eval_statement(statement: &Statement) -> Object {
    match statement {
        Statement::Expression(expression) => eval_expression(expression),
        _ => todo!("statement evaluation for {}", statement),
    }
}

pub fn eval_statements(statements: &Vec<Statement>) -> Object {
    let mut result = Object::Null;

    for statement in statements {
        result = eval_statement(statement);
    }

    result
}

#[cfg(test)]
mod tests {
    use super::{eval_statement, Object};
    use crate::ast::{Expression, ExpressionOperator, PrefixOperationExpression, Statement};

    #[test]
    fn eval_integer_expression() {
        let statements_to_expected_objects = vec![
            (
                Statement::Expression(Expression::Integer(42)),
                Object::Integer(42),
            ),
            (
                Statement::Expression(Expression::Integer(-1)),
                Object::Integer(-1),
            ),
            (
                Statement::Expression(Expression::PrefixOperation(PrefixOperationExpression {
                    prefix_operator: ExpressionOperator::Minus,
                    right_expression: Box::new(Expression::Integer(4)),
                })),
                Object::Integer(-4),
            ),
            (
                Statement::Expression(Expression::PrefixOperation(PrefixOperationExpression {
                    prefix_operator: ExpressionOperator::Minus,
                    right_expression: Box::new(Expression::Integer(-4)),
                })),
                Object::Integer(4),
            ),
        ];

        for (statement, expected_object) in statements_to_expected_objects {
            assert_eq!(eval_statement(&statement), expected_object);
        }
    }

    #[test]
    fn eval_boolean_expression() {
        let statements_to_expected_objects = vec![
            (
                Statement::Expression(Expression::Boolean(true)),
                Object::Boolean(true),
            ),
            (
                Statement::Expression(Expression::Boolean(false)),
                Object::Boolean(false),
            ),
        ];

        for (statement, expected_object) in statements_to_expected_objects {
            assert_eq!(eval_statement(&statement), expected_object);
        }
    }

    #[test]
    fn eval_bang_operator() {
        let statements_to_expected_objects = vec![
            (
                Statement::Expression(Expression::PrefixOperation(PrefixOperationExpression {
                    prefix_operator: ExpressionOperator::Bang,
                    right_expression: Box::new(Expression::Boolean(true)),
                })),
                Object::Boolean(false),
            ),
            (
                Statement::Expression(Expression::PrefixOperation(PrefixOperationExpression {
                    prefix_operator: ExpressionOperator::Bang,
                    right_expression: Box::new(Expression::Boolean(false)),
                })),
                Object::Boolean(true),
            ),
            (
                Statement::Expression(Expression::PrefixOperation(PrefixOperationExpression {
                    prefix_operator: ExpressionOperator::Bang,
                    right_expression: Box::new(Expression::Integer(2)),
                })),
                Object::Boolean(false),
            ),
            (
                Statement::Expression(Expression::PrefixOperation(PrefixOperationExpression {
                    prefix_operator: ExpressionOperator::Bang,
                    right_expression: Box::new(Expression::PrefixOperation(
                        PrefixOperationExpression {
                            prefix_operator: ExpressionOperator::Bang,
                            right_expression: Box::new(Expression::Boolean(true)),
                        },
                    )),
                })),
                Object::Boolean(true),
            ),
        ];

        for (statement, expected_object) in statements_to_expected_objects {
            assert_eq!(eval_statement(&statement), expected_object);
        }
    }
}
