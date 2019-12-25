use crate::representations::ast::{
    ConditionalExpression, Expression, ExpressionOperator, InfixOperationExpression,
    PrefixOperationExpression, Program, Statement,
};
use std::fmt;

#[derive(PartialEq, Debug)]
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

fn eval_integer_infix_operation(
    operator: &ExpressionOperator,
    left_value: i64,
    right_value: i64,
) -> Object {
    match operator {
        ExpressionOperator::Plus => Object::Integer(left_value + right_value),
        ExpressionOperator::Minus => Object::Integer(left_value - right_value),
        ExpressionOperator::Multiply => Object::Integer(left_value * right_value),
        ExpressionOperator::Divide => Object::Integer(left_value / right_value),
        ExpressionOperator::Equal => Object::Boolean(left_value == right_value),
        ExpressionOperator::NotEqual => Object::Boolean(left_value != right_value),
        ExpressionOperator::GreaterThan => Object::Boolean(left_value > right_value),
        ExpressionOperator::LessThan => Object::Boolean(left_value < right_value),
        _ => todo!("infix operation with {}", operator),
    }
}

fn eval_boolean_infix_operation(
    operator: &ExpressionOperator,
    left_value: bool,
    right_value: bool,
) -> Object {
    match operator {
        ExpressionOperator::Equal => Object::Boolean(left_value == right_value),
        ExpressionOperator::NotEqual => Object::Boolean(left_value != right_value),
        _ => todo!("infix operation with {}", operator),
    }
}

fn eval_infix_operation(
    operator: &ExpressionOperator,
    left_value: Object,
    right_value: Object,
) -> Object {
    match (left_value, right_value) {
        (Object::Integer(left_integer), Object::Integer(right_integer)) => {
            eval_integer_infix_operation(operator, left_integer, right_integer)
        }
        (Object::Boolean(left_bool), Object::Boolean(right_bool)) => {
            eval_boolean_infix_operation(operator, left_bool, right_bool)
        }
        _ => todo!("infix operation with {}", operator),
    }
}

fn is_truthy(object: Object) -> bool {
    let is_false_or_null = object == Object::Boolean(false) || object == Object::Null;

    !is_false_or_null
}

fn eval_conditional_expression(conditional_expression: &ConditionalExpression) -> Object {
    let condition = eval_expression(&conditional_expression.condition);

    if is_truthy(condition) {
        eval_block_statement(&conditional_expression.consequence.statements)
    } else if let Some(alternative) = &conditional_expression.alternative {
        eval_block_statement(&alternative.statements)
    } else {
        Object::Null
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
        Expression::InfixOperation(InfixOperationExpression {
            left_expression,
            infix_operator,
            right_expression,
        }) => {
            let left_value = eval_expression(left_expression);
            let right_value = eval_expression(right_expression);
            eval_infix_operation(infix_operator, left_value, right_value)
        }
        Expression::Conditional(conditional_expression) => {
            eval_conditional_expression(conditional_expression)
        }
        expression => todo!("expression evaluation for {}", expression),
    }
}

fn eval_statement(statement: &Statement) -> Object {
    match statement {
        Statement::Expression(expression) => eval_expression(expression),
        Statement::Return(return_statement) => {
            Object::EarlyReturnedObject(Box::new(eval_expression(&return_statement.value)))
        }
        _ => todo!("statement evaluation for {}", statement),
    }
}

pub fn eval_block_statement(statements: &[Statement]) -> Object {
    let mut result = Object::Null;

    for statement in statements {
        result = eval_statement(statement);

        if let Object::EarlyReturnedObject(_) = result {
            break;
        }
    }

    result
}

pub fn eval_program(program: &Program) -> Object {
    match eval_block_statement(&program.statements) {
        Object::EarlyReturnedObject(value) => *value,
        value => value,
    }
}

#[cfg(test)]
mod tests {
    use super::{eval_statement, Object};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::representations::ast::{
        Expression, ExpressionOperator, PrefixOperationExpression, Statement,
    };

    fn parse_eval(input: &str) -> Result<Object, String> {
        let mut lexer = Lexer::new(input);

        let program = Parser::new(&mut lexer)
            .parse_program()
            .map_err(|errors| format!("parse_program returned errors {:?}", errors))?;

        Ok(super::eval_program(&program))
    }

    #[test]
    fn eval_if_else_expressions() {
        let inputs_to_expected_objects = vec![
            ("jika (benar) { 10 }", Object::Integer(10)),
            ("jika (salah) { 10 }", Object::Null),
            ("jika (1) { 10 }", Object::Integer(10)),
            ("jika (1 < 2) { 10 }", Object::Integer(10)),
            ("jika (1 > 2) { 10 }", Object::Null),
            ("jika (1 > 2) { 10 } jika tidak { 20 }", Object::Integer(20)),
            ("jika (1 < 2) { 10 } jika tidak { 20 }", Object::Integer(10)),
        ];

        for (input, expected_object) in inputs_to_expected_objects {
            let object = parse_eval(input).unwrap();

            assert_eq!(object, expected_object);
        }
    }

    #[test]
    fn eval_infix_operations() {
        let inputs_to_expected_objects = vec![
            ("5 + 5 + 5 + 5 - 10", Object::Integer(10)),
            ("2 * 2 * 2 * 2 * 2", Object::Integer(32)),
            ("-50 + 100 + -50", Object::Integer(0)),
            ("5 * 2 + 10", Object::Integer(20)),
            ("5 + 2 * 10", Object::Integer(25)),
            ("20 + 2 * -10", Object::Integer(0)),
            ("50 / 2 * 2 + 10", Object::Integer(60)),
            ("2 * (5 + 10)", Object::Integer(30)),
            ("3 * 3 * 3 + 10", Object::Integer(37)),
            ("3 * (3 * 3) + 10", Object::Integer(37)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50)),
            ("1 < 2", Object::Boolean(true)),
            ("1 > 2", Object::Boolean(false)),
            ("1 < 1", Object::Boolean(false)),
            ("1 > 1", Object::Boolean(false)),
            ("1 == 1", Object::Boolean(true)),
            ("1 != 1", Object::Boolean(false)),
            ("1 == 2", Object::Boolean(false)),
            ("1 != 2", Object::Boolean(true)),
        ];

        for (input, expected_object) in inputs_to_expected_objects {
            let object = parse_eval(input).unwrap();

            assert_eq!(object, expected_object);
        }
    }

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
        let inputs_to_expected_objects = vec![
            ("benar", Object::Boolean(true)),
            ("salah", Object::Boolean(false)),
            ("benar == benar", Object::Boolean(true)),
            ("salah == salah", Object::Boolean(true)),
            ("benar == salah", Object::Boolean(false)),
            ("benar != salah", Object::Boolean(true)),
            ("salah != benar", Object::Boolean(true)),
            ("(1 < 2) == benar", Object::Boolean(true)),
            ("(1 < 2) == salah", Object::Boolean(false)),
            ("(1 > 2) == benar", Object::Boolean(false)),
            ("(1 > 2) == salah", Object::Boolean(true)),
        ];

        for (input, expected_object) in inputs_to_expected_objects {
            let object = parse_eval(input).unwrap();

            assert_eq!(object, expected_object);
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

    #[test]
    fn eval_return_statement() {
        let inputs_to_expected_objects = vec![
            ("kembalikan 10;", Object::Integer(10)),
            ("kembalikan 10; 9;", Object::Integer(10)),
            ("kembalikan 2 * 5; 9;", Object::Integer(10)),
            ("9; kembalikan 2 * 5; 9;", Object::Integer(10)),
            (
                "
                jika (10 > 1) {
                    jika (10 > 1) {
                        kembalikan 10;
                    }

                    kembalikan 1;
                }
                ",
                Object::Integer(10),
            ),
        ];

        for (input, expected_object) in inputs_to_expected_objects {
            let object = parse_eval(input).unwrap();

            assert_eq!(object, expected_object);
        }
    }
}
