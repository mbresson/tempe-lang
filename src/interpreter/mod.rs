use crate::representations::ast::{
    ConditionalExpression, Expression, ExpressionOperator, Identifier, InfixOperationExpression,
    PrefixOperationExpression, Program, Statement,
};

pub mod errors;
pub mod object;

use errors::{ErrorKind, Result as InterpretingResult};
use object::{Environment, Object};

fn eval_bang_operator_expression(value: Object) -> Object {
    match value {
        Object::Boolean(value) => Object::Boolean(!value),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_minus_prefix_operator_expression(value: Object) -> InterpretingResult<Object> {
    match value {
        Object::Integer(value) => Ok(Object::Integer(-value)),
        object => Err(ErrorKind::UnknownPrefixOperator(ExpressionOperator::Minus, object).into()),
    }
}

fn eval_prefix_operation(
    operator: ExpressionOperator,
    right_value: Object,
) -> InterpretingResult<Object> {
    match operator {
        ExpressionOperator::Bang => Ok(eval_bang_operator_expression(right_value)),
        ExpressionOperator::Minus => eval_minus_prefix_operator_expression(right_value),
        _ => Err(ErrorKind::UnknownPrefixOperator(operator, right_value).into()),
    }
}

fn eval_integer_infix_operation(
    operator: ExpressionOperator,
    left_value: i64,
    right_value: i64,
) -> InterpretingResult<Object> {
    let result = match operator {
        ExpressionOperator::Plus => Object::Integer(left_value + right_value),
        ExpressionOperator::Minus => Object::Integer(left_value - right_value),
        ExpressionOperator::Multiply => Object::Integer(left_value * right_value),
        ExpressionOperator::Divide => Object::Integer(left_value / right_value),
        ExpressionOperator::Equal => Object::Boolean(left_value == right_value),
        ExpressionOperator::NotEqual => Object::Boolean(left_value != right_value),
        ExpressionOperator::GreaterThan => Object::Boolean(left_value > right_value),
        ExpressionOperator::LessThan => Object::Boolean(left_value < right_value),
        _ => {
            return Err(ErrorKind::UnknownInfixOperator(
                operator,
                Object::Integer(left_value),
                Object::Integer(right_value),
            )
            .into());
        }
    };

    Ok(result)
}

fn eval_boolean_infix_operation(
    operator: ExpressionOperator,
    left_value: bool,
    right_value: bool,
) -> InterpretingResult<Object> {
    match operator {
        ExpressionOperator::Equal => Ok(Object::Boolean(left_value == right_value)),
        ExpressionOperator::NotEqual => Ok(Object::Boolean(left_value != right_value)),
        _ => Err(ErrorKind::UnknownInfixOperator(
            operator,
            Object::Boolean(left_value),
            Object::Boolean(right_value),
        )
        .into()),
    }
}

fn eval_infix_operation(
    operator: ExpressionOperator,
    left_value: Object,
    right_value: Object,
) -> InterpretingResult<Object> {
    match (left_value, right_value) {
        (Object::Integer(left_integer), Object::Integer(right_integer)) => {
            eval_integer_infix_operation(operator, left_integer, right_integer)
        }
        (Object::Boolean(left_bool), Object::Boolean(right_bool)) => {
            eval_boolean_infix_operation(operator, left_bool, right_bool)
        }
        (left_value, right_value) => Err(ErrorKind::TypeMismatch(left_value, right_value).into()),
    }
}

fn is_truthy(object: Object) -> bool {
    let is_false_or_null = object == Object::Boolean(false) || object == Object::Null;

    !is_false_or_null
}

fn eval_conditional_expression(
    conditional_expression: &ConditionalExpression,
    environment: &mut Environment,
) -> InterpretingResult<Object> {
    let condition = eval_expression(&conditional_expression.condition, environment)?;

    if is_truthy(condition) {
        eval_block_statement(&conditional_expression.consequence.statements, environment)
    } else if let Some(alternative) = &conditional_expression.alternative {
        eval_block_statement(&alternative.statements, environment)
    } else {
        Ok(Object::Null)
    }
}

fn eval_expression(
    expression: &Expression,
    environment: &mut Environment,
) -> InterpretingResult<Object> {
    match expression {
        Expression::Boolean(value) => Ok(Object::Boolean(*value)),
        Expression::Integer(value) => Ok(Object::Integer(*value)),
        Expression::PrefixOperation(PrefixOperationExpression {
            prefix_operator,
            right_expression,
        }) => {
            let right_value = eval_expression(right_expression, environment)?;
            eval_prefix_operation(*prefix_operator, right_value)
        }
        Expression::InfixOperation(InfixOperationExpression {
            left_expression,
            infix_operator,
            right_expression,
        }) => {
            let left_value = eval_expression(left_expression, environment)?;
            let right_value = eval_expression(right_expression, environment)?;
            eval_infix_operation(*infix_operator, left_value, right_value)
        }
        Expression::Conditional(conditional_expression) => {
            eval_conditional_expression(conditional_expression, environment)
        }
        Expression::Identifier(identifier) => eval_identifier(&identifier, environment),
        expression => todo!("expression evaluation for {}", expression),
    }
}

fn eval_identifier(
    identifier: &Identifier,
    environment: &Environment,
) -> InterpretingResult<Object> {
    match environment.get(identifier) {
        Some(value) => Ok(value.clone()),
        None => Err(ErrorKind::IdentifierNotFound(identifier.clone()).into()),
    }
}

fn eval_statement(
    statement: &Statement,
    environment: &mut Environment,
) -> InterpretingResult<Object> {
    match statement {
        Statement::Expression(expression) => eval_expression(expression, environment),
        Statement::Return(return_statement) => {
            let value = eval_expression(&return_statement.value, environment)?;
            Ok(Object::EarlyReturnedObject(Box::new(value)))
        }
        Statement::Let(let_statement) => {
            let value = eval_expression(&let_statement.value, environment)?;

            environment.set(let_statement.name.clone(), value.clone());
            Ok(value)
        }
    }
}

pub fn eval_block_statement(
    statements: &[Statement],
    environment: &mut Environment,
) -> InterpretingResult<Object> {
    let mut result = Object::Null;

    for statement in statements {
        result = eval_statement(statement, environment)?;

        if let Object::EarlyReturnedObject(_) = result {
            break;
        }
    }

    Ok(result)
}

pub fn eval_program(
    program: &Program,
    environment: &mut Environment,
) -> InterpretingResult<Object> {
    Ok(
        match eval_block_statement(&program.statements, environment)? {
            Object::EarlyReturnedObject(value) => *value,
            value => value,
        },
    )
}

#[cfg(test)]
mod tests {
    use super::errors::{ErrorKind, Result as InterpretingResult};
    use super::{Environment, Object};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::representations::ast::{ExpressionOperator, Identifier};
    use crate::representations::token::Literal;

    fn parse_eval(input: &str) -> InterpretingResult<Object> {
        let mut lexer = Lexer::new(input);

        let program = Parser::new(&mut lexer)
            .parse_program()
            .map_err(|errors| format!("parse_program returned errors {:?}", errors))?;

        let mut environment = Environment::new();

        super::eval_program(&program, &mut environment)
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
    fn eval_integer_expressions() {
        let inputs_to_expected_objects = vec![
            ("42", Object::Integer(42)),
            ("-1", Object::Integer(-1)),
            ("-4", Object::Integer(-4)),
            ("--4", Object::Integer(4)),
        ];

        for (input, expected_object) in inputs_to_expected_objects {
            let object = parse_eval(input).unwrap();

            assert_eq!(object, expected_object);
        }
    }

    #[test]
    fn eval_boolean_expressions() {
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
    fn eval_bang_operators() {
        let inputs_to_expected_objects = vec![
            ("!benar", Object::Boolean(false)),
            ("!salah", Object::Boolean(true)),
            ("!2", Object::Boolean(false)),
            ("!!benar", Object::Boolean(true)),
        ];

        for (input, expected_object) in inputs_to_expected_objects {
            let object = parse_eval(input).unwrap();

            assert_eq!(object, expected_object);
        }
    }

    #[test]
    fn eval_return_statements() {
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

    #[test]
    fn error_handling() {
        let inputs_to_expected_errors = vec![
            (
                "5 + benar;",
                ErrorKind::TypeMismatch(Object::Integer(5), Object::Boolean(true)),
            ),
            (
                "5 + benar; 5;",
                ErrorKind::TypeMismatch(Object::Integer(5), Object::Boolean(true)),
            ),
            (
                "-benar;",
                ErrorKind::UnknownPrefixOperator(ExpressionOperator::Minus, Object::Boolean(true)),
            ),
            (
                "benar + salah;",
                ErrorKind::UnknownInfixOperator(
                    ExpressionOperator::Plus,
                    Object::Boolean(true),
                    Object::Boolean(false),
                ),
            ),
            (
                "5; benar + salah; 5",
                ErrorKind::UnknownInfixOperator(
                    ExpressionOperator::Plus,
                    Object::Boolean(true),
                    Object::Boolean(false),
                ),
            ),
            (
                "jika (10 > 1) { benar + salah; }",
                ErrorKind::UnknownInfixOperator(
                    ExpressionOperator::Plus,
                    Object::Boolean(true),
                    Object::Boolean(false),
                ),
            ),
            (
                "
                jika (10 > 1) {
                    jika (10 > 1) {
                        kembalikan benar + salah;
                    }

                    kembalikan 1;
                }
                ",
                ErrorKind::UnknownInfixOperator(
                    ExpressionOperator::Plus,
                    Object::Boolean(true),
                    Object::Boolean(false),
                ),
            ),
            (
                "hahasiapakamu",
                ErrorKind::IdentifierNotFound(Identifier::new(Literal(
                    "hahasiapakamu".to_string(),
                ))),
            ),
        ];

        for (input, expected_error) in inputs_to_expected_errors {
            let error = parse_eval(input).unwrap_err();

            assert_eq!(error.description(), expected_error.description());
        }
    }

    #[test]
    fn eval_let_statements() {
        let inputs_to_expected_objects = vec![
            ("diketahui a = 5; a;", Object::Integer(5)),
            ("diketahui a = 5 * 5; a;", Object::Integer(25)),
            ("diketahui a = 5; diketahui b = a; b;", Object::Integer(5)),
            (
                "diketahui a = 5; diketahui b = a; diketahui c = a + b + 5; c;",
                Object::Integer(15),
            ),
        ];

        for (input, expected_object) in inputs_to_expected_objects {
            let object = parse_eval(input).unwrap();

            assert_eq!(object, expected_object);
        }
    }
}
