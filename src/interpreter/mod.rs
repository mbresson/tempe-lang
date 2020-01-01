use crate::representations::ast::{
    ConditionalExpression, Expression, ExpressionOperator, FunctionCallExpression,
    FunctionExpression, Identifier, InfixOperationExpression, PrefixOperationExpression, Program,
    Statement,
};

pub mod errors;
pub mod object;

use errors::{ErrorKind, Result as InterpretingResult};
use object::{Environment, FunctionObject, Object};

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
    env: &mut Environment,
) -> InterpretingResult<Object> {
    let condition = eval_expression(&conditional_expression.condition, env)?;

    if is_truthy(condition) {
        eval_block_statement(&conditional_expression.consequence.statements, env)
    } else if let Some(alternative) = &conditional_expression.alternative {
        eval_block_statement(&alternative.statements, env)
    } else {
        Ok(Object::Null)
    }
}

fn eval_expression(expression: &Expression, env: &mut Environment) -> InterpretingResult<Object> {
    match expression {
        Expression::Boolean(value) => Ok(Object::Boolean(*value)),
        Expression::Integer(value) => Ok(Object::Integer(*value)),
        Expression::PrefixOperation(PrefixOperationExpression {
            prefix_operator,
            right_expression,
        }) => {
            let right_value = eval_expression(right_expression, env)?;
            eval_prefix_operation(*prefix_operator, right_value)
        }
        Expression::InfixOperation(InfixOperationExpression {
            left_expression,
            infix_operator,
            right_expression,
        }) => {
            let left_value = eval_expression(left_expression, env)?;
            let right_value = eval_expression(right_expression, env)?;
            eval_infix_operation(*infix_operator, left_value, right_value)
        }
        Expression::Conditional(conditional_expression) => {
            eval_conditional_expression(conditional_expression, env)
        }
        Expression::Identifier(identifier) => eval_identifier(&identifier, env),
        Expression::Function(function) => Ok(Object::Function(create_function(function, env))),
        Expression::FunctionCall(call) => eval_function_call(call, env),
    }
}

fn eval_function_call(
    call: &FunctionCallExpression,
    env: &mut Environment,
) -> InterpretingResult<Object> {
    let function = match call.function.as_ref() {
        Expression::Function(function_expression) => {
            // direct function definition and call
            // e.g. fungsi(x) { x + 1 }(2)
            create_function(function_expression, env)
        }
        Expression::Identifier(identifier) => {
            // calling a previously defined function
            // e.g. myFunction(arg1, arg2)
            if let Object::Function(function) = eval_identifier(identifier, env)? {
                function
            } else {
                return Err(ErrorKind::IdentifierNotFound(identifier.clone()).into());
            }
        }
        Expression::FunctionCall(function_call) => {
            // calling a function and then calling its result
            // e.g. myFunction(arg1, arg2)(arg_to_resulting_function)
            let result = eval_function_call(function_call, env)?;

            if let Object::Function(function) = result {
                function
            } else {
                return Err(ErrorKind::ExpectedFunction(call.function.as_ref().clone()).into());
            }
        }
        expression => return Err(ErrorKind::ExpectedFunction(expression.clone()).into()),
    };

    if function.parameters.len() != call.arguments.len() {
        return Err(ErrorKind::WrongNumberOfArguments(*function, call.arguments.clone()).into());
    }

    let mut execution_env = function.env.clone();

    for (argument_index, unevaluated_argument) in call.arguments.iter().enumerate() {
        let argument = eval_expression(unevaluated_argument, env)?;
        let argument_name = function.parameters[argument_index].clone();

        execution_env.set(argument_name, argument);
    }

    // NOTE: this is a bit of a hack to support recursion
    // (
    //   I didn't follow the original design of the book Writing an Interpreter in Go here,
    //   because 1. it's Go :) and 2. it required defining Environment as a recursive struct,
    //   which is not trivial at all in Rust
    //     ( basically amounts to complicating the code with lots of Rc<RefCell<...>>,
    //       or using unsafe Rust, and I'm unfamiliar with both! )
    //
    // The alternative solution that I found:
    // - The function will hold a snapshot of the environment when the function was declared
    // - just before running the function, the function's environment snapshot will be cloned
    //   and the function name will be injected inside this final environment a.k.a. the execution_env
    // )
    let statements = function.body.statements.clone();

    if let Some(function_name) = &function.name {
        execution_env.set(function_name.clone(), Object::Function(function));
    }

    eval_block_statement(&statements, &mut execution_env)
}

fn create_function(
    function_expression: &FunctionExpression,
    env: &Environment,
) -> Box<FunctionObject> {
    Box::new(FunctionObject::new(
        function_expression.parameters.clone(),
        function_expression.body.clone(),
        env.clone(),
    ))
}

fn eval_identifier(identifier: &Identifier, env: &Environment) -> InterpretingResult<Object> {
    match env.get(identifier) {
        Some(value) => Ok(value.clone()),
        None => Err(ErrorKind::IdentifierNotFound(identifier.clone()).into()),
    }
}

fn eval_statement(statement: &Statement, env: &mut Environment) -> InterpretingResult<Object> {
    match statement {
        Statement::Expression(expression) => eval_expression(expression, env),
        Statement::Return(return_statement) => {
            let value = eval_expression(&return_statement.value, env)?;
            Ok(Object::EarlyReturnedObject(Box::new(value)))
        }
        Statement::Let(let_statement) => {
            let mut value = eval_expression(&let_statement.value, env)?;

            // if the let statement binds a function, then the function must know its own name,
            // which will allow us to later add its name to its execution environment to support recursion
            if let Object::Function(function) = &mut value {
                function.bind_name(let_statement.name.clone());
            }

            env.set(let_statement.name.clone(), value.clone());

            Ok(value)
        }
    }
}

pub fn eval_block_statement(
    statements: &[Statement],
    env: &mut Environment,
) -> InterpretingResult<Object> {
    let mut result = Object::Null;

    for statement in statements {
        result = eval_statement(statement, env)?;

        if let Object::EarlyReturnedObject(_) = result {
            break;
        }
    }

    Ok(result)
}

pub fn eval_program(program: &Program, env: &mut Environment) -> InterpretingResult<Object> {
    Ok(match eval_block_statement(&program.statements, env)? {
        Object::EarlyReturnedObject(value) => *value,
        value => value,
    })
}

#[cfg(test)]
mod tests {
    use super::errors::{ErrorKind, Result as InterpretingResult};
    use super::{Environment, FunctionObject, Object};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::representations::ast::{
        BlockStatement, Expression, ExpressionOperator, Identifier, InfixOperationExpression,
        Statement,
    };
    use crate::representations::token::Literal;

    fn parse_eval(input: &str) -> InterpretingResult<Object> {
        let mut lexer = Lexer::new(input);

        let program = Parser::new(&mut lexer)
            .parse_program()
            .map_err(|errors| format!("parse_program returned errors {:?}", errors))?;

        let mut env = Environment::new();

        super::eval_program(&program, &mut env)
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

    #[test]
    fn eval_function_definition() {
        let input = "fungsi(x) { x + 2; };";

        let expected_object = Object::Function(Box::new(FunctionObject::new(
            vec![Identifier::new(Literal("x".to_string()))],
            BlockStatement::new(vec![Statement::Expression(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::Plus,
                    Expression::Identifier(Identifier::new(Literal("x".to_string()))),
                    Expression::Integer(2),
                ),
            ))]),
            Environment::new(),
        )));

        let object = parse_eval(input).unwrap();

        assert_eq!(object, expected_object);
    }

    #[test]
    fn eval_function_call() {
        let inputs_to_expected_objects = vec![
            (
                "diketahui identity = fungsi(x) { x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "diketahui identity = fungsi(x) { kembalikan x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "diketahui double = fungsi(x) { x * 2; }; double(5);",
                Object::Integer(10),
            ),
            (
                "diketahui add = fungsi(x, y) { x + y; }; add(5, 5);",
                Object::Integer(10),
            ),
            (
                "diketahui add = fungsi(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Object::Integer(20),
            ),
            ("fungsi(x) { x; }(5)", Object::Integer(5)),
            (
                "
                    diketahui fibonacci = fungsi(n) {
                        jika (n < 3) {
                            jika(n == 0) { 0 }
                            jika tidak { 1 }
                        } jika tidak {
                            fibonacci(n-1) + fibonacci(n-2)
                        }
                    };
                    
                    fibonacci(10)
                ",
                Object::Integer(55),
            ),
            (
                "
                    diketahui newAdder = fungsi(x) { fungsi(y) { x + y }; };
                    diketahui addTwo = newAdder(2);
                    addTwo(2);
                ",
                Object::Integer(4),
            ),
            (
                // the sky is the limit!! (but I don't recommend writing such spaghetti code ever...)
                "
                    fungsi() {
                        fungsi(y) {
                            fungsi(z) { z*10 }(y)+1000
                        }
                    }()(2)
                ",
                Object::Integer(1020),
            ),
            (
                "
                    diketahui add = fungsi(a, b) { a + b };
                    diketahui applyFunc = fungsi(a, b, func) { func(a, b) };
                    applyFunc(2, 2, add);
                ",
                Object::Integer(4),
            ),
        ];

        for (input, expected_object) in inputs_to_expected_objects {
            let object = parse_eval(input).unwrap();

            assert_eq!(object, expected_object);
        }
    }
}
