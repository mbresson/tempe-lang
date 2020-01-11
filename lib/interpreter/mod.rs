use crate::representations::ast::{
    ConditionalExpression, Expression, ExpressionOperator, FunctionCallExpression,
    FunctionExpression, Identifier, IndexOperationExpression, InfixOperationExpression,
    PrefixOperationExpression, Program, Statement,
};
use std::collections::HashMap;
use std::convert::TryInto;

mod builtins;
pub mod errors;
pub mod object;

use errors::{ErrorKind, Result as InterpretingResult};
use object::{BuiltinFunctionObject, Environment, FunctionObject, Object};

/// # Examples
///
/// ```
/// use tempe_lang::lexer::Lexer;
/// use tempe_lang::parser::Parser;
/// use tempe_lang::interpreter::{eval_program, object::Environment};
///
/// let source_code = "
/// 1 + 1
/// ";
///
/// let mut lexer = Lexer::new(source_code);
///
/// let program = Parser::new(&mut lexer)
///     .parse_program()
///     .unwrap();
///
/// let mut env = Environment::new_with_builtin_functions();
///
/// eval_program(&program, &mut env);
/// ```

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

fn eval_index_operation(index: Object, left_value: Object) -> InterpretingResult<Object> {
    match (index, left_value) {
        (Object::Integer(numeric_index), Object::Array(array)) => {
            if numeric_index < 0 {
                return Err(ErrorKind::OutOfBoundsArrayIndex(array, numeric_index).into());
            }

            let u_numeric_index: usize = numeric_index.try_into().unwrap();

            if u_numeric_index >= array.len() {
                Err(ErrorKind::OutOfBoundsArrayIndex(array, numeric_index).into())
            } else {
                Ok(array[u_numeric_index].clone())
            }
        }
        (Object::Str(key), Object::HashMap(hashmap)) => {
            Ok(hashmap.get(&key).map(Object::clone).unwrap_or(Object::Null))
        }
        (index, left_value) => Err(ErrorKind::UnknownIndexOperator(index, left_value).into()),
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

fn eval_str_infix_operation(
    operator: ExpressionOperator,
    left_value: &str,
    right_value: &str,
) -> InterpretingResult<Object> {
    match operator {
        ExpressionOperator::Plus => Ok(Object::Str(format!("{}{}", left_value, right_value))),
        ExpressionOperator::Equal => Ok(Object::Boolean(left_value == right_value)),
        ExpressionOperator::NotEqual => Ok(Object::Boolean(left_value != right_value)),
        _ => Err(ErrorKind::UnknownInfixOperator(
            operator,
            Object::Str(left_value.to_string()),
            Object::Str(right_value.to_string()),
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
        (Object::Str(left_str), Object::Str(right_str)) => {
            eval_str_infix_operation(operator, &left_str, &right_str)
        }
        (left_value, right_value) => {
            Err(ErrorKind::UnknownInfixOperator(operator, left_value, right_value).into())
        }
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
        Expression::Str(string) => Ok(Object::Str(string.clone())),
        Expression::Array(items) => Ok(Object::Array(
            items
                .iter()
                .map(|item| eval_expression(item, env))
                .collect::<InterpretingResult<Vec<Object>>>()?,
        )),
        Expression::HashLiteral(key_value_pairs) => {
            let mut hashmap = HashMap::new();

            for (key_expression, value_expression) in key_value_pairs {
                let key = eval_expression(key_expression, env)?;
                let value = eval_expression(value_expression, env)?;

                if let Object::Str(key_string) = key {
                    hashmap.insert(key_string, value);
                } else {
                    return Err(ErrorKind::WrongHashMapKeyType(key).into());
                }
            }

            Ok(Object::HashMap(hashmap))
        }
        Expression::IndexOperation(IndexOperationExpression {
            index,
            left_expression,
        }) => {
            let left_value = eval_expression(left_expression, env)?;
            let index_value = eval_expression(index, env)?;

            eval_index_operation(index_value, left_value)
        }
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
    let mut arguments = Vec::new();

    for unevaluated_argument in &call.arguments {
        let argument = eval_expression(unevaluated_argument, env)?;

        arguments.push(argument);
    }

    let function = match call.function.as_ref() {
        Expression::Function(function_expression) => {
            // direct function definition and call
            // e.g. fungsi(x) { x + 1 }(2)
            create_function(function_expression, env)
        }
        Expression::Identifier(identifier) => {
            // calling a previously defined function
            // e.g. myFunction(arg1, arg2)

            let maybe_function = eval_identifier(identifier, env)?;

            if let Object::Function(function) = maybe_function {
                function
            } else if let Object::BuiltinFunction(BuiltinFunctionObject {
                implementation, ..
            }) = maybe_function
            {
                return implementation(arguments);
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

    if function.parameters.len() != arguments.len() {
        return Err(ErrorKind::WrongNumberOfArguments(
            function.name,
            function.parameters.len(),
            arguments.len(),
        )
        .into());
    }

    let mut execution_env = function.env.clone();

    for (argument_index, argument) in arguments.into_iter().enumerate() {
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

fn eval_block_statement(
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
