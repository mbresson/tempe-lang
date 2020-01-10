extern crate error_chain;

use super::Object;
use crate::representations::ast::{Expression, ExpressionOperator, Identifier};
use error_chain::error_chain;
use itertools::Itertools;

fn function_name_or_anonymous(function_name: &Option<Identifier>) -> &str {
    if let Some(name) = function_name {
        &name.value.0
    } else {
        "anonymous"
    }
}

error_chain! {
    errors {
        UnknownPrefixOperator(operator: ExpressionOperator, right_value: Object) {
            description("unknown prefix operator")
            display("unknown prefix operator {} {:?}", operator, right_value)
        }

        UnknownInfixOperator(operator: ExpressionOperator, left_value: Object, right_value: Object) {
            description("unknown infix operator")
            display("unknown infix operator {:?} {} {:?}", left_value, operator, right_value)
        }

        UnknownIndexOperator(index: Object, left_value: Object) {
            description("unknown index operator")
            display("unknown index operator {:?} for {:?}", index, left_value)
        }

        OutOfBoundsArrayIndex(array: Vec<Object>, index: i64) {
            description("out of bounds array index")
            display("out of bounds array index {} for array [{}]", index, array.iter().format(", "))
        }

        IdentifierNotFound(identifier: Identifier) {
            description("identifier not found")
            display("identifier {:?} not found", identifier)
        }

        ExpectedFunction(expression: Expression) {
            description("expected function")
            display("expected function, got {:?}", expression)
        }

        WrongNumberOfArguments(function: Option<Identifier>, expected: usize, received: usize) {
            description("wrong number of arguments provided to function")
            display(
                "wrong number of arguments provided to function {:?}, expected {}, received {}",
                function_name_or_anonymous(function), expected, received
            )
        }

        WrongArgumentType(function: Option<Identifier>, argument: Object, expected_types: Vec<&'static str>) {
            description("wrong argument type provided to function")
            display(
                "wrong argument type provided to function {:?}, expected {}, got {:?}",
                function_name_or_anonymous(function), expected_types.iter().format(" | "), argument
            )
        }

        WrongHashMapKeyType(key: Object) {
            description("wrong hashmap key type")
            display("wrong hashmap key type, expected string, got {:?}", key)
        }
    }
}
