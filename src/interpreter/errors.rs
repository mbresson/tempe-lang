extern crate error_chain;

use super::Object;
use crate::representations::ast::{Expression, ExpressionOperator, Identifier};
use crate::representations::token::Literal;
use error_chain::error_chain;

fn function_name_or_anonymous(function_name: &Option<Identifier>) -> Identifier {
    function_name
        .clone()
        .unwrap_or_else(|| Identifier::new(Literal("anonymous".to_string())))
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
                "wrong number of arguments provided to function {:?}, expected {}, received {}", function_name_or_anonymous(function), expected, received)
        }

        WrongArgumentType(function: Option<Identifier>, argument: Object, expected: String) {
            description("wrong argument type provided to function")
            display("wrong argument type provided to function {:?}, expected {}, got {:?}", function_name_or_anonymous(function), expected, argument)
        }
    }
}
