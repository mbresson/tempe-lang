extern crate error_chain;

use super::{FunctionObject, Object};
use crate::representations::ast::{Expression, ExpressionOperator, Identifier};
use error_chain::error_chain;

error_chain! {
    errors {
        TypeMismatch(left_object: Object, right_object: Object) {
            description("type mismatch")
            display("type mismatch between {:?} and {:?}", left_object, right_object)
        }

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

        WrongNumberOfArguments(function: FunctionObject, found_arguments: Vec<Expression>) {
            description("wrong number of arguments provided to function")
            display("wrong number of arguments provided to function {:?}, got {:?}", function, found_arguments)
        }
    }
}
