extern crate error_chain;

use super::Object;
use crate::representations::ast::{ExpressionOperator, Identifier};
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
    }
}
