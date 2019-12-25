extern crate error_chain;

use crate::representations::token::Token;
use error_chain::error_chain;

error_chain! {
    errors {
        ExpectedPrefixOperator(token_found: Token) {
            description("expected prefix operator")
            display("expected prefix operator, got {:?}", token_found)
        }

        ExpectedInfixOperator(token_found: Token) {
            description("expected infix operator")
            display("expected infix operator, got {:?}", token_found)
        }

        ExpectedSpecificToken(token_expected: Token, token_found: Token) {
            description("expected specific token")
            display("expected token {:?}, got {:?}", token_expected, token_found)
        }

        ExpectedToken {
            description("expected token, got none")
            display("expected token, got none")
        }

        ExpectedIdentifier(token_found: Token) {
            description("expected identifier")
            display("expected identifier, got {:?}", token_found)
        }

        CannotParseTokenAsPrefix(token: Token) {
            description("cannot parse token as prefix")
            display("cannot parse token {:?} as prefix", token)
        }
    }
}
