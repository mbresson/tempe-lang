extern crate error_chain;

use crate::representations::token::{Context, Token, TokenWithContext};
use error_chain::error_chain;

error_chain! {
    errors {
        ExpectedPrefixOperator(token_found: TokenWithContext) {
            description("expected prefix operator")
            display("expected prefix operator, got {:?}", token_found.token)
        }

        ExpectedInfixOperator(token_found: TokenWithContext) {
            description("expected infix operator")
            display("expected infix operator, got {:?}", token_found.token)
        }

        ExpectedSpecificToken(token_expected: Token, token_found: TokenWithContext) {
            description("expected specific token")
            display("expected token {:?}, got {:?}", token_expected, token_found.token)
        }

        ExpectedToken(context: Context) {
            description("expected token, got none")
            display("expected token, got none")
        }

        ExpectedExpression(context: Context, token_found: Option<TokenWithContext>) {
            description("expected expression")
            display("expected expression, got {:?}", token_found.as_ref().map(|t| &t.token))
        }

        ExpectedIdentifier(token_found: TokenWithContext) {
            description("expected identifier")
            display("expected identifier, got {:?}", token_found.token)
        }

        CannotParseTokenAsPrefix(token_with_context: TokenWithContext) {
            description("cannot parse token as prefix")
            display("cannot parse token {:?} as prefix", token_with_context.token)
        }
    }
}

impl Error {
    pub fn context(&self) -> Context {
        match self {
            Error(ErrorKind::ExpectedPrefixOperator(token_found), _) => token_found.context,
            Error(ErrorKind::ExpectedInfixOperator(token_found), _) => token_found.context,
            Error(ErrorKind::ExpectedSpecificToken(_, token_found), _) => token_found.context,
            Error(ErrorKind::ExpectedToken(context), _) => *context,
            Error(ErrorKind::ExpectedExpression(context, _), _) => *context,
            Error(ErrorKind::ExpectedIdentifier(token_found), _) => token_found.context,
            Error(ErrorKind::CannotParseTokenAsPrefix(token_found), _) => token_found.context,
            error => unreachable!("error without context: {}", error),
        }
    }
}
