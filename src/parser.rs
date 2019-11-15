use crate::ast::{
    Expression, ExpressionOperator, ExpressionStatement, Identifier, InfixOperationExpression,
    LetStatement, Precedence, PrefixOperationExpression, Program, ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::Token;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::vec::Vec;

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum ParseableToken {
    Identifier,
    Integer,
    Bang,
    Minus,
    Plus,
    Slash,
    Asterisk,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
}

impl TryFrom<&Token> for ParseableToken {
    type Error = String;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token {
            Token::Identifier(_) => Ok(ParseableToken::Identifier),
            Token::Integer(_) => Ok(ParseableToken::Integer),
            Token::Bang => Ok(ParseableToken::Bang),
            Token::Minus => Ok(ParseableToken::Minus),
            Token::Plus => Ok(ParseableToken::Plus),
            Token::Slash => Ok(ParseableToken::Slash),
            Token::Asterisk => Ok(ParseableToken::Asterisk),
            Token::Equal => Ok(ParseableToken::Equal),
            Token::NotEqual => Ok(ParseableToken::NotEqual),
            Token::LessThan => Ok(ParseableToken::LessThan),
            Token::GreaterThan => Ok(ParseableToken::GreaterThan),
            _ => Err(format!("unparseable token: {:?}", token)),
        }
    }
}

type PrefixParseFunction<'a> = dyn Fn(&mut Parser<'a>) -> Result<Expression, String>;
type InfixParseFunction<'a> = dyn Fn(&mut Parser<'a>, Expression) -> Result<Expression, String>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,

    current_token: Option<Token>,
    peek_token: Option<Token>,

    prefix_parse_functions: HashMap<ParseableToken, &'a PrefixParseFunction<'a>>,
    infix_parse_functions: HashMap<ParseableToken, &'a InfixParseFunction<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: None,
            peek_token: None,

            prefix_parse_functions: HashMap::new(),
            infix_parse_functions: HashMap::new(),
        };

        parser
            .prefix_parse_functions
            .insert(ParseableToken::Identifier, &Self::parse_identifier);
        parser
            .prefix_parse_functions
            .insert(ParseableToken::Integer, &Self::parse_integer);
        parser
            .prefix_parse_functions
            .insert(ParseableToken::Bang, &Self::parse_prefix_expression);
        parser
            .prefix_parse_functions
            .insert(ParseableToken::Minus, &Self::parse_prefix_expression);

        parser
            .infix_parse_functions
            .insert(ParseableToken::Plus, &Self::parse_infix_expression);
        parser
            .infix_parse_functions
            .insert(ParseableToken::Minus, &Self::parse_infix_expression);
        parser
            .infix_parse_functions
            .insert(ParseableToken::Slash, &Self::parse_infix_expression);
        parser
            .infix_parse_functions
            .insert(ParseableToken::Asterisk, &Self::parse_infix_expression);
        parser
            .infix_parse_functions
            .insert(ParseableToken::Equal, &Self::parse_infix_expression);
        parser
            .infix_parse_functions
            .insert(ParseableToken::NotEqual, &Self::parse_infix_expression);
        parser
            .infix_parse_functions
            .insert(ParseableToken::LessThan, &Self::parse_infix_expression);
        parser
            .infix_parse_functions
            .insert(ParseableToken::GreaterThan, &Self::parse_infix_expression);

        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = self.lexer.next();
    }

    pub fn parse_program(&mut self) -> Result<Program, Vec<String>> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        'statements_parsing_loop: while let Some(token) = &self.current_token {
            let parsed_statement = match token {
                Token::Let => self.parse_let_statement(),
                Token::Return => self.parse_return_statement(),
                _ => self.parse_expression_statement(),
            };

            let statement = match parsed_statement {
                Err(parsing_error) => {
                    errors.push(parsing_error);
                    self.jump_to_next_statement();
                    continue 'statements_parsing_loop;
                }
                Ok(statement) => statement,
            };

            statements.push(statement);

            self.next_token();
        }

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(Program { statements })
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let expression = self.parse_expression(Precedence::Lowest);

        self.next_token();

        expression.map(|e| Statement::Expression(ExpressionStatement::new(e)))
    }

    fn jump_to_next_statement(&mut self) {
        while let Some(token) = &self.current_token {
            if *token == Token::Semicolon {
                self.next_token();
                return;
            }

            self.next_token();
        }
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        self.next_token();

        Ok(Statement::Return(ReturnStatement::new(value)))
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        self.next_token();

        let identifier = match self.parse_identifier()? {
            Expression::Identifier(identifier) => identifier,
            expression => return Err(format!("expected identifier, found {}", expression)),
        };

        self.next_token();

        match Self::get_token_or_error(&self.current_token.clone())? {
            Token::Assign => {}
            illegal_token => {
                return Err(format!(
                    "expected {:?}, found {:?}",
                    Token::Assign,
                    illegal_token
                ));
            }
        };

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        self.next_token();

        Ok(Statement::Let(LetStatement::new(identifier, value)))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, String> {
        let token = ParseableToken::try_from(
            self.current_token
                .as_ref()
                .ok_or_else(|| "token expected, got None".to_string())?,
        )?;

        let prefix_parser = match self.prefix_parse_functions.get(&token) {
            Some(parser) => *parser,
            _ => return Err(format!("cannot parse token {:?} as prefix", token)),
        };

        let mut left_expression = prefix_parser(self)?;

        'parsing_expression: while let Some(following_token) = &self.peek_token {
            let following_operator =
                if let Some(operator) = Self::token_to_infix_operator(&following_token) {
                    operator
                } else {
                    break 'parsing_expression;
                };

            let following_parseable_token =
                if let Ok(parseable_token) = ParseableToken::try_from(following_token) {
                    parseable_token
                } else {
                    break 'parsing_expression;
                };

            if Precedence::from(&following_operator) <= precedence {
                break 'parsing_expression;
            }

            let infix_parser = match self.infix_parse_functions.get(&following_parseable_token) {
                Some(parser) => *parser,
                _ => return Ok(left_expression),
            };

            self.next_token();

            left_expression = infix_parser(self, left_expression)?;
        }

        Ok(left_expression)
    }

    fn parse_integer(&mut self) -> Result<Expression, String> {
        match Self::get_token_or_error(&self.current_token)? {
            Token::Integer(int) => Ok(Expression::Integer(*int)),
            token => Err(format!("expected integer, got {:?}", token)),
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression, String> {
        match Self::get_token_or_error(&self.current_token)? {
            Token::Identifier(literal) => {
                Ok(Expression::Identifier(Identifier::new(literal.clone())))
            }
            token => Err(format!("expected identifier, got {:?}", token)),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, String> {
        let operator = match Self::get_token_or_error(&self.current_token)? {
            Token::Bang => ExpressionOperator::Bang,
            Token::Minus => ExpressionOperator::Minus,
            token => {
                return Err(format!("expected prefix operator, got {:?}", token));
            }
        };

        self.next_token();

        Ok(Expression::PrefixOperation(PrefixOperationExpression::new(
            operator,
            self.parse_expression(Precedence::Prefix)?,
        )))
    }

    fn token_to_infix_operator(token: &Token) -> Option<ExpressionOperator> {
        match token {
            Token::Plus => Some(ExpressionOperator::Plus),
            Token::Minus => Some(ExpressionOperator::Minus),
            Token::Asterisk => Some(ExpressionOperator::Multiply),
            Token::Slash => Some(ExpressionOperator::Divide),
            Token::Equal => Some(ExpressionOperator::Equal),
            Token::NotEqual => Some(ExpressionOperator::NotEqual),
            Token::LessThan => Some(ExpressionOperator::LessThan),
            Token::GreaterThan => Some(ExpressionOperator::GreaterThan),
            _ => None,
        }
    }

    fn parse_infix_expression(
        &mut self,
        left_expression: Expression,
    ) -> Result<Expression, String> {
        let operator = Self::get_token_or_error(&self.current_token).and_then(|token| {
            Self::token_to_infix_operator(&token)
                .ok_or_else(|| format!("expected infix operator, got {:?}", token))
        })?;

        self.next_token();

        let precedence = Precedence::from(&operator);

        Ok(Expression::InfixOperation(InfixOperationExpression::new(
            operator,
            left_expression,
            self.parse_expression(precedence)?,
        )))
    }

    fn get_token_or_error(tentative_token: &Option<Token>) -> Result<&Token, String> {
        match tentative_token {
            Some(token) => Ok(token),
            None => Err("expected token, got none".to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        Expression, ExpressionOperator, ExpressionStatement, Identifier, InfixOperationExpression,
        LetStatement, PrefixOperationExpression, ReturnStatement, Statement,
    };
    use crate::lexer::Lexer;
    use crate::token::Literal;

    #[test]
    fn let_statements() {
        let input = "
            diketahui x = 5;
            diketahui y = 10;
            diketahui foobar = 838383;
        ";

        let mut parser = super::Parser::new(Lexer::new(input));

        let program = parser
            .parse_program()
            .unwrap_or_else(|errors| panic!(format!("parse_program returned errors {:?}", errors)));

        let expected_statements = vec![
            Statement::Let(LetStatement::new(
                Identifier::new(Literal("x".to_string())),
                Expression::Integer(5),
            )),
            Statement::Let(LetStatement::new(
                Identifier::new(Literal("y".to_string())),
                Expression::Integer(10),
            )),
            Statement::Let(LetStatement::new(
                Identifier::new(Literal("foobar".to_string())),
                Expression::Integer(838_383),
            )),
        ];

        for (statement_index, expected_statement) in expected_statements.iter().enumerate() {
            let statement = &program.statements[statement_index];

            assert_eq!(statement, expected_statement);
        }
    }

    #[test]
    fn return_statements() {
        let input = "
            kembalikan 5;
            kembalikan 10;
            kembalikan 993322;
        ";

        let mut parser = super::Parser::new(Lexer::new(input));

        let program = parser
            .parse_program()
            .unwrap_or_else(|errors| panic!(format!("parse_program returned errors {:?}", errors)));

        let expected_statements = vec![
            Statement::Return(ReturnStatement::new(Expression::Integer(5))),
            Statement::Return(ReturnStatement::new(Expression::Integer(10))),
            Statement::Return(ReturnStatement::new(Expression::Integer(993_322))),
        ];

        for (statement_index, expected_statement) in expected_statements.iter().enumerate() {
            let statement = &program.statements[statement_index];

            assert_eq!(statement, expected_statement);
        }
    }

    #[test]
    fn identifier_expressions() {
        let input = "foobar;";

        let mut parser = super::Parser::new(Lexer::new(input));

        let program = parser
            .parse_program()
            .unwrap_or_else(|errors| panic!(format!("parse_program returned errors {:?}", errors)));

        let expected_identifier_expression = Statement::Expression(ExpressionStatement::new(
            Expression::Identifier(Identifier::new(Literal("foobar".to_string()))),
        ));

        assert_eq!(program.statements[0], expected_identifier_expression);
    }

    #[test]
    fn integer_expressions() {
        let input = "
            42;
            52
        ";

        let mut parser = super::Parser::new(Lexer::new(input));

        let program = parser
            .parse_program()
            .unwrap_or_else(|errors| panic!(format!("parse_program returned errors {:?}", errors)));

        let expected_expressions = vec![
            Statement::Expression(ExpressionStatement::new(Expression::Integer(42))),
            Statement::Expression(ExpressionStatement::new(Expression::Integer(52))),
        ];

        assert_eq!(program.statements, expected_expressions);
    }

    #[test]
    fn prefix_expressions() {
        let input = "
            !5;
            -15;
        ";

        let mut parser = super::Parser::new(Lexer::new(input));

        let program = parser
            .parse_program()
            .unwrap_or_else(|errors| panic!(format!("parse_program returned errors {:?}", errors)));

        let expected_expressions = vec![
            Statement::Expression(ExpressionStatement::new(Expression::PrefixOperation(
                PrefixOperationExpression::new(ExpressionOperator::Bang, Expression::Integer(5)),
            ))),
            Statement::Expression(ExpressionStatement::new(Expression::PrefixOperation(
                PrefixOperationExpression::new(ExpressionOperator::Minus, Expression::Integer(15)),
            ))),
        ];

        assert_eq!(program.statements, expected_expressions);
    }

    #[test]
    fn infix_expressions() {
        let input = r#"
            5 + 6;
            5 + ab;
            5 - 6;
            5 * 6;
            5 / 6;
            5 > 6;
            5 < 6;
            5 == 6;
            5 != 6;
        "#;

        let mut parser = super::Parser::new(Lexer::new(input));

        let program = parser
            .parse_program()
            .unwrap_or_else(|errors| panic!(format!("parse_program returned errors {:?}", errors)));

        let expected_expressions = vec![
            Statement::Expression(ExpressionStatement::new(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::Plus,
                    Expression::Integer(5),
                    Expression::Integer(6),
                ),
            ))),
            Statement::Expression(ExpressionStatement::new(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::Plus,
                    Expression::Integer(5),
                    Expression::Identifier(Identifier::new(Literal("ab".to_string()))),
                ),
            ))),
            Statement::Expression(ExpressionStatement::new(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::Minus,
                    Expression::Integer(5),
                    Expression::Integer(6),
                ),
            ))),
            Statement::Expression(ExpressionStatement::new(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::Multiply,
                    Expression::Integer(5),
                    Expression::Integer(6),
                ),
            ))),
            Statement::Expression(ExpressionStatement::new(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::Divide,
                    Expression::Integer(5),
                    Expression::Integer(6),
                ),
            ))),
            Statement::Expression(ExpressionStatement::new(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::GreaterThan,
                    Expression::Integer(5),
                    Expression::Integer(6),
                ),
            ))),
            Statement::Expression(ExpressionStatement::new(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::LessThan,
                    Expression::Integer(5),
                    Expression::Integer(6),
                ),
            ))),
            Statement::Expression(ExpressionStatement::new(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::Equal,
                    Expression::Integer(5),
                    Expression::Integer(6),
                ),
            ))),
            Statement::Expression(ExpressionStatement::new(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::NotEqual,
                    Expression::Integer(5),
                    Expression::Integer(6),
                ),
            ))),
        ];

        assert_eq!(program.statements, expected_expressions);
    }

    #[test]
    fn complex_infix_expressions() {
        struct InputAndExpected {
            input: &'static str,
            expected: &'static str,
        }

        let input = vec![
            InputAndExpected {
                input: "-a * b",
                expected: "((-a) * b);",
            },
            InputAndExpected {
                input: "!-a",
                expected: "(!(-a));",
            },
            InputAndExpected {
                input: "a + b + c",
                expected: "((a + b) + c);",
            },
            InputAndExpected {
                input: "a + b - c",
                expected: "((a + b) - c);",
            },
            InputAndExpected {
                input: "a * b * c",
                expected: "((a * b) * c);",
            },
            InputAndExpected {
                input: "a * b / c",
                expected: "((a * b) / c);",
            },
            InputAndExpected {
                input: "a + b / c",
                expected: "(a + (b / c));",
            },
            InputAndExpected {
                input: "a + b * c + d / e - f",
                expected: "(((a + (b * c)) + (d / e)) - f);",
            },
            InputAndExpected {
                input: "3 + 4; -5 * 5",
                expected: "(3 + 4);\n\
                           ((-5) * 5);",
            },
            InputAndExpected {
                input: "5 > 4 == 3 < 4",
                expected: "((5 > 4) == (3 < 4));",
            },
            InputAndExpected {
                input: "5 < 4 != 3 > 4",
                expected: "((5 < 4) != (3 > 4));",
            },
            InputAndExpected {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
            },
        ];

        for input_and_expected in input {
            let mut parser = super::Parser::new(Lexer::new(input_and_expected.input));

            let program = parser.parse_program().unwrap_or_else(|errors| {
                panic!(format!("parse_program returned errors {:?}", errors))
            });

            assert_eq!(format!("{}", program), input_and_expected.expected);
        }
    }
}
