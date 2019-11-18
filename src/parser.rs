use crate::ast::{
    Expression, ExpressionOperator, Identifier, InfixOperationExpression,
    LetStatement, Precedence, PrefixOperationExpression, Program, ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::Token;
use std::vec::Vec;

pub struct Parser<'a> {
    lexer: Lexer<'a>,

    current_token: Option<Token>,
    peek_token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: None,
            peek_token: None,
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn parse_infix(&mut self, left_expression: Expression) -> Result<Expression, String> {
        match Self::get_token_or_error(&self.current_token)? {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::Equal
            | Token::NotEqual
            | Token::LessThan
            | Token::GreaterThan => self.parse_infix_expression(left_expression),
            token => Err(format!("expected infix operator, got {:?}", token)),
        }
    }

    fn parse_prefix(&mut self) -> Result<Expression, String> {
        match Self::get_token_or_error(&self.current_token)? {
            Token::Identifier(literal) => {
                Ok(Expression::Identifier(Identifier::new(literal.clone())))
            }
            Token::Integer(integer) => Ok(Expression::Integer(*integer)),
            Token::True => Ok(Expression::Boolean(true)),
            Token::False => Ok(Expression::Boolean(false)),
            Token::Minus | Token::Bang => self.parse_prefix_expression(),
            token => Err(format!("cannot parse token {:?} as prefix", token)),
        }
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

        expression.map(Statement::Expression)
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

        let identifier = match Self::get_token_or_error(&self.current_token)? {
            Token::Identifier(literal) => Identifier::new(literal.clone()),
            token => {
                return Err(format!("expected identifier, got {:?}", token));
            }
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
        let mut left_expression = self.parse_prefix()?;

        'parsing_expression: while let Some(following_token) = &self.peek_token {
            let following_operator =
                if let Some(operator) = Self::token_to_infix_operator(&following_token) {
                    operator
                } else {
                    break 'parsing_expression;
                };

            if Precedence::from(&following_operator) <= precedence {
                break 'parsing_expression;
            }

            self.next_token();

            left_expression = self.parse_infix(left_expression)?;
        }

        Ok(left_expression)
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
        Expression, ExpressionOperator, Identifier, InfixOperationExpression,
        LetStatement, PrefixOperationExpression, ReturnStatement, Statement,
    };
    use crate::lexer::Lexer;
    use crate::token::Literal;

    fn parse(input: &str) -> Result<super::Program, String> {
        super::Parser::new(Lexer::new(input))
            .parse_program()
            .map_err(|errors| format!("parse_program returned errors {:?}", errors))
    }

    #[test]
    fn let_statements() {
        let input = "
            diketahui x = 5;
            diketahui y = 10;
            diketahui foobar = 838383;
        ";

        let program = parse(input).unwrap();

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

        let program = parse(input).unwrap();

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

        let program = parse(input).unwrap();

        let expected_identifier_expression = Statement::Expression(
            Expression::Identifier(Identifier::new(Literal("foobar".to_string()))),
        );

        assert_eq!(program.statements[0], expected_identifier_expression);
    }

    #[test]
    fn integer_expressions() {
        let input = "
            42;
            52
        ";

        let program = parse(input).unwrap();

        let expected_expressions = vec![
            Statement::Expression(Expression::Integer(42)),
            Statement::Expression(Expression::Integer(52)),
        ];

        assert_eq!(program.statements, expected_expressions);
    }

    #[test]
    fn boolean_expressions() {
        let input = "
            benar;
            salah
        ";

        let program = parse(input).unwrap();

        let expected_expressions = vec![
            Statement::Expression(Expression::Boolean(true)),
            Statement::Expression(Expression::Boolean(false)),
        ];

        assert_eq!(program.statements, expected_expressions);
    }

    #[test]
    fn prefix_expressions() {
        let input = "
            !5;
            -15;
            !benar;
            !salah;
        ";

        let program = parse(input).unwrap();

        let expected_expressions = vec![
            Statement::Expression(Expression::PrefixOperation(
                PrefixOperationExpression::new(ExpressionOperator::Bang, Expression::Integer(5)),
            )),
            Statement::Expression(Expression::PrefixOperation(
                PrefixOperationExpression::new(ExpressionOperator::Minus, Expression::Integer(15)),
            )),
            Statement::Expression(Expression::PrefixOperation(
                PrefixOperationExpression::new(ExpressionOperator::Bang, Expression::Boolean(true)),
            )),
            Statement::Expression(Expression::PrefixOperation(
                PrefixOperationExpression::new(ExpressionOperator::Bang, Expression::Boolean(false)),
            )),
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
            benar == benar;
            benar != salah;
            salah == salah;
        "#;

        let program = parse(input).unwrap();

        let expected_expressions = vec![
            Statement::Expression(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::Plus,
                    Expression::Integer(5),
                    Expression::Integer(6),
                ),
            )),
            Statement::Expression(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::Plus,
                    Expression::Integer(5),
                    Expression::Identifier(Identifier::new(Literal("ab".to_string()))),
                ),
            )),
            Statement::Expression(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::Minus,
                    Expression::Integer(5),
                    Expression::Integer(6),
                ),
            )),
            Statement::Expression(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::Multiply,
                    Expression::Integer(5),
                    Expression::Integer(6),
                ),
            )),
            Statement::Expression(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::Divide,
                    Expression::Integer(5),
                    Expression::Integer(6),
                ),
            )),
            Statement::Expression(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::GreaterThan,
                    Expression::Integer(5),
                    Expression::Integer(6),
                ),
            )),
            Statement::Expression(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::LessThan,
                    Expression::Integer(5),
                    Expression::Integer(6),
                ),
            )),
            Statement::Expression(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::Equal,
                    Expression::Integer(5),
                    Expression::Integer(6),
                ),
            )),
            Statement::Expression(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::NotEqual,
                    Expression::Integer(5),
                    Expression::Integer(6),
                ),
            )),
            Statement::Expression(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::Equal,
                    Expression::Boolean(true),
                    Expression::Boolean(true),
                ),
            )),
            Statement::Expression(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::NotEqual,
                    Expression::Boolean(true),
                    Expression::Boolean(false),
                ),
            )),
            Statement::Expression(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::Equal,
                    Expression::Boolean(false),
                    Expression::Boolean(false),
                ),
            )),
        ];

        assert_eq!(program.statements, expected_expressions);
    }

    #[test]
    fn infix_expressions_with_operator_precedence() {
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
            InputAndExpected {
                input: "benar",
                expected: "benar;",
            },
            InputAndExpected {
                input: "salah",
                expected: "salah;",
            },
            InputAndExpected {
                input: "3 > 5 == false",
                expected: "((3 > 5) == false);",
            },
            InputAndExpected {
                input: "3 < 5 == true",
                expected: "((3 < 5) == true);",
            },
        ];

        for input_and_expected in input {
            let program = parse(input_and_expected.input).unwrap();

            assert_eq!(format!("{}", program), input_and_expected.expected);
        }
    }
}
