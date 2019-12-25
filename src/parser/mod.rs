use crate::representations::ast::{
    BlockStatement, ConditionalExpression, Expression, ExpressionOperator, FunctionCallExpression,
    FunctionExpression, Identifier, InfixOperationExpression, LetStatement, Precedence,
    PrefixOperationExpression, Program, ReturnStatement, Statement,
};
use crate::representations::token::Token;
use std::vec::Vec;

mod errors;
use errors::{Error, ErrorKind, Result as ParsingResult};

pub struct Parser<'a> {
    token_iterator: &'a mut dyn Iterator<Item = Token>,

    current_token: Option<Token>,
    peek_token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(token_iterator: &'a mut dyn Iterator<Item = Token>) -> Self {
        let mut parser = Parser {
            token_iterator,
            current_token: None,
            peek_token: None,
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn parse_infix(&mut self, left_expression: Expression) -> ParsingResult<Expression> {
        match Self::get_token_or_error(&self.current_token)? {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::Equal
            | Token::NotEqual
            | Token::LessThan
            | Token::GreaterThan => self.parse_infix_expression(left_expression),
            Token::OpeningParenthesis => self.parse_function_call_expression(left_expression),
            token => Err(ErrorKind::ExpectedInfixOperator(token.clone()).into()),
        }
    }

    fn parse_prefix(&mut self) -> ParsingResult<Expression> {
        match Self::get_token_or_error(&self.current_token)? {
            Token::Identifier(literal) => {
                Ok(Expression::Identifier(Identifier::new(literal.clone())))
            }
            Token::Integer(integer) => Ok(Expression::Integer(*integer)),
            Token::True => Ok(Expression::Boolean(true)),
            Token::False => Ok(Expression::Boolean(false)),
            Token::Minus | Token::Bang => self.parse_prefix_expression(),
            Token::OpeningParenthesis => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_expression(),
            token => Err(ErrorKind::CannotParseTokenAsPrefix(token.clone()).into()),
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = self.token_iterator.next();
    }

    fn parse_statement(&mut self) -> ParsingResult<Statement> {
        match Self::get_token_or_error(&self.current_token)? {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, Vec<Error>> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        'statements_parsing_loop: while self.current_token.is_some() {
            let statement = match self.parse_statement() {
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

    fn parse_expression_statement(&mut self) -> ParsingResult<Statement> {
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

    fn parse_return_statement(&mut self) -> ParsingResult<Statement> {
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        self.next_token();

        Ok(Statement::Return(ReturnStatement::new(value)))
    }

    fn parse_let_statement(&mut self) -> ParsingResult<Statement> {
        self.next_token();

        let identifier = match Self::get_token_or_error(&self.current_token)? {
            Token::Identifier(literal) => Identifier::new(literal.clone()),
            token => {
                return Err(ErrorKind::ExpectedIdentifier(token.clone()).into());
            }
        };

        self.next_token();

        match Self::get_token_or_error(&self.current_token.clone())? {
            Token::Assign => {}
            illegal_token => {
                return Err(
                    ErrorKind::ExpectedSpecificToken(Token::Assign, illegal_token.clone()).into(),
                );
            }
        };

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        self.next_token();

        Ok(Statement::Let(LetStatement::new(identifier, value)))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParsingResult<Expression> {
        let mut left_expression = self.parse_prefix()?;

        'parsing_expression: while let Some(following_token) = &self.peek_token {
            if let Some(operator) = Self::token_to_infix_operator(&following_token) {
                if Precedence::from(&operator) <= precedence {
                    break 'parsing_expression;
                }
            } else if *following_token != Token::OpeningParenthesis {
                break 'parsing_expression;
            }

            self.next_token();

            left_expression = self.parse_infix(left_expression)?;
        }

        Ok(left_expression)
    }

    fn parse_prefix_expression(&mut self) -> ParsingResult<Expression> {
        let operator = match Self::get_token_or_error(&self.current_token)? {
            Token::Bang => ExpressionOperator::Bang,
            Token::Minus => ExpressionOperator::Minus,
            token => {
                return Err(ErrorKind::ExpectedPrefixOperator(token.clone()).into());
            }
        };

        self.next_token();

        Ok(Expression::PrefixOperation(PrefixOperationExpression::new(
            operator,
            self.parse_expression(Precedence::Prefix)?,
        )))
    }

    fn parse_grouped_expression(&mut self) -> ParsingResult<Expression> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        Self::expect_token_or_error(&self.peek_token, Token::ClosingParenthesis)?;

        self.next_token();

        expression
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

    fn parse_function_expression(&mut self) -> ParsingResult<Expression> {
        self.next_token();

        Self::expect_token_or_error(&self.current_token, Token::OpeningParenthesis)?;

        self.next_token();

        let parameters = self.parse_function_expression_parameters()?;

        self.next_token();

        Self::expect_token_or_error(&self.current_token, Token::OpeningBrace)?;

        let body = self.parse_block_statement()?;

        Self::expect_token_or_error(&self.current_token, Token::ClosingBrace)?;

        self.next_token();

        Ok(Expression::Function(FunctionExpression::new(
            parameters, body,
        )))
    }

    fn parse_function_call_expression(
        &mut self,
        function_expression: Expression,
    ) -> ParsingResult<Expression> {
        self.next_token();

        let arguments = self.parse_function_call_arguments()?;

        Ok(Expression::FunctionCall(FunctionCallExpression::new(
            function_expression,
            arguments,
        )))
    }

    fn parse_function_call_arguments(&mut self) -> ParsingResult<Vec<Expression>> {
        let mut arguments = Vec::new();

        match Self::get_token_or_error(&self.current_token)? {
            Token::ClosingParenthesis => return Ok(arguments),
            _ => arguments.push(self.parse_expression(Precedence::Lowest)?),
        }

        self.next_token();

        loop {
            if self.current_token != Some(Token::Comma) {
                break;
            }

            self.next_token();

            arguments.push(self.parse_expression(Precedence::Lowest)?);

            self.next_token();
        }

        Self::expect_token_or_error(&self.current_token, Token::ClosingParenthesis)?;

        Ok(arguments)
    }

    fn parse_function_expression_parameters(&mut self) -> ParsingResult<Vec<Identifier>> {
        let mut identifiers = Vec::new();

        match Self::get_token_or_error(&self.current_token)? {
            Token::ClosingParenthesis => return Ok(identifiers),
            Token::Identifier(literal) => identifiers.push(Identifier::new(literal.clone())),
            illegal_token => {
                return Err(ErrorKind::ExpectedIdentifier(illegal_token.clone()).into())
            }
        }

        self.next_token();

        loop {
            if self.current_token != Some(Token::Comma) {
                break;
            }

            self.next_token();

            match Self::get_token_or_error(&self.current_token)? {
                Token::Identifier(literal) => identifiers.push(Identifier::new(literal.clone())),
                illegal_token => {
                    return Err(ErrorKind::ExpectedIdentifier(illegal_token.clone()).into())
                }
            }

            self.next_token();
        }

        Self::expect_token_or_error(&self.current_token, Token::ClosingParenthesis)?;

        Ok(identifiers)
    }

    fn parse_if_expression(&mut self) -> ParsingResult<Expression> {
        self.next_token();

        Self::expect_token_or_error(&self.current_token, Token::OpeningParenthesis)?;

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.next_token();

        Self::expect_token_or_error(&self.current_token, Token::ClosingParenthesis)?;

        self.next_token();

        Self::expect_token_or_error(&self.current_token, Token::OpeningBrace)?;

        let consequence = self.parse_block_statement()?;

        Self::expect_token_or_error(&self.current_token, Token::ClosingBrace)?;

        self.next_token();

        // in Indonesian, "else" is translated by a group of 2 words: "jika tidak" (literally "if not")
        // so we must check the current token (if) + the following token (not) to detect "else"
        let is_else_expression =
            self.current_token == Some(Token::If) && self.peek_token == Some(Token::Not);
        let alternative = if is_else_expression {
            // skip the "if not" a.k.a. "else" tokens
            self.next_token();
            self.next_token();

            Self::expect_token_or_error(&self.current_token, Token::OpeningBrace)?;

            let alternative_consequence = self.parse_block_statement()?;

            Self::expect_token_or_error(&self.current_token, Token::ClosingBrace)?;

            self.next_token();

            Some(alternative_consequence)
        } else {
            None
        };

        Ok(Expression::Conditional(ConditionalExpression::new(
            condition,
            consequence,
            alternative,
        )))
    }

    fn parse_block_statement(&mut self) -> ParsingResult<BlockStatement> {
        let mut statements = Vec::new();

        self.next_token();

        while let Some(token) = &self.current_token {
            if *token == Token::ClosingBrace {
                break;
            }

            let statement = self.parse_statement()?;

            if self.current_token == Some(Token::Semicolon) {
                self.next_token();
            }

            statements.push(statement);
        }

        Ok(BlockStatement::new(statements))
    }

    fn parse_infix_expression(&mut self, left_expression: Expression) -> ParsingResult<Expression> {
        let operator = Self::get_token_or_error(&self.current_token).and_then(|token| {
            Self::token_to_infix_operator(&token).map_or_else(
                || Err(ErrorKind::ExpectedInfixOperator(token.clone()).into()),
                Ok,
            )
        })?;

        self.next_token();

        let precedence = Precedence::from(&operator);

        Ok(Expression::InfixOperation(InfixOperationExpression::new(
            operator,
            left_expression,
            self.parse_expression(precedence)?,
        )))
    }

    fn expect_token_or_error(
        tentative_token: &Option<Token>,
        expected_token: Token,
    ) -> ParsingResult<()> {
        let token = Self::get_token_or_error(tentative_token)?;

        if *token != expected_token {
            Err(ErrorKind::ExpectedSpecificToken(expected_token, token.clone()).into())
        } else {
            Ok(())
        }
    }

    fn get_token_or_error(tentative_token: &Option<Token>) -> ParsingResult<&Token> {
        match tentative_token {
            Some(token) => Ok(token),
            None => Err(ErrorKind::ExpectedToken.into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::representations::ast::{
        BlockStatement, ConditionalExpression, Expression, ExpressionOperator,
        FunctionCallExpression, FunctionExpression, Identifier, InfixOperationExpression,
        LetStatement, PrefixOperationExpression, ReturnStatement, Statement,
    };
    use crate::lexer::Lexer;
    use crate::representations::token::Literal;

    fn parse(input: &str) -> Result<super::Program, String> {
        let mut lexer = Lexer::new(input);

        super::Parser::new(&mut lexer)
            .parse_program()
            .map_err(|errors| format!("parse_program returned errors {:?}", errors))
    }

    #[test]
    fn let_statements() {
        let input = "
            diketahui x = 5;
            diketahui y = 10;
            diketahui foobar = 838383;
            diketahui z = benar;
            diketahui foobar_bar = foobar;
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
            Statement::Let(LetStatement::new(
                Identifier::new(Literal("z".to_string())),
                Expression::Boolean(true),
            )),
            Statement::Let(LetStatement::new(
                Identifier::new(Literal("foobar_bar".to_string())),
                Expression::Identifier(Identifier::new(Literal("foobar".to_string()))),
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
            kembalikan salah;
            kembalikan 3*2;
        ";

        let program = parse(input).unwrap();

        let expected_statements = vec![
            Statement::Return(ReturnStatement::new(Expression::Integer(5))),
            Statement::Return(ReturnStatement::new(Expression::Integer(10))),
            Statement::Return(ReturnStatement::new(Expression::Integer(993_322))),
            Statement::Return(ReturnStatement::new(Expression::Boolean(false))),
            Statement::Return(ReturnStatement::new(Expression::InfixOperation(
                InfixOperationExpression::new(
                    ExpressionOperator::Multiply,
                    Expression::Integer(3),
                    Expression::Integer(2),
                ),
            ))),
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

        let expected_identifier_expression = Statement::Expression(Expression::Identifier(
            Identifier::new(Literal("foobar".to_string())),
        ));

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
            Statement::Expression(Expression::PrefixOperation(PrefixOperationExpression::new(
                ExpressionOperator::Bang,
                Expression::Integer(5),
            ))),
            Statement::Expression(Expression::PrefixOperation(PrefixOperationExpression::new(
                ExpressionOperator::Minus,
                Expression::Integer(15),
            ))),
            Statement::Expression(Expression::PrefixOperation(PrefixOperationExpression::new(
                ExpressionOperator::Bang,
                Expression::Boolean(true),
            ))),
            Statement::Expression(Expression::PrefixOperation(PrefixOperationExpression::new(
                ExpressionOperator::Bang,
                Expression::Boolean(false),
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
            benar == benar;
            benar != salah;
            salah == salah;
        "#;

        let program = parse(input).unwrap();

        let expected_expressions = vec![
            Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
                ExpressionOperator::Plus,
                Expression::Integer(5),
                Expression::Integer(6),
            ))),
            Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
                ExpressionOperator::Plus,
                Expression::Integer(5),
                Expression::Identifier(Identifier::new(Literal("ab".to_string()))),
            ))),
            Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
                ExpressionOperator::Minus,
                Expression::Integer(5),
                Expression::Integer(6),
            ))),
            Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
                ExpressionOperator::Multiply,
                Expression::Integer(5),
                Expression::Integer(6),
            ))),
            Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
                ExpressionOperator::Divide,
                Expression::Integer(5),
                Expression::Integer(6),
            ))),
            Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
                ExpressionOperator::GreaterThan,
                Expression::Integer(5),
                Expression::Integer(6),
            ))),
            Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
                ExpressionOperator::LessThan,
                Expression::Integer(5),
                Expression::Integer(6),
            ))),
            Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
                ExpressionOperator::Equal,
                Expression::Integer(5),
                Expression::Integer(6),
            ))),
            Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
                ExpressionOperator::NotEqual,
                Expression::Integer(5),
                Expression::Integer(6),
            ))),
            Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
                ExpressionOperator::Equal,
                Expression::Boolean(true),
                Expression::Boolean(true),
            ))),
            Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
                ExpressionOperator::NotEqual,
                Expression::Boolean(true),
                Expression::Boolean(false),
            ))),
            Statement::Expression(Expression::InfixOperation(InfixOperationExpression::new(
                ExpressionOperator::Equal,
                Expression::Boolean(false),
                Expression::Boolean(false),
            ))),
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
            InputAndExpected {
                input: "1 + (2 + 3) + 4",
                expected: "((1 + (2 + 3)) + 4);",
            },
            InputAndExpected {
                input: "(5 + 5) * 2",
                expected: "((5 + 5) * 2);",
            },
            InputAndExpected {
                input: "2 / (5 + 5)",
                expected: "(2 / (5 + 5));",
            },
            InputAndExpected {
                input: "-(5 + 5)",
                expected: "(-(5 + 5));",
            },
            InputAndExpected {
                input: "!(true == true)",
                expected: "(!(true == true));",
            },
            InputAndExpected {
                input: "a + add(b * c) + d",
                expected: "((a + add((b * c))) + d);",
            },
            InputAndExpected {
                input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));",
            },
            InputAndExpected {
                input: "add(a + b + c * d / f + g)",
                expected: "add((((a + b) + ((c * d) / f)) + g));",
            },
        ];

        for input_and_expected in input {
            let program = parse(input_and_expected.input).unwrap();

            assert_eq!(format!("{}", program), input_and_expected.expected);
        }
    }

    #[test]
    fn if_expression() {
        let input = "jika (x < y) { x }";

        let program = parse(input).unwrap();

        let expected_statements = vec![Statement::Expression(Expression::Conditional(
            ConditionalExpression::new(
                Expression::InfixOperation(InfixOperationExpression::new(
                    ExpressionOperator::LessThan,
                    Expression::Identifier(Identifier::new(Literal("x".to_string()))),
                    Expression::Identifier(Identifier::new(Literal("y".to_string()))),
                )),
                BlockStatement::new(vec![Statement::Expression(Expression::Identifier(
                    Identifier::new(Literal("x".to_string())),
                ))]),
                None,
            ),
        ))];

        assert_eq!(program.statements, expected_statements);
    }

    #[test]
    fn if_else_expression() {
        let input = "jika (x < y) { x } jika tidak { y }";

        let program = parse(input).unwrap();

        let expected_statements = vec![Statement::Expression(Expression::Conditional(
            ConditionalExpression::new(
                Expression::InfixOperation(InfixOperationExpression::new(
                    ExpressionOperator::LessThan,
                    Expression::Identifier(Identifier::new(Literal("x".to_string()))),
                    Expression::Identifier(Identifier::new(Literal("y".to_string()))),
                )),
                BlockStatement::new(vec![Statement::Expression(Expression::Identifier(
                    Identifier::new(Literal("x".to_string())),
                ))]),
                Some(BlockStatement::new(vec![Statement::Expression(
                    Expression::Identifier(Identifier::new(Literal("y".to_string()))),
                )])),
            ),
        ))];

        assert_eq!(program.statements, expected_statements);
    }

    #[test]
    fn function_expression() {
        let input = "fungsi (x, y) { x + y; }";

        let program = parse(input).unwrap();

        let expected_statements = vec![Statement::Expression(Expression::Function(
            FunctionExpression::new(
                vec![
                    Identifier::new(Literal("x".to_string())),
                    Identifier::new(Literal("y".to_string())),
                ],
                BlockStatement::new(vec![Statement::Expression(Expression::InfixOperation(
                    InfixOperationExpression::new(
                        ExpressionOperator::Plus,
                        Expression::Identifier(Identifier::new(Literal("x".to_string()))),
                        Expression::Identifier(Identifier::new(Literal("y".to_string()))),
                    ),
                ))]),
            ),
        ))];

        assert_eq!(program.statements, expected_statements);
    }

    #[test]
    fn function_parameters() {
        struct InputAndExpected {
            input: &'static str,
            expected: Vec<Statement>,
        }

        let input = vec![
            InputAndExpected {
                input: "fungsi() {};",
                expected: vec![Statement::Expression(Expression::Function(
                    FunctionExpression::new(vec![], BlockStatement::new(vec![])),
                ))],
            },
            InputAndExpected {
                input: "fungsi(x) {};",
                expected: vec![Statement::Expression(Expression::Function(
                    FunctionExpression::new(
                        vec![Identifier::new(Literal("x".to_string()))],
                        BlockStatement::new(vec![]),
                    ),
                ))],
            },
            InputAndExpected {
                input: "fungsi(x, y, z) {};",
                expected: vec![Statement::Expression(Expression::Function(
                    FunctionExpression::new(
                        vec![
                            Identifier::new(Literal("x".to_string())),
                            Identifier::new(Literal("y".to_string())),
                            Identifier::new(Literal("z".to_string())),
                        ],
                        BlockStatement::new(vec![]),
                    ),
                ))],
            },
        ];

        for input_and_expected in input {
            let program = parse(input_and_expected.input).unwrap();

            assert_eq!(program.statements, input_and_expected.expected);
        }
    }

    #[test]
    fn function_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5); ";

        let program = parse(input).unwrap();

        let expected_statements = vec![Statement::Expression(Expression::FunctionCall(
            FunctionCallExpression::new(
                Expression::Identifier(Identifier::new(Literal("add".to_string()))),
                vec![
                    Expression::Integer(1),
                    Expression::InfixOperation(InfixOperationExpression::new(
                        ExpressionOperator::Multiply,
                        Expression::Integer(2),
                        Expression::Integer(3),
                    )),
                    Expression::InfixOperation(InfixOperationExpression::new(
                        ExpressionOperator::Plus,
                        Expression::Integer(4),
                        Expression::Integer(5),
                    )),
                ],
            ),
        ))];

        assert_eq!(program.statements, expected_statements);
    }
}
