use crate::representations::ast::{
    BlockStatement, ConditionalExpression, Expression, ExpressionOperator, FunctionCallExpression,
    FunctionExpression, HashMapKeyValue, Identifier, IndexOperationExpression,
    InfixOperationExpression, LetStatement, Precedence, PrefixOperationExpression, Program,
    ReturnStatement, Statement,
};
use crate::representations::token::{Context, Token, TokenWithContext};
use std::vec::Vec;

pub mod errors;
use errors::{Error, ErrorKind, Result as ParsingResult};

pub struct Parser<'a> {
    token_iterator: &'a mut dyn Iterator<Item = TokenWithContext>,

    previous_context: Context,

    current_token: Option<TokenWithContext>,
    peek_token: Option<TokenWithContext>,
}

impl<'a> Parser<'a> {
    /// # Examples
    ///
    /// ```
    /// use tempe_lang::lexer::Lexer;
    /// use tempe_lang::parser::Parser;
    ///
    /// let source_code = "
    /// 1 + 1
    /// ";
    ///
    /// let mut lexer = Lexer::new(source_code);
    ///
    /// let program = Parser::new(&mut lexer)
    ///   .parse_program()
    ///   .unwrap();
    /// ```

    pub fn new(token_iterator: &'a mut dyn Iterator<Item = TokenWithContext>) -> Self {
        let mut parser = Parser {
            token_iterator,
            previous_context: Context {
                start_line: 0,
                start_column: 0,
                end_line: 0,
                end_column: 0,
            },
            current_token: None,
            peek_token: None,
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    fn parse_infix(&mut self, left_expression: Expression) -> ParsingResult<Expression> {
        let token_with_context = self.get_token_or_error(&self.current_token)?;

        match &token_with_context.token {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::Equal
            | Token::NotEqual
            | Token::LessThan
            | Token::GreaterThan => self.parse_infix_expression(left_expression),
            Token::OpeningBracket => self.parse_index_expression(left_expression),
            Token::OpeningParenthesis => self.parse_function_call_expression(left_expression),
            _ => Err(ErrorKind::ExpectedInfixOperator(token_with_context.clone()).into()),
        }
    }

    fn parse_prefix(&mut self) -> ParsingResult<Expression> {
        let token_with_context = self.get_token_or_error(&self.current_token)?;

        match &token_with_context.token {
            Token::Identifier(literal) => {
                let identifier = Expression::Identifier(Identifier::new(literal.clone()));
                self.next_token();

                Ok(identifier)
            }
            Token::Integer(integer) => {
                let integer = Expression::Integer(*integer);
                self.next_token();

                Ok(integer)
            }
            Token::True => {
                self.next_token();
                Ok(Expression::Boolean(true))
            }
            Token::False => {
                self.next_token();
                Ok(Expression::Boolean(false))
            }
            Token::Str(string) => {
                let string = Expression::Str(string.clone());

                self.next_token();

                Ok(string)
            }
            Token::OpeningBrace => self.parse_hashmap_expression(),
            Token::OpeningBracket => self.parse_array_expression(),
            Token::Minus | Token::Bang => self.parse_prefix_expression(),
            Token::OpeningParenthesis => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_expression(),
            Token::Illegal(_) => Err(ErrorKind::IllegalToken(token_with_context.clone()).into()),
            _ => Err(ErrorKind::CannotParseTokenAsPrefix(token_with_context.clone()).into()),
        }
    }

    fn next_token(&mut self) {
        if let Some(token_with_context) = &self.current_token {
            self.previous_context = token_with_context.context;
        }

        self.current_token = self.peek_token.take();
        self.peek_token = self.token_iterator.next();
    }

    fn parse_statement(&mut self) -> ParsingResult<Statement> {
        let token_with_context = self.get_token_or_error(&self.current_token)?;

        match token_with_context.token {
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

            if let Some(token_with_context) = &self.current_token {
                if token_with_context.token == Token::Semicolon {
                    self.next_token();
                } else {
                    errors.push(ErrorKind::IllegalToken(token_with_context.clone()).into());
                    self.jump_to_next_statement();
                }
            }
        }

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(Program { statements })
        }
    }

    fn parse_expression_statement(&mut self) -> ParsingResult<Statement> {
        let expression = self.parse_expression(Precedence::Lowest);

        expression.map(Statement::Expression)
    }

    fn jump_to_next_statement(&mut self) {
        while let Some(token_with_context) = &self.current_token {
            if token_with_context.token == Token::Semicolon {
                self.next_token();
                return;
            }

            self.next_token();
        }
    }

    fn parse_return_statement(&mut self) -> ParsingResult<Statement> {
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        Ok(Statement::Return(ReturnStatement::new(value)))
    }

    fn parse_let_statement(&mut self) -> ParsingResult<Statement> {
        self.next_token();

        let identifier = match self.get_token_or_error(&self.current_token)? {
            TokenWithContext {
                token: Token::Identifier(literal),
                ..
            } => Identifier::new(literal.clone()),
            token => {
                return Err(ErrorKind::ExpectedIdentifier(token.clone()).into());
            }
        };

        self.next_token();

        self.expect_token_or_error(&self.current_token, Token::Assign)?;

        let previous_token = self.current_token.as_ref().unwrap().clone();

        self.next_token();

        if self.current_token.is_none() || Self::is_token(&self.current_token, &Token::Semicolon) {
            return Err(ErrorKind::ExpectedExpression(
                previous_token.context,
                self.current_token.clone(),
            )
            .into());
        }

        let value = self.parse_expression(Precedence::Lowest)?;

        Ok(Statement::Let(LetStatement::new(identifier, value)))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParsingResult<Expression> {
        let mut left_expression = self.parse_prefix()?;

        'parsing_expression: while let Some(current_token) = &self.current_token {
            if let Some(operator) = Self::token_to_infix_operator(&current_token.token) {
                if Precedence::from(&operator) <= precedence {
                    break 'parsing_expression;
                }
            } else if current_token.token != Token::OpeningParenthesis {
                break 'parsing_expression;
            }

            left_expression = self.parse_infix(left_expression)?;
        }

        Ok(left_expression)
    }

    fn parse_prefix_expression(&mut self) -> ParsingResult<Expression> {
        let token_with_context = self.get_token_or_error(&self.current_token)?;

        let operator = match &token_with_context.token {
            Token::Bang => ExpressionOperator::Bang,
            Token::Minus => ExpressionOperator::Minus,
            _ => {
                return Err(ErrorKind::ExpectedPrefixOperator(token_with_context.clone()).into());
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

        self.expect_token_or_error(&self.current_token, Token::ClosingParenthesis)?;

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
            Token::OpeningBracket => Some(ExpressionOperator::Indexer),
            _ => None,
        }
    }

    fn parse_function_expression(&mut self) -> ParsingResult<Expression> {
        self.next_token();

        self.expect_token_or_error(&self.current_token, Token::OpeningParenthesis)?;

        self.next_token();

        let parameters = self.parse_function_expression_parameters()?;

        self.next_token();

        self.expect_token_or_error(&self.current_token, Token::OpeningBrace)?;

        let body = self.parse_block_statement()?;

        self.expect_token_or_error(&self.current_token, Token::ClosingBrace)?;

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

    fn parse_hashmap_key_value_pair(&mut self) -> ParsingResult<HashMapKeyValue> {
        let key_expression = self.parse_expression(Precedence::Lowest)?;

        self.expect_token_or_error(&self.current_token, Token::Colon)?;

        self.next_token();

        let value_expression = self.parse_expression(Precedence::Lowest)?;

        Ok((key_expression, value_expression))
    }

    fn parse_hashmap_expression(&mut self) -> ParsingResult<Expression> {
        self.next_token();

        let mut key_value_pairs = Vec::new();

        loop {
            if Self::is_token(&self.current_token, &Token::ClosingBrace) {
                break;
            }

            let key_value = self.parse_hashmap_key_value_pair()?;

            key_value_pairs.push(key_value);

            let current_token_is_comma_between_items =
                Self::is_token(&self.current_token, &Token::Comma);

            if !current_token_is_comma_between_items {
                self.expect_token_or_error(&self.current_token, Token::ClosingBrace)?;

                break;
            }

            self.next_token();
        }

        self.next_token();

        Ok(Expression::HashLiteral(key_value_pairs))
    }

    fn parse_array_expression(&mut self) -> ParsingResult<Expression> {
        self.next_token();

        Ok(Expression::Array(
            self.parse_comma_separated_expression_list(Token::ClosingBracket)?,
        ))
    }

    fn parse_function_call_arguments(&mut self) -> ParsingResult<Vec<Expression>> {
        self.parse_comma_separated_expression_list(Token::ClosingParenthesis)
    }

    fn parse_function_expression_parameters(&mut self) -> ParsingResult<Vec<Identifier>> {
        let mut identifiers = Vec::new();

        loop {
            if Self::is_token(&self.current_token, &Token::ClosingParenthesis) {
                break;
            }

            match self.get_token_or_error(&self.current_token)? {
                TokenWithContext {
                    token: Token::Identifier(literal),
                    ..
                } => identifiers.push(Identifier::new(literal.clone())),
                illegal_token => {
                    return Err(ErrorKind::ExpectedIdentifier(illegal_token.clone()).into())
                }
            }

            self.next_token();

            let current_token_is_comma_between_items =
                Self::is_token(&self.current_token, &Token::Comma);

            if !current_token_is_comma_between_items {
                self.expect_token_or_error(&self.current_token, Token::ClosingParenthesis)?;

                break;
            }

            self.next_token();
        }

        Ok(identifiers)
    }

    fn parse_comma_separated_expression_list(
        &mut self,
        closing_token: Token,
    ) -> ParsingResult<Vec<Expression>> {
        let mut items = Vec::new();

        loop {
            if Self::is_token(&self.current_token, &closing_token) {
                break;
            }

            items.push(self.parse_expression(Precedence::Lowest)?);

            let current_token_is_comma_between_items =
                Self::is_token(&self.current_token, &Token::Comma);

            if !current_token_is_comma_between_items {
                self.expect_token_or_error(&self.current_token, closing_token)?;

                break;
            }

            self.next_token();
        }

        self.next_token();

        Ok(items)
    }

    fn parse_if_expression(&mut self) -> ParsingResult<Expression> {
        self.next_token();

        self.expect_token_or_error(&self.current_token, Token::OpeningParenthesis)?;

        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_token_or_error(&self.current_token, Token::ClosingParenthesis)?;

        self.next_token();

        self.expect_token_or_error(&self.current_token, Token::OpeningBrace)?;

        let consequence = self.parse_block_statement()?;

        self.expect_token_or_error(&self.current_token, Token::ClosingBrace)?;

        self.next_token();

        // in Indonesian, "else" is translated by a group of 2 words: "jika tidak" (literally "if not")
        // so we must check the current token (if) + the following token (not) to detect "else"
        let is_else_expression = Self::is_token(&self.current_token, &Token::If)
            && Self::is_token(&self.peek_token, &Token::Not);

        let alternative = if is_else_expression {
            // skip the "if not" a.k.a. "else" tokens
            self.next_token();
            self.next_token();

            self.expect_token_or_error(&self.current_token, Token::OpeningBrace)?;

            let alternative_consequence = self.parse_block_statement()?;

            self.expect_token_or_error(&self.current_token, Token::ClosingBrace)?;

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

        while let Some(token_with_context) = &self.current_token {
            if token_with_context.token == Token::ClosingBrace {
                break;
            }

            let statement = self.parse_statement()?;

            if Self::is_token(&self.current_token, &Token::Semicolon) {
                self.next_token();
            }

            statements.push(statement);
        }

        Ok(BlockStatement::new(statements))
    }

    fn parse_index_expression(&mut self, left_expression: Expression) -> ParsingResult<Expression> {
        self.next_token();

        let index = self.parse_expression(Precedence::Lowest)?;

        self.expect_token_or_error(&self.current_token, Token::ClosingBracket)?;

        self.next_token();

        Ok(Expression::IndexOperation(IndexOperationExpression::new(
            index,
            left_expression,
        )))
    }

    fn parse_infix_expression(&mut self, left_expression: Expression) -> ParsingResult<Expression> {
        let operator =
            self.get_token_or_error(&self.current_token)
                .and_then(|token_with_context| {
                    Self::token_to_infix_operator(&token_with_context.token).map_or_else(
                        || Err(ErrorKind::ExpectedInfixOperator(token_with_context.clone()).into()),
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
        &self,
        tentative_token: &Option<TokenWithContext>,
        expected_token: Token,
    ) -> ParsingResult<()> {
        let token_with_context = self.get_token_or_error(tentative_token)?;

        if token_with_context.token != expected_token {
            Err(ErrorKind::ExpectedSpecificToken(expected_token, token_with_context.clone()).into())
        } else {
            Ok(())
        }
    }

    fn is_token(tentative_token: &Option<TokenWithContext>, expected_token: &Token) -> bool {
        tentative_token
            .as_ref()
            .map_or(false, |token_with_context| {
                token_with_context.token == *expected_token
            })
    }

    fn get_token_or_error<'b>(
        &self,
        tentative_token: &'b Option<TokenWithContext>,
    ) -> ParsingResult<&'b TokenWithContext> {
        match tentative_token {
            Some(token) => Ok(token),
            None => Err(ErrorKind::ExpectedToken(
                self.current_token
                    .as_ref()
                    .map_or(self.previous_context, |token_with_context| {
                        token_with_context.context
                    }),
            )
            .into()),
        }
    }
}
