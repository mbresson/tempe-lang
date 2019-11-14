use crate::ast::{
    Expression, ExpressionOperator, ExpressionStatement, Identifier, LetStatement, Precedence,
    PrefixOperationExpression, Program, ReturnStatement, Statement,
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
}

impl TryFrom<&Token> for ParseableToken {
    type Error = String;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token {
            Token::Identifier(_) => Ok(ParseableToken::Identifier),
            Token::Integer(_) => Ok(ParseableToken::Integer),
            Token::Bang => Ok(ParseableToken::Bang),
            Token::Minus => Ok(ParseableToken::Minus),
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

        parser.register_prefix_parser(ParseableToken::Identifier, &Self::parse_identifier);
        parser.register_prefix_parser(ParseableToken::Integer, &Self::parse_integer);
        parser.register_prefix_parser(ParseableToken::Bang, &Self::parse_prefix_expression);
        parser.register_prefix_parser(ParseableToken::Minus, &Self::parse_prefix_expression);

        parser.next_token();
        parser.next_token();

        parser
    }

    fn register_prefix_parser(
        &mut self,
        token: ParseableToken,
        parser: &'a PrefixParseFunction<'a>,
    ) {
        self.prefix_parse_functions.insert(token, parser);
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

        println!("TOKEN == {:?}\n\n", token);

        let prefix_parser = match self.prefix_parse_functions.get(&token) {
            Some(parser) => *parser,
            _ => return Err(format!("cannot parse token {:?} as prefix", token)),
        };

        let left_expression = prefix_parser(self);

        left_expression
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
        Expression, ExpressionOperator, ExpressionStatement, Identifier, LetStatement,
        PrefixOperationExpression, ReturnStatement, Statement,
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

        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(errors) => {
                panic!(format!("parse_program returned errors {:?}", errors));
            }
        };

        if program.statements.len() != 3 {
            panic!(format!(
                "program.statements does not contain 3 statements, got {}",
                program.statements.len()
            ));
        }

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

        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(errors) => {
                panic!(format!("parse_program returned errors {:?}", errors));
            }
        };

        if program.statements.len() != 3 {
            panic!(format!(
                "program.statements does not contain 3 statements, got {}",
                program.statements.len()
            ));
        }

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

        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(errors) => {
                panic!(format!("parse_program returned errors {:?}", errors));
            }
        };

        if program.statements.len() != 1 {
            panic!(format!(
                "program.statements does not contain 1 statement, got {}",
                program.statements.len()
            ));
        }

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

        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(errors) => {
                panic!(format!("parse_program returned errors {:?}", errors));
            }
        };

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

        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(errors) => {
                panic!(format!("parse_program returned errors {:?}", errors));
            }
        };

        if program.statements.len() != 2 {
            panic!(format!(
                "program.statements does not contain 2 statements, got {}",
                program.statements.len()
            ));
        }

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
}
