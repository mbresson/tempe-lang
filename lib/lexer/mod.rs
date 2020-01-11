use crate::representations::token::{keywords, Context, Literal, Token, TokenWithContext};

/// A Lexer iterates over a text and returns tokens.
pub struct Lexer<'a> {
    chars_iterator: std::str::Chars<'a>,

    current_char: Option<char>,
    current_line: usize,
    current_column: usize,
}

impl<'a> Lexer<'a> {
    /// # Examples
    ///
    /// ```
    /// use tempe_lang::lexer::Lexer;
    ///
    /// let source_code = "
    /// 1 + 1
    /// ";
    ///
    /// let mut lexer = Lexer::new(source_code);
    /// ```
    pub fn new(text: &'a str) -> Self {
        let mut chars_iterator = text.chars();
        let current_char = chars_iterator.next();

        Lexer {
            chars_iterator,
            current_char,
            current_line: 0,
            current_column: 0,
        }
    }

    fn is_new_line(ch: char) -> bool {
        // for commodity purposes, we choose to consider only '\n' as a newline,
        // ignoring \r (\r\n, \r\r\n, etc will hence amount to only one single new line)
        ch == '\n'
    }

    fn advance_to_next_char(&mut self) -> Option<char> {
        self.current_char = self.chars_iterator.next();

        self.current_column += 1;
        if let Some(ch) = self.current_char {
            if Self::is_new_line(ch) {
                self.current_line += 1;
                self.current_column = 0;
            }
        }

        self.current_char
    }

    fn get_next_non_whitespace_char_or_eof(&mut self) -> Option<char> {
        while let Some(current_char) = self.advance_to_next_char() {
            if !current_char.is_whitespace() {
                return Some(current_char);
            }
        }

        None
    }

    // returns None if the literal doesn't correspond to a known language keyword
    fn literal_to_keyword(literal: &str) -> Option<Token> {
        match literal {
            keywords::FUNCTION => Some(Token::Function),
            keywords::LET => Some(Token::Let),
            keywords::TRUE => Some(Token::True),
            keywords::FALSE => Some(Token::False),
            keywords::IF => Some(Token::If),
            keywords::NOT => Some(Token::Not),
            keywords::RETURN => Some(Token::Return),
            _ => None,
        }
    }

    fn parse_identifier_or_keyword(literal: &str) -> Token {
        match Self::literal_to_keyword(literal) {
            Some(keyword) => keyword,
            None => Token::Identifier(Literal(literal.to_string())),
        }
    }

    // identifiers and keywords are sequences or letters, digits and/or underscores, and must start with a letter
    //
    // e.g.
    // 'abcd' is an identifier
    // 'a_42_' is an identifier
    // 'fungsi' is a keyword (because it is one of the defined language keywords)
    //
    // this function assumes that `first_letter` is indeed a letter
    fn read_identifier_or_keyword(&mut self, first_letter: char) -> Token {
        let mut token_buffer = first_letter.to_string();

        // read following characters until the first char that is not a letter, digit or underscore
        while let Some(current_char) = self.advance_to_next_char() {
            if current_char.is_alphanumeric() || current_char == '_' {
                token_buffer.push(current_char);
            } else {
                self.current_char = Some(current_char);

                return Self::parse_identifier_or_keyword(&token_buffer);
            }
        }

        // or until no more char can be read (EOF)
        self.current_char = None;

        Self::parse_identifier_or_keyword(&token_buffer)
    }

    fn read_str(&mut self) -> Token {
        let mut token_buffer = String::new();

        let mut is_escaped_character = false;

        // read following characters until a closing '"' character
        while let Some(current_char) = self.advance_to_next_char() {
            if is_escaped_character {
                token_buffer.push(current_char);
                is_escaped_character = false;
                continue;
            }

            match current_char {
                '"' => {
                    self.advance_to_next_char();
                    return Token::Str(token_buffer);
                }
                '\\' => {
                    is_escaped_character = true;
                }
                _ => {
                    token_buffer.push(current_char);
                }
            }
        }

        // or until no more char can be read (EOF)
        self.current_char = None;

        // the string was not terminated by a '"', so it's an illegal token
        Token::Illegal(Literal(format!("\"{}", token_buffer)))
    }

    fn parse_base_10_integer(literal: &str) -> Token {
        match literal.parse::<i64>() {
            Ok(integer) => Token::Integer(integer),
            Err(parsing_error) => {
                // this should never happen, since at this stage, token_buffer should only contain digits,
                // however:
                // if the literal represents a number too big to fit in the i64 type, we will get an error
                // in a production lexer we should return a meaningful error type, but this is just a toy project,
                // so we'll be content with returning Token::Illegal and printing the real error to stderr
                eprintln!(
                    "Error while trying to parse sequence of digits '{}' to integer: {}",
                    literal, parsing_error
                );
                Token::Illegal(Literal(literal.to_string()))
            }
        }
    }

    // integers are sequences of digits
    //
    // e.g
    // '42' is an integer
    // '09' is an integer
    // '13.4' is not a valid integer
    //
    // this function assumes that `first_digit`  is indeed a digit
    fn read_integer(&mut self, first_digit: char) -> Token {
        let mut token_buffer = first_digit.to_string();

        let number_base = 10;

        // read following characters until the first non-digit
        while let Some(current_char) = self.advance_to_next_char() {
            if current_char.is_digit(number_base) {
                token_buffer.push(current_char);
            } else if current_char.is_alphabetic() || current_char == '.' {
                token_buffer.push(current_char);

                'read_until_end_of_illegal_sequence: loop {
                    if let Some(next_char) = self.advance_to_next_char() {
                        if next_char.is_alphanumeric() {
                            token_buffer.push(next_char);
                        } else {
                            self.current_char = Some(next_char);
                            break 'read_until_end_of_illegal_sequence;
                        }
                    } else {
                        self.current_char = None;
                        break 'read_until_end_of_illegal_sequence;
                    }
                }

                return Token::Illegal(Literal(token_buffer));
            } else {
                self.current_char = Some(current_char);

                return Self::parse_base_10_integer(&token_buffer);
            }
        }

        // or until no more char can be read (EOF)
        self.current_char = None;

        Self::parse_base_10_integer(&token_buffer)
    }

    // symbols are predefined sequences of one or more characters that are neither letters, nor digits or whitespaces
    //
    // e.g.
    // '{' is the opening brace symbol
    // '==' is the equality check symbol and is an operator
    // '_' is not a recognized symbol, so it is invalid
    fn read_special_symbol(&mut self, first_char: char) -> Token {
        match first_char {
            '(' => {
                self.current_char = self.advance_to_next_char();

                Token::OpeningParenthesis
            }
            ')' => {
                self.current_char = self.advance_to_next_char();

                Token::ClosingParenthesis
            }
            '[' => {
                self.current_char = self.advance_to_next_char();

                Token::OpeningBracket
            }
            ']' => {
                self.current_char = self.advance_to_next_char();

                Token::ClosingBracket
            }
            '{' => {
                self.current_char = self.advance_to_next_char();

                Token::OpeningBrace
            }
            '}' => {
                self.current_char = self.advance_to_next_char();

                Token::ClosingBrace
            }
            ',' => {
                self.current_char = self.advance_to_next_char();

                Token::Comma
            }
            ';' => {
                self.current_char = self.advance_to_next_char();

                Token::Semicolon
            }
            ':' => {
                self.current_char = self.advance_to_next_char();

                Token::Colon
            }
            '+' => {
                self.current_char = self.advance_to_next_char();

                Token::Plus
            }
            '-' => {
                self.current_char = self.advance_to_next_char();

                Token::Minus
            }
            '*' => {
                self.current_char = self.advance_to_next_char();

                Token::Asterisk
            }
            '/' => {
                self.current_char = self.advance_to_next_char();

                Token::Slash
            }
            '<' => {
                self.current_char = self.advance_to_next_char();

                Token::LessThan
            }
            '>' => {
                self.current_char = self.advance_to_next_char();

                Token::GreaterThan
            }
            '=' => {
                // can be either Assign ("=") or Equal ("==")

                match self.advance_to_next_char() {
                    Some('=') => {
                        self.current_char = self.advance_to_next_char();

                        Token::Equal
                    }
                    Some(other_char) => {
                        self.current_char = Some(other_char);

                        Token::Assign
                    }
                    None => {
                        self.current_char = None;

                        Token::Assign
                    }
                }
            }
            '!' => {
                // can be either Bang ("!") or NotEqual ("!=")

                match self.advance_to_next_char() {
                    Some('=') => {
                        self.current_char = self.advance_to_next_char();

                        Token::NotEqual
                    }
                    Some(other_char) => {
                        self.current_char = Some(other_char);

                        Token::Bang
                    }
                    None => {
                        self.current_char = None;

                        Token::Bang
                    }
                }
            }
            illegal => {
                self.current_char = self.advance_to_next_char();

                Token::Illegal(Literal(illegal.to_string()))
            }
        }
    }

    fn read_token(&mut self, first_char: char) -> Option<TokenWithContext> {
        let mut current_char = first_char;
        let mut start_line = self.current_line;
        let mut start_column = self.current_column;

        if current_char.is_whitespace() {
            if let Some(next_char) = self.get_next_non_whitespace_char_or_eof() {
                start_line = self.current_line;
                start_column = self.current_column;

                current_char = next_char;
            } else {
                return None;
            }
        }

        let token = if current_char.is_alphabetic() {
            self.read_identifier_or_keyword(current_char)
        } else if current_char.is_ascii_digit() {
            self.read_integer(current_char)
        } else if current_char == '"' {
            self.read_str()
        } else {
            self.read_special_symbol(current_char)
        };

        Some(TokenWithContext {
            token,
            context: Context {
                start_line,
                start_column,
                end_line: self.current_line,
                end_column: self.current_column,
            },
        })
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = TokenWithContext;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current_char) = self.current_char {
            self.read_token(current_char)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::representations::token::{Literal, Token, TokenWithContext};

    #[test]
    fn next_token_from_code_snippet() {
        // the test input is the same as the one in the book Writing An Interpreter In Go,
        // except that Monkey code is translated to tempe-lang code
        // and a few things are changed, to test corner cases (e.g. an identifier containing an underscore)
        let input = "

            diketahui five = 5;
            diketahui number10 = 10;

            diketahui add = fungsi(x, y) {
                x + y;
            };

            diketahui the_result = add(five, number10);
            !-/*5;
            5 < 10 > 5;
            4aa * 9

            $

            jika (5 < 10) {
                kembalikan benar;
            } jika tidak {
                kembalikan salah;
            }

            10 == 10;
            10 != 9;

            \"哈囉！สวัสดีครับ \\\"Salam!\\\" привет!\"

            [1, [benar, \"ini rumit\"]];
            {\"foo\": \"bar\"}
        ";

        let expected_tokens = vec![
            // diketahui five = 5;
            Token::Let,
            Token::Identifier(Literal("five".to_string())),
            Token::Assign,
            Token::Integer(5),
            Token::Semicolon,
            //
            // diketahui number10 = 10;
            Token::Let,
            Token::Identifier(Literal("number10".to_string())),
            Token::Assign,
            Token::Integer(10),
            Token::Semicolon,
            //
            // diketahui add = fungsi(x, y) {
            Token::Let,
            Token::Identifier(Literal("add".to_string())),
            Token::Assign,
            Token::Function,
            Token::OpeningParenthesis,
            Token::Identifier(Literal("x".to_string())),
            Token::Comma,
            Token::Identifier(Literal("y".to_string())),
            Token::ClosingParenthesis,
            Token::OpeningBrace,
            //
            // x + y;
            Token::Identifier(Literal("x".to_string())),
            Token::Plus,
            Token::Identifier(Literal("y".to_string())),
            Token::Semicolon,
            //
            // };
            Token::ClosingBrace,
            Token::Semicolon,
            //
            // diketahui the_result = add(five, number10);
            Token::Let,
            Token::Identifier(Literal("the_result".to_string())),
            Token::Assign,
            Token::Identifier(Literal("add".to_string())),
            Token::OpeningParenthesis,
            Token::Identifier(Literal("five".to_string())),
            Token::Comma,
            Token::Identifier(Literal("number10".to_string())),
            Token::ClosingParenthesis,
            Token::Semicolon,
            //
            // !-/*5;
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Integer(5),
            Token::Semicolon,
            //
            // 5 < 10 > 5;
            Token::Integer(5),
            Token::LessThan,
            Token::Integer(10),
            Token::GreaterThan,
            Token::Integer(5),
            Token::Semicolon,
            //
            // 4aa * 9
            Token::Illegal(Literal("4aa".to_string())),
            Token::Asterisk,
            Token::Integer(9),
            //
            // $
            Token::Illegal(Literal("$".to_string())),
            //
            // jika (5 < 10) {
            Token::If,
            Token::OpeningParenthesis,
            Token::Integer(5),
            Token::LessThan,
            Token::Integer(10),
            Token::ClosingParenthesis,
            Token::OpeningBrace,
            //
            // kembalikan true;
            Token::Return,
            Token::True,
            Token::Semicolon,
            //
            // } jika tidak {
            Token::ClosingBrace,
            Token::If,
            Token::Not,
            Token::OpeningBrace,
            //
            // kembalikan false;
            Token::Return,
            Token::False,
            Token::Semicolon,
            //
            // }
            Token::ClosingBrace,
            //
            // 10 == 10;
            Token::Integer(10),
            Token::Equal,
            Token::Integer(10),
            Token::Semicolon,
            //
            // 10 != 9;
            Token::Integer(10),
            Token::NotEqual,
            Token::Integer(9),
            Token::Semicolon,
            //
            // "哈囉！สวัสดีครับ \"Salam!\"" привет!"
            Token::Str("哈囉！สวัสดีครับ \"Salam!\" привет!".to_string()),
            //
            // [1, [benar, \"ini rumit\"]];
            Token::OpeningBracket,
            Token::Integer(1),
            Token::Comma,
            Token::OpeningBracket,
            Token::True,
            Token::Comma,
            Token::Str("ini rumit".to_string()),
            Token::ClosingBracket,
            Token::ClosingBracket,
            Token::Semicolon,
            //
            // {\"foo\": \"bar\"}
            Token::OpeningBrace,
            Token::Str("foo".to_string()),
            Token::Colon,
            Token::Str("bar".to_string()),
            Token::ClosingBrace,
        ];

        let lexer = super::Lexer::new(input);

        let tokens = lexer
            .map(|TokenWithContext { token, .. }| token)
            .collect::<Vec<Token>>();

        assert_eq!(tokens, expected_tokens);
    }

    #[test]
    fn next_token_from_single_line() {
        let input = "=+(){},;";

        let expected_tokens = vec![
            Token::Assign,
            Token::Plus,
            Token::OpeningParenthesis,
            Token::ClosingParenthesis,
            Token::OpeningBrace,
            Token::ClosingBrace,
            Token::Comma,
            Token::Semicolon,
        ];

        let mut lexer = super::Lexer::new(input);

        for expected_token in expected_tokens {
            match lexer.next() {
                Some(TokenWithContext { token, .. }) => {
                    assert_eq!(token, expected_token);
                }
                None => {
                    panic!("Expected to get a token");
                }
            }
        }
    }

    #[test]
    fn next_token_from_single_line_with_prefix_operator() {
        let input = "-15, -ab, -ab()";

        let expected_tokens = vec![
            Token::Minus,
            Token::Integer(15),
            Token::Comma,
            Token::Minus,
            Token::Identifier(Literal("ab".to_string())),
            Token::Comma,
            Token::Minus,
            Token::Identifier(Literal("ab".to_string())),
            Token::OpeningParenthesis,
            Token::ClosingParenthesis,
        ];

        let mut lexer = super::Lexer::new(input);

        for expected_token in expected_tokens {
            match lexer.next() {
                Some(TokenWithContext { token, .. }) => {
                    assert_eq!(token, expected_token);
                }
                None => {
                    panic!("Expected to get a token");
                }
            }
        }
    }
}
