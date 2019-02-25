use crate::token::{keywords, Literal, Token};

/// A Lexer iterates over a text and returns tokens.
pub struct Lexer<'a> {
    chars_iterator: std::str::Chars<'a>,
    previous_char: Option<char>,
}

impl<'a> Lexer<'a> {
    /// # Examples
    ///
    /// ```
    /// let source_code = "
    /// 1 + 1
    /// ";
    ///
    /// let mut lexer = Lexer::new(source_code);
    /// ```
    pub fn new(text: &'a str) -> Self {
        let mut chars_iterator = text.chars();
        let first_char = chars_iterator.next();

        Lexer {
            chars_iterator,
            previous_char: first_char,
        }
    }

    fn get_next_non_whitespace_char_or_eof(&mut self) -> Option<char> {
        while let Some(current_char) = self.chars_iterator.next() {
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
        while let Some(current_char) = self.chars_iterator.next() {
            if current_char.is_alphanumeric() || current_char == '_' {
                token_buffer.push(current_char);
            } else {
                self.previous_char = Some(current_char);

                return Self::parse_identifier_or_keyword(&token_buffer);
            }
        }

        // or until no more char can be read (EOF)
        self.previous_char = None;

        Self::parse_identifier_or_keyword(&token_buffer)
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
        while let Some(current_char) = self.chars_iterator.next() {
            if current_char.is_digit(number_base) {
                token_buffer.push(current_char);
            } else if current_char.is_alphabetic() {
                token_buffer.push(current_char);

                'read_until_end_of_illegal_sequence: loop {
                    if let Some(next_char) = self.chars_iterator.next() {
                        if next_char.is_alphanumeric() {
                            token_buffer.push(next_char);
                        } else {
                            self.previous_char = Some(next_char);
                            break 'read_until_end_of_illegal_sequence;
                        }
                    } else {
                        self.previous_char = None;
                        break 'read_until_end_of_illegal_sequence;
                    }
                }

                return Token::Illegal(Literal(token_buffer));
            } else {
                self.previous_char = Some(current_char);

                return Self::parse_base_10_integer(&token_buffer);
            }
        }

        // or until no more char can be read (EOF)
        self.previous_char = None;

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
                self.previous_char = self.chars_iterator.next();

                Token::OpeningParenthesis
            }
            ')' => {
                self.previous_char = self.chars_iterator.next();

                Token::ClosingParenthesis
            }
            '{' => {
                self.previous_char = self.chars_iterator.next();

                Token::OpeningBrace
            }
            '}' => {
                self.previous_char = self.chars_iterator.next();

                Token::ClosingBrace
            }
            ',' => {
                self.previous_char = self.chars_iterator.next();

                Token::Comma
            }
            ';' => {
                self.previous_char = self.chars_iterator.next();

                Token::Semicolon
            }
            '+' => {
                self.previous_char = self.chars_iterator.next();

                Token::Plus
            }
            '-' => {
                self.previous_char = self.chars_iterator.next();

                Token::Minus
            }
            '*' => {
                self.previous_char = self.chars_iterator.next();

                Token::Asterisk
            }
            '/' => {
                self.previous_char = self.chars_iterator.next();

                Token::Slash
            }
            '<' => {
                self.previous_char = self.chars_iterator.next();

                Token::LessThan
            }
            '>' => {
                self.previous_char = self.chars_iterator.next();

                Token::GreaterThan
            }
            '=' => {
                // can be either Assign ("=") or Equal ("==")

                match self.chars_iterator.next() {
                    Some('=') => {
                        self.previous_char = self.chars_iterator.next();

                        Token::Equal
                    }
                    Some(other_char) => {
                        self.previous_char = Some(other_char);

                        Token::Assign
                    }
                    None => {
                        self.previous_char = None;

                        Token::Assign
                    }
                }
            }
            '!' => {
                // can be either Bang ("!") or NotEqual ("!=")

                match self.chars_iterator.next() {
                    Some('=') => {
                        self.previous_char = self.chars_iterator.next();

                        Token::NotEqual
                    }
                    Some(other_char) => {
                        self.previous_char = Some(other_char);

                        Token::Bang
                    }
                    None => {
                        self.previous_char = None;

                        Token::Bang
                    }
                }
            }
            illegal => {
                self.previous_char = self.chars_iterator.next();

                Token::Illegal(Literal(illegal.to_string()))
            }
        }
    }

    fn read_token(&mut self, first_char: char) -> Option<Token> {
        let mut current_char = first_char;

        if current_char.is_whitespace() {
            if let Some(next_char) = self.get_next_non_whitespace_char_or_eof() {
                current_char = next_char;
            } else {
                return None;
            }
        }

        if current_char.is_alphabetic() {
            Some(self.read_identifier_or_keyword(current_char))
        } else if current_char.is_ascii_digit() {
            Some(self.read_integer(current_char))
        } else {
            Some(self.read_special_symbol(current_char))
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current_char) = self.previous_char {
            self.read_token(current_char)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::{Literal, Token};

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
        ];

        let mut lexer = super::Lexer::new(input);

        for expected_token in expected_tokens {
            match lexer.next() {
                Some(token) => {
                    assert_eq!(token, expected_token);
                }
                None => {
                    panic!("Expected to get a token");
                }
            }
        }
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
                Some(token) => {
                    assert_eq!(token, expected_token);
                }
                None => {
                    panic!("Expected to get a token");
                }
            }
        }
    }
}
