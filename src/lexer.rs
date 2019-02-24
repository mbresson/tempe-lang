use crate::token::Token;

#[cfg(test)]
mod tests {
    use crate::token::{Literal, Token};

    #[test]
    fn next_token_from_code_snippet() {
        let input = "
        
            diketahui five = 5;
            diketahui ten = 10;
            diketahui add = fungsi(x, y) {
                x + y;
            };

            diketahui the_result = add(five, ten);

        ";

        let expected_tokens = vec![
            Token::Let,
            Token::Identifier(Literal("five".to_string())),
            Token::Assign,
            Token::Integer(5),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(Literal("ten".to_string())),
            Token::Assign,
            Token::Integer(10),
            Token::Semicolon,
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
            Token::Identifier(Literal("x".to_string())),
            Token::Plus,
            Token::Identifier(Literal("y".to_string())),
            Token::Semicolon,
            Token::ClosingBrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(Literal("the_result".to_string())),
            Token::Assign,
            Token::Identifier(Literal("add".to_string())),
            Token::OpeningParenthesis,
            Token::Identifier(Literal("five".to_string())),
            Token::Comma,
            Token::Identifier(Literal("ten".to_string())),
            Token::ClosingParenthesis,
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

// a Lexer iterates over a text and returns tokens
// it supports Unicode in identifiers
pub struct Lexer<'a> {
    chars_iterator: std::str::Chars<'a>,
    previous_non_whitespace_char: Option<char>,
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
        Lexer {
            chars_iterator: text.chars(),
            previous_non_whitespace_char: None,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut token_buffer = if let Some(ch) = self.previous_non_whitespace_char {
            ch.to_string()
        } else {
            String::new()
        };

        'lexing: while let Some(ch) = self.chars_iterator.next() {
            // we must guess if the current character signals a new token
            // in 'a bcd', the space signals a new token (starting with 'b')
            // in 'ab_42cd(', '(' signals a new token
            // in '(foobar' or '(42', '(' signals a new token
            // in '((', there is no new token (but it will be an illegal token)

            if ch.is_whitespace() {
                if token_buffer.is_empty() {
                    continue 'lexing;
                }

                self.previous_non_whitespace_char = None;

                return Some(Token::from(token_buffer.as_str()));
            }

            let is_new_token_delimiter = match self.previous_non_whitespace_char {
                None => false,
                Some(previous_ch) => {
                    let ch_is_literal = ch.is_alphanumeric() || ch == '_';
                    let previous_ch_is_literal =
                        previous_ch.is_alphanumeric() || previous_ch == '_';

                    !(ch_is_literal && previous_ch_is_literal)
                }
            };

            self.previous_non_whitespace_char = Some(ch);

            if is_new_token_delimiter && !token_buffer.is_empty() {
                return Some(Token::from(token_buffer.as_ref()));
            }

            token_buffer.push(ch);
        }

        if token_buffer.is_empty() {
            None
        } else {
            Some(Token::from(token_buffer.as_str()))
        }
    }
}
