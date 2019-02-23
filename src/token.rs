#[cfg(test)]
mod tests {
    use super::{Literal, Token};

    #[test]
    fn from_str() {
        let str_and_expected_tokens = vec![
            ("=", Token::Assign),
            ("+", Token::Plus),
            (" +   \n", Token::Plus), // white spaces should be tolerated
            (",", Token::Comma),
            (";", Token::Semicolon),
            ("(", Token::OpeningParenthesis),
            (")", Token::ClosingParenthesis),
            ("{", Token::OpeningBrace),
            ("}", Token::ClosingBrace),
            ("fungsi", Token::Function),
            ("diketahui", Token::Let),
            ("42829", Token::Integer(42829)),
            ("848.28", Token::Illegal(Literal("848.28".to_string()))), // our language is basic and only supports integers (no floats)
            (
                "awesome_variable_42",
                Token::Identifier(Literal("awesome_variable_42".to_string())),
            ),
            (
                "   awesome_variable_42",
                Token::Identifier(Literal("awesome_variable_42".to_string())),
            ), // white spaces shouldn't break tokenization
            (
                "1awesome_variable",
                Token::Illegal(Literal("1awesome_variable".to_string())),
            ), // identifiers cannot start with a digit
            (
                "_awesome_variable",
                Token::Illegal(Literal("_awesome_variable".to_string())),
            ), // actually, they cannot start with any non-alphabetic character
            (
                "สนุกไหม",
                Token::Identifier(Literal("สนุกไหม".to_string())),
            ), // it's 21st century, we can support any Unicode letter in our identifiers
        ];

        for (s, expected_token) in str_and_expected_tokens {
            assert_eq!(Token::from(s), expected_token);
        }
    }
}

// "fungsi" is a direct translation of 'function' in Bahasa Indonesia
const LITERAL_FUNCTION: &str = "fungsi";

// "diketahui" means something like "It is known that..." in Bahasa Indonesia
const LITERAL_LET: &str = "diketahui";

#[derive(Debug, Eq, PartialEq)]
pub struct Literal(pub String);

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Illegal(Literal),

    Identifier(Literal),
    Integer(i64),

    Assign,
    Plus,

    Comma,
    Semicolon,

    OpeningParenthesis,
    ClosingParenthesis,
    OpeningBrace,
    ClosingBrace,

    Function,
    Let,
}

impl From<&str> for Token {
    fn from(s: &str) -> Self {
        use self::{Literal, Token};

        if s.is_empty() {
            return Token::Illegal(Literal(String::new()));
        }

        match s.trim() {
            "=" => Token::Assign,
            "+" => Token::Plus,

            "," => Token::Comma,
            ";" => Token::Semicolon,

            "(" => Token::OpeningParenthesis,
            ")" => Token::ClosingParenthesis,
            "{" => Token::OpeningBrace,
            "}" => Token::ClosingBrace,

            LITERAL_FUNCTION => Token::Function,
            LITERAL_LET => Token::Let,
            trimmed_s => {
                if let Ok(integer) = trimmed_s.parse::<i64>() {
                    // since this is only a toy lexer, our implementation doesn't support integers bigger than i64 can hold
                    // it means that if the source code contains a bigger number, it will fail with Illegal(identifier) and no further explanation
                    return Token::Integer(integer);
                }

                let mut chars = trimmed_s.chars();
                let first_char = chars.next().unwrap(); // at this stage the string is guaranteed to contain at least one char

                if !first_char.is_alphabetic() {
                    return Token::Illegal(Literal(trimmed_s.to_string()));
                }

                Token::Identifier(Literal(trimmed_s.to_string()))
            }
        }
    }
}
