
use std::fmt;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Literal(pub String);

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token {
    Illegal(Literal),

    Identifier(Literal),
    Integer(i64),

    // keywords
    Function,
    Let,
    True,
    False,
    If,
    Not, // not to be confused with Bang ("!"), see keywords module for why this keyword exists
    Return,

    // operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LessThan,
    GreaterThan,
    Equal,
    NotEqual,

    // other symbols
    Comma,
    Semicolon,

    OpeningParenthesis,
    ClosingParenthesis,
    OpeningBrace,
    ClosingBrace,
}

pub mod keywords {

    // "fungsi" is a direct translation of 'function' in Bahasa Indonesia
    pub const FUNCTION: &str = "fungsi";

    // "diketahui" means something like "It is known that..." in Bahasa Indonesia
    pub const LET: &str = "diketahui";

    pub const TRUE: &str = "benar";

    pub const FALSE: &str = "salah";

    pub const IF: &str = "jika";

    // in Indonesian language, there is no one-word translation for the words "else" or "otherwise"
    // instead, the best translation for these words is "jika tidak", that is, two words separated by a space
    // as a result, we must introduce the keyword "tidak", which will be only used for this purpose
    pub const NOT: &str = "tidak";

    pub const RETURN: &str = "kembalikan";
}
