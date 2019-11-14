mod ast;
mod lexer;
mod parser;
mod repl;
mod token;

use std::io::{stdin, stdout};

fn main() {
    let current_username = users::get_current_username().unwrap();

    println!(
        "Hello {}! This is the Tempe programming language! Selamat makan!",
        current_username.to_string_lossy()
    );

    println!("Feel free to type in commands");

    // TMP inserted here only to silence warnings of unused Parser and Parser methods,
    // as the parser is not ready yet to be used
    let _ = crate::parser::Parser::new(
        crate::lexer::Lexer::new("blabla")
    ).parse_program();

    repl::start(
        &mut stdin().lock(),
        &mut std::io::LineWriter::new(stdout().lock()),
    );
}
