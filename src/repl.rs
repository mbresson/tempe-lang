use std::io::{BufRead, Write};
use std::process;

use crate::interpreter::{eval_program, object::Environment};
use crate::lexer::Lexer;
use crate::parser::{
    errors::{Error as ParserError, ErrorKind as ParserErrorKind},
    Parser,
};

const PROMPT: &[u8] = b">> ";

pub fn start<R: BufRead, W: Write>(reader: &mut R, writer: &mut W) {
    let mut lines = reader.lines();

    let mut shared_environment = Environment::new();

    'repl: loop {
        writer.write_all(PROMPT).unwrap();
        writer.flush().unwrap();

        if let Some(line_result) = lines.next() {
            let line = line_result.unwrap_or_else(|err| {
                eprintln!("{}", err);
                process::abort();
            });

            let mut lexer = Lexer::new(&line);

            let program = match Parser::new(&mut lexer).parse_program() {
                Ok(program) => program,
                Err(errors) => {
                    for error in &errors {
                        write_parser_error(writer, error);
                    }

                    continue 'repl;
                }
            };

            match eval_program(&program, &mut shared_environment) {
                Ok(evaluated) => {
                    writeln!(writer, "{}", evaluated).unwrap();
                }
                Err(error) => {
                    writeln!(writer, "OOOPS! Evaluation error encountered: {}", error).unwrap();
                }
            }
        } else {
            return;
        }
    }
}

fn write_parser_error<W: Write>(writer: &mut W, error: &ParserError) {
    let ParserError(error_kind, _) = error;

    let blamed_column = match error_kind {
        ParserErrorKind::CannotParseTokenAsPrefix(token_with_context)
        | ParserErrorKind::ExpectedSpecificToken(_, token_with_context)
        | ParserErrorKind::IllegalToken(token_with_context) => {
            token_with_context.context.start_column
        }
        _ => error.context().end_column,
    };

    writeln!(
        writer,
        "{}{}^",
        " ".repeat(PROMPT.len()),
        ".".repeat(blamed_column)
    )
    .unwrap();

    writeln!(writer, "{} {}", "!".repeat(PROMPT.len() - 1), error).unwrap();
}
