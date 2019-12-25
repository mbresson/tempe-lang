use std::io::{BufRead, Write};
use std::process;

use crate::interpreter::eval_program;
use crate::lexer::Lexer;
use crate::parser::Parser;

const PROMPT: &[u8] = b">> ";

pub fn start<R: BufRead, W: Write>(reader: &mut R, writer: &mut W) {
    let mut lines = reader.lines();

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
                    writeln!(writer, "OOOPS! Parsing errors encountered:").unwrap();

                    for error in errors {
                        writeln!(writer, "{}", error).unwrap();
                    }

                    continue 'repl;
                }
            };

            match eval_program(&program) {
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
