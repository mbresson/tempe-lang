use std::io::{BufRead, Write};
use std::process;

use crate::ast::Program;
use crate::interpreter::eval_statements;
use crate::lexer::Lexer;
use crate::parser::Parser;

const PROMPT: &[u8] = b">> ";

pub fn start<R: BufRead, W: Write>(reader: &mut R, writer: &mut W) {
    let mut lines = reader.lines();

    loop {
        writer.write_all(PROMPT).unwrap();
        writer.flush().unwrap();

        if let Some(line_result) = lines.next() {
            match line_result {
                Ok(line) => {
                    let mut lexer = Lexer::new(&line);

                    let mut parser = Parser::new(&mut lexer);

                    match parser.parse_program() {
                        Ok(Program { statements, .. }) => {
                            let evaluated = eval_statements(&statements);
                            writeln!(writer, "{}", evaluated).unwrap();
                        }
                        Err(errors) => {
                            writeln!(writer, "OOOPS! Parsing errors encountered:").unwrap();

                            for error in errors {
                                writeln!(writer, "{}", error).unwrap();
                            }
                        }
                    }
                }
                Err(err) => {
                    eprintln!("{}", err);
                    process::abort();
                }
            }
        } else {
            return;
        }
    }
}
