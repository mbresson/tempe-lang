use std::io::{BufRead, Write};
use std::process;

const PROMPT: &[u8] = b">> ";

pub fn start<R: BufRead, W: Write>(reader: &mut R, writer: &mut W) {
    let mut lines = reader.lines();

    loop {
        writer.write_all(PROMPT).unwrap();
        writer.flush().unwrap();

        if let Some(line_result) = lines.next() {
            match line_result {
                Ok(line) => {
                    let lexer = crate::lexer::Lexer::new(&line);

                    for token in lexer {
                        writeln!(writer, "{:?}", token).unwrap();
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
