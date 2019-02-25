use std::io::{BufRead, Write};
use std::process;

const PROMPT: &[u8] = b">> ";

pub fn start(reader: &mut BufRead, writer: &mut Write) {
    let mut lines = reader.lines();

    loop {
        writer.write_all(PROMPT).unwrap();
        writer.flush().unwrap();

        if let Some(line_result) = lines.next() {
            match line_result {
                Ok(line) => {
                    let lexer = crate::lexer::Lexer::new(&line);

                    for token in lexer {
                        write!(writer, "{:?}\n", token).unwrap();
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
