use super::errors::{ErrorKind, Result as InterpretingResult};
use super::object::Object;
use crate::representations::ast::Identifier;
use crate::representations::token::Literal;
use std::collections::HashMap;
use std::convert::TryInto;

use unicode_segmentation::UnicodeSegmentation;

pub type BuiltinFn = fn(Vec<Object>) -> InterpretingResult<Object>;

const LEN_NAME: &str = "panjang";

fn new_len() -> Box<BuiltinFn> {
    Box::new(|arguments| {
        if arguments.len() != 1 {
            return Err(ErrorKind::WrongNumberOfArguments(
                Some(Identifier::new(Literal(LEN_NAME.to_string()))),
                1,
                arguments.len(),
            )
            .into());
        }

        let string_argument = match &arguments[0] {
            Object::Str(string) => string,
            object => {
                return Err(ErrorKind::WrongArgumentType(
                    Some(Identifier::new(Literal(LEN_NAME.to_string()))),
                    object.clone(),
                    "string".to_string(),
                )
                .into());
            }
        };

        let graphemes_count = string_argument.graphemes(true).count();

        Ok(Object::Integer(graphemes_count.try_into().unwrap()))
    })
}

pub fn builtins_map() -> HashMap<Identifier, (Vec<Identifier>, Box<BuiltinFn>)> {
    let mut builtins = HashMap::new();

    builtins.insert(
        Identifier::new(Literal(LEN_NAME.to_string())),
        (
            vec![Identifier::new(Literal("string".to_string()))],
            new_len(),
        ),
    );
    builtins
}
