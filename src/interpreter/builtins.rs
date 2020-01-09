use super::errors::{ErrorKind, Result as InterpretingResult};
use super::object::Object;
use crate::representations::ast::Identifier;
use crate::representations::token::Literal;
use std::collections::HashMap;
use std::convert::TryInto;

use unicode_segmentation::UnicodeSegmentation;

pub type BuiltinFn = fn(Vec<Object>) -> InterpretingResult<Object>;

const NAME_FN_LEN: &str = "panjang";
const NAME_FN_FIRST: &str = "pertama";
const NAME_FN_LAST: &str = "terakhir";
const NAME_FN_REST: &str = "sisa";
const NAME_FN_PUSH: &str = "tambah";

fn assert_has_n_arguments(arguments: &[Object], n: usize, fn_name: &str) -> InterpretingResult<()> {
    if arguments.len() != n {
        Err(ErrorKind::WrongNumberOfArguments(
            Some(Identifier::new(Literal(fn_name.to_string()))),
            1,
            arguments.len(),
        )
        .into())
    } else {
        Ok(())
    }
}

fn new_len() -> Box<BuiltinFn> {
    Box::new(|arguments| {
        assert_has_n_arguments(&arguments, 1, NAME_FN_LEN)?;

        match &arguments[0] {
            Object::Str(string) => {
                let graphemes_count = string.graphemes(true).count();

                Ok(Object::Integer(graphemes_count.try_into().unwrap()))
            }
            Object::Array(items) => Ok(Object::Integer(items.len().try_into().unwrap())),
            object => Err(ErrorKind::WrongArgumentType(
                Some(Identifier::new(Literal(NAME_FN_LEN.to_string()))),
                object.clone(),
                vec!["string", "array"],
            )
            .into()),
        }
    })
}

fn new_first() -> Box<BuiltinFn> {
    Box::new(|arguments| {
        assert_has_n_arguments(&arguments, 1, NAME_FN_FIRST)?;

        match &arguments[0] {
            Object::Array(items) => {
                if items.is_empty() {
                    Ok(Object::Null)
                } else {
                    Ok(items[0].clone())
                }
            }
            object => Err(ErrorKind::WrongArgumentType(
                Some(Identifier::new(Literal(NAME_FN_FIRST.to_string()))),
                object.clone(),
                vec!["array"],
            )
            .into()),
        }
    })
}

fn new_last() -> Box<BuiltinFn> {
    Box::new(|arguments| {
        assert_has_n_arguments(&arguments, 1, NAME_FN_LAST)?;

        match &arguments[0] {
            Object::Array(items) => {
                if items.is_empty() {
                    Ok(Object::Null)
                } else {
                    Ok(items[items.len() - 1].clone())
                }
            }
            object => Err(ErrorKind::WrongArgumentType(
                Some(Identifier::new(Literal(NAME_FN_LAST.to_string()))),
                object.clone(),
                vec!["array"],
            )
            .into()),
        }
    })
}

fn new_rest() -> Box<BuiltinFn> {
    Box::new(|arguments| {
        assert_has_n_arguments(&arguments, 1, NAME_FN_REST)?;

        match &arguments[0] {
            Object::Array(items) => {
                if items.is_empty() {
                    Ok(Object::Null)
                } else {
                    let (_, rest) = items.split_at(1);

                    Ok(Object::Array(rest.to_vec()))
                }
            }
            object => Err(ErrorKind::WrongArgumentType(
                Some(Identifier::new(Literal(NAME_FN_REST.to_string()))),
                object.clone(),
                vec!["array"],
            )
            .into()),
        }
    })
}

fn new_push() -> Box<BuiltinFn> {
    Box::new(|arguments| {
        assert_has_n_arguments(&arguments, 2, NAME_FN_PUSH)?;

        match (&arguments[0], &arguments[1]) {
            (Object::Array(items), item_to_push) => {
                let mut array = items.clone();

                array.push(item_to_push.clone());

                Ok(Object::Array(array))
            }
            (object, _) => Err(ErrorKind::WrongArgumentType(
                Some(Identifier::new(Literal(NAME_FN_PUSH.to_string()))),
                object.clone(),
                vec!["array"],
            )
            .into()),
        }
    })
}

pub fn builtins_map() -> HashMap<Identifier, (Vec<Identifier>, Box<BuiltinFn>)> {
    let mut builtins = HashMap::new();

    builtins.insert(
        Identifier::new(Literal(NAME_FN_LEN.to_string())),
        (vec![Identifier::new(Literal("a".to_string()))], new_len()),
    );

    builtins.insert(
        Identifier::new(Literal(NAME_FN_FIRST.to_string())),
        (vec![Identifier::new(Literal("a".to_string()))], new_first()),
    );

    builtins.insert(
        Identifier::new(Literal(NAME_FN_LAST.to_string())),
        (vec![Identifier::new(Literal("a".to_string()))], new_last()),
    );

    builtins.insert(
        Identifier::new(Literal(NAME_FN_REST.to_string())),
        (vec![Identifier::new(Literal("a".to_string()))], new_rest()),
    );

    builtins.insert(
        Identifier::new(Literal(NAME_FN_PUSH.to_string())),
        (
            vec![Identifier::new(Literal("a, b".to_string()))],
            new_push(),
        ),
    );

    builtins
}
