use std::fs::{read_to_string, File};

use indexmap::{map::Entry, IndexMap};
use poke_instrument::profile_scope;
use thiserror::Error;

pub struct JsonNumber(pub f64);

pub struct JsonObject(pub IndexMap<String, JsonValue>);

pub struct JsonList(pub Vec<JsonValue>);

pub enum JsonValue {
    Number(JsonNumber),
    Object(JsonObject),
    List(JsonList),
}

#[derive(Error, Debug)]
pub enum JsonError {
    #[error("unexpectedly not the end: {0}")]
    UnexpectedlyNotTheEnd(String),
    #[error("unexpected end")]
    UnexpectedEnd,
    #[error("unexpected char '{0}'")]
    UnexpectedChar(char),
    #[error("duplicate key \"{0}\"")]
    DuplicateKey(String),
    #[error("not a number: {0}")]
    NotANumber(String),
}

fn peek_next_char(json_data: &str) -> Result<char, JsonError> {
    json_data.chars().next().ok_or(JsonError::UnexpectedEnd)
}

fn extract_next_char(json_data: &mut &str) -> Result<char, JsonError> {
    let result = peek_next_char(json_data)?;
    *json_data = &json_data[1..];
    Ok(result)
}

fn expect_char(json_data: &mut &str, expected: char) -> Result<(), JsonError> {
    let actual = extract_next_char(json_data)?;

    if actual != expected {
        return Err(JsonError::UnexpectedChar(actual));
    }

    Ok(())
}

fn parse_to_json_list(json_data: &mut &str) -> Result<JsonList, JsonError> {
    expect_char(json_data, '[')?;

    let mut result = Vec::new();

    loop {
        let value = parse_to_json(json_data)?;
        result.push(value);

        match extract_next_char(json_data)? {
            ',' => {}
            ']' => return Ok(JsonList(result)),
            unexpected => return Err(JsonError::UnexpectedChar(unexpected)),
        }
    }
}

fn parse_to_json_object(json_data: &mut &str) -> Result<JsonObject, JsonError> {
    expect_char(json_data, '{')?;

    let mut result = IndexMap::new();

    loop {
        expect_char(json_data, '"')?;
        let Some(end_id) = json_data.find('"') else {
            return Err(JsonError::UnexpectedEnd);
        };

        let key = &json_data[..end_id];
        *json_data = &json_data[end_id + 1..];
        expect_char(json_data, ':')?;
        let value = parse_to_json(json_data)?;

        match result.entry(key.into()) {
            Entry::Occupied(_) => return Err(JsonError::DuplicateKey(key.into())),
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(value);
            }
        }

        match extract_next_char(json_data)? {
            '}' => return Ok(JsonObject(result)),
            ',' => {}
            unexpected => return Err(JsonError::UnexpectedChar(unexpected)),
        }
    }
}

fn parse_to_json(json_data: &mut &str) -> Result<JsonValue, JsonError> {
    let parsed = match peek_next_char(json_data)? {
        '{' => JsonValue::Object(parse_to_json_object(json_data)?),
        '[' => JsonValue::List(parse_to_json_list(json_data)?),

        _ => JsonValue::Number(parse_to_json_number(json_data)?),
    };

    Ok(parsed)
}

fn parse_to_json_root(json_data: &mut &str) -> Result<JsonValue, JsonError> {
    let result = parse_to_json(json_data)?;

    if !json_data.is_empty() {
        return Err(JsonError::UnexpectedlyNotTheEnd((*json_data).into()));
    }

    Ok(result)
}

fn parse_to_json_number(json_data: &mut &str) -> Result<JsonNumber, JsonError> {
    let delim = json_data.find(['}', ']', ',']).unwrap_or(json_data.len());

    let number_string = &json_data[..delim];
    *json_data = &json_data[delim..];

    let Ok(result) = number_string.parse() else {
        return Err(JsonError::NotANumber(number_string.into()));
    };

    Ok(JsonNumber(result))
}

pub fn load_json_file(path: &str) -> anyhow::Result<JsonValue> {
    let file = File::open(path)?;
    let file_size = file.metadata().unwrap().len();

    let json_data = {
        profile_scope!("read_ro_string", file_size);
        read_to_string(path)
    }?;

    assert_eq!(file_size as usize, json_data.len());

    Ok({
        profile_scope!("parse json", file_size);
        parse_to_json_root(&mut json_data.as_str())
    }?)
}
