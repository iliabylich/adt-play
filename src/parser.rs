use regex::Regex;
use std::rc::Rc;
use std::cell::RefCell;
use std::str;

use crate::JSON;
use crate::Buffer;
use crate::ParseError;

type JsonObjectMember = (String, JSON);

#[derive(Debug, Clone)]
struct ParseResult<T> {
    pub value: Result<Option<T>, String>,
    buffer: Rc<RefCell<Buffer>>,
    length: usize
}

trait MergeableJsonPart {
    fn merge(&self, rhs: &Self) -> Self;
}

impl MergeableJsonPart for String {
    fn merge(&self, rhs: &Self) -> Self {
        format!("{}{}", self, rhs)
    }
}

impl<T> MergeableJsonPart for Vec<T> where T: Clone {
    fn merge(&self, rhs: &Self) -> Self {
        [&self[..], rhs].concat()
    }
}

impl<T> ParseResult<T> {
    fn rollback(&self) {
        self.buffer.borrow_mut().rollback(self.length)
    }

    pub fn and(&self, fnext: &dyn Fn() -> Self) -> Self where T: MergeableJsonPart + Clone {
        match &self.value {
            Err(err) => {
                ParseResult { value: Err(err.clone()), length: self.length, buffer: self.buffer.clone() }
            },
            Ok(prev) => {
                let next = fnext();
                let next_length = next.length;
                match &next.value {
                    Err(..) => {
                        self.rollback();
                        next
                    },
                    Ok(next) => {
                        let prev_length = self.length;
                        let buffer = self.buffer.clone();
                        match (prev, next) {
                            (None,           None)           => Self { value: Ok(None),                   length: 0,                         buffer },
                            (None,           Some(..))       => Self { value: Ok(next.clone()),           length: next_length,               buffer },
                            (Some(..),       None)           => Self { value: Ok(prev.clone()),           length: prev_length,               buffer },
                            (Some(prev), Some(next)) => Self { value: Ok(Some(prev.merge(next))), length: prev_length + next_length, buffer }
                        }
                    }
                }
            }
        }
    }

    pub fn and_compose<TNext, TComposed>(
        &self,
        fnext: &dyn Fn() -> ParseResult<TNext>,
        compose: &dyn Fn(&Option<T>, &Option<TNext>) -> Option<TComposed>
    ) -> ParseResult<TComposed>
    where
        T: Sized + Clone
    {
        let buffer = self.buffer.clone();
        match &self.value {
            Err(err) => {
                ParseResult { value: Err(err.clone()), length: self.length, buffer }
            },
            Ok(prev) => {
                let next = fnext();
                let next_length = next.length;
                match &next.value {
                    Err(err) => {
                        self.rollback();
                        ParseResult { value: Err(err.clone()), length: next_length, buffer }
                    },
                    Ok(next) => {
                        let prev_length = self.length;
                        let composed = compose(prev, next);
                        ParseResult { value: Ok(composed), length: prev_length + next_length, buffer }
                    }
                }
            }
        }
    }

    pub fn or(self, f: &dyn Fn() -> ParseResult<T>) -> Self {
        match self.value {
            Err(..) => {
                self.rollback();
                f()
            },
            Ok(_) => self
        }
    }

    pub fn map<TNext>(&self, f: &dyn Fn(&T) -> ParseResult<TNext>) -> ParseResult<TNext> {
        match &self.value {
            Err(err) => ParseResult { value: Err(err.clone()), buffer: self.buffer.clone(), length: self.length },
            Ok(None) => ParseResult { value: Ok(None), buffer: self.buffer.clone(), length: self.length },
            Ok(Some(value)) => f(&value)
        }
    }

    pub fn map_value<TNext>(&self, f: &dyn Fn(&T) -> Option<TNext>) -> ParseResult<TNext> {
        self.map(&|value|
            ParseResult { value: Ok(f(value)), length: self.length, buffer: self.buffer.clone() }
        )
    }

    pub fn map_to_value<TNext>(&self, value: TNext) -> ParseResult<TNext> where TNext: Clone {
        self.map_value(&|_json| Some(value.clone()))
    }

    pub fn ignore_value<TNext>(&self) -> ParseResult<TNext> {
        self.map_value(&|_json| None)
    }
}

pub struct Parser {
    pub buffer: Rc<RefCell<Buffer>>
}

impl<'a> Parser {
    pub fn new(s: &str) -> Self {
        Self { buffer: Rc::new(RefCell::new(Buffer::from(s))) }
    }

    pub fn parse(&self) -> Result<JSON, ParseError> {
        let result = self.try_json();
        match result.value {
            Ok(Some(json)) => Ok(json),
            Ok(None) => Err(ParseError { message: "Internal error".into(), range: 0..0 }),
            Err(err) => {
                let range_end = self.buffer.borrow().pos;
                let range_start = range_end - result.length;
                let range = range_start..range_end;
                Err(ParseError { message: err, range })
            }
        }
    }

    fn try_json(&self) -> ParseResult<JSON> {
        self.try_element()
    }

    fn try_value(&self) -> ParseResult<JSON> {
        self.try_object()
            .or(&|| self.try_array() )
            .or(&|| self.try_string().map_value(&|value| Some(JSON::String(value.clone())) ) )
            .or(&|| self.try_number() )
            .or(&|| self.try_raw_string("true").map_to_value(JSON::True) )
            .or(&|| self.try_raw_string("false").map_to_value(JSON::False) )
            .or(&|| self.try_raw_string("null").map_to_value(JSON::Null) )
    }

    fn try_object(&self) -> ParseResult<JSON> {
        self.try_raw_string("{").ignore_value()
            .and(&|| self.try_members().or(&|| self.skip_ws() ) )
            .and(&|| self.try_raw_string("}").ignore_value() )
            .map_value(&|value| {
                Some(JSON::Object(value.clone()))
            })
    }

    fn try_members(&self) -> ParseResult<Vec<JsonObjectMember>> {
        self.try_member().map_value(&|value| Some(vec![value.clone()]) )
            .and(&||
                (
                self.try_raw_string(",").ignore_value()
                    .and(&|| self.try_members())
                )
                .or(&|| ParseResult { value: Ok(Some(vec![])), buffer: self.buffer.clone(), length: 0 } )
            )
    }

    fn try_member(&self) -> ParseResult<JsonObjectMember> {
        self.skip_ws::<()>()
            .and_compose::<String, String>(&|| self.try_string(), &|_prev, next| next.clone() )
            .and_compose::<(), String>(&|| self.skip_ws::<()>(), &|prev, _next| prev.clone() )
            .and_compose::<(), String>(&|| self.try_raw_string(":").ignore_value::<()>(), &|prev, _next| prev.clone() )
            .and_compose::<(), String>(&|| self.skip_ws::<()>(), &|prev, _next| prev.clone() )
            .and_compose::<JSON, JsonObjectMember>(&|| self.try_element(), &|prev, next| Some((prev.clone().unwrap(), next.clone().unwrap())) )
    }

    fn try_array(&self) -> ParseResult<JSON> {
        self.skip_string::<Vec<JSON>>("[")
            .and(&|| self.try_array_items().or(&|| self.skip_ws() ) )
            .and(&|| self.try_raw_string("]").ignore_value() )
            .map_value(&|items| Some(JSON::Array(items.clone())))
    }

    fn try_array_items(&self) -> ParseResult<Vec<JSON>> {
        self.try_element()
            .map_value(&|element| Some(vec![element.clone()]) )
            .and(&||
                self.try_raw_string(",").ignore_value()
                    .and(&|| self.try_array_items() )
                    .or(&|| ParseResult { value: Ok(Some(vec![])), buffer: self.buffer.clone(), length: 0 } )
            )
    }

    fn try_element(&self) -> ParseResult<JSON> {
        self.skip_ws::<()>()
            .and_compose(&|| self.try_value(), &|_lhs, rhs| rhs.clone() )
            .and_compose(&|| self.skip_ws::<()>(), &|lhs, _rhs| lhs.clone() )
    }

    fn try_string(&self) -> ParseResult<String> {
        self.try_raw_string("\"").map_to_value(String::from(""))
            .and(&|| self.try_characters() )
            .and(&|| self.try_raw_string("\"").map_to_value(String::from("")) )
    }

    fn try_characters(&self) -> ParseResult<String> {
        self.try_character()
            .and(&|| self.try_characters() )
            .or(&||
                self.build_raw_str("", 0)
            )
    }

    fn try_character(&self) -> ParseResult<String> {
        self.try_raw_regex(Regex::new(r#"[^"\\]+"#).unwrap())
            .or(&||
                self.try_raw_string("\\")
                    .map_to_value::<String>("".into())
                    .and(&|| self.try_escape())
            )
    }

    fn try_escape(&self) -> ParseResult<String> {
        self.try_raw_string("\"").map_to_value::<String>("\"".into())
            .or(&|| self.try_raw_string("\\)").map_to_value::<String>("\\".into()) )
            .or(&|| self.try_raw_string("/").map_to_value::<String>("/".into()) )
            .or(&|| self.try_raw_string("b").map_to_value::<String>(r"\b".into()) )
            .or(&|| self.try_raw_string("f").map_to_value::<String>(r"\f".into()) )
            .or(&|| self.try_raw_string("n").map_to_value::<String>("\n".into()) )
            .or(&|| self.try_raw_string("r").map_to_value::<String>("\r".into()) )
            .or(&|| self.try_raw_string("t").map_to_value::<String>("\t".into()) )
            .or(&||
                self.try_raw_string("u")
                    .map_to_value::<String>("".into())
                    .and(&|| self.try_hex() )
                    .and(&|| self.try_hex() )
                    .and(&|| self.try_hex() )
                    .and(&|| self.try_hex() )
                    .map_value(&|value| {
                        let codepoint = u32::from_str_radix(&value, 16).unwrap();
                        let s = std::char::from_u32(codepoint).unwrap().to_string();
                        Some(s)
                    })
            )
    }

    fn try_hex(&self) -> ParseResult<String> {
        self.try_raw_regex(Regex::new("[0-9A-Fa-f]").unwrap())
    }

    fn try_number(&self) -> ParseResult<JSON> {
        self.try_integer()
            .and(&|| self.try_fraction() )
            .and(&|| self.try_exponent() )
            .map_value(&|value| Some(JSON::Number(value.clone())) )
    }

    fn try_integer(&self) -> ParseResult<String> {
        self.try_raw_string("-")
            .or(&|| self.build_raw_str("", 0) )
            .and(&||
                (self.try_onenine().and(&|| self.try_digits() ))
                .or(&|| self.try_digit() )
            )
    }

    fn try_digits(&self) -> ParseResult<String> {
        self.try_digit().and(&|| self.try_digits().or(&|| self.build_raw_str("", 0) ) )
    }

    fn try_digit(&self) -> ParseResult<String> {
        self.try_raw_string("0").or(&|| self.try_onenine())
    }

    fn try_onenine(&self) -> ParseResult<String> {
        self.try_raw_regex(Regex::new("[1-9]").unwrap())
    }

    fn try_fraction(&self) -> ParseResult<String> {
        (self.try_raw_string(".").and(&|| self.try_digits() ))
            .or(&|| self.empty() )
    }

    fn try_exponent(&self) -> ParseResult<String> {
        self.try_raw_regex(Regex::new("[eE]").unwrap())
            .and(&|| self.try_sign() )
            .and(&|| self.try_digits() )
            .or(&|| self.empty() )
    }

    fn try_sign(&self) -> ParseResult<String> {
        self.try_raw_string("-")
            .or(&|| self.try_raw_string("+") )
            .or(&|| self.empty() )
    }

    fn skip_ws<T>(&self) -> ParseResult<T> {
        self.try_raw_regex(Regex::new(r"\s*").unwrap()).ignore_value::<T>()
    }

    fn try_raw_string(&self, s: &str) -> ParseResult<String> {
        match self.buffer.try_borrow_mut().unwrap().scan_str(s) {
            Ok((matched, length)) => self.build_raw_str(&matched, length),
            Err(message) => self.build_err(message)
        }
    }

    fn skip_string<T>(&self, s: &str) -> ParseResult<T> {
        self.try_raw_string(s).ignore_value::<T>()
    }

    fn try_raw_regex(&self, re: Regex) -> ParseResult<String> {
        match self.buffer.try_borrow_mut().unwrap().scan_re(re) {
            Ok((matched, length)) => self.build_raw_str(&matched, length),
            Err(message) => self.build_err(message)
        }
    }

    fn build_err<T>(&self, err: String) -> ParseResult<T> {
        ParseResult { value: Err(err), length: 0, buffer: self.buffer.clone() }
    }

    fn build_raw_str(&self, s: &str, length: usize) -> ParseResult<String> {
        ParseResult { value: Ok(Some(s.into())), length, buffer: self.buffer.clone() }
    }

    fn empty<T>(&self) -> ParseResult<T> {
        ParseResult { value: Ok(None), length: 0, buffer: self.buffer.clone() }
    }
}
