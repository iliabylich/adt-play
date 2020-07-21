use regex::Regex;
use std::rc::Rc;
use std::cell::{RefCell};
use std::ops::Add;
use std::str;

use crate::JSON;
use crate::Buffer;
use crate::ParseError;

#[derive(Debug, Clone)]
pub enum ParsedValue {
    JSON(JSON),
    Err(String),

    Member(String, JSON),
    Members(Vec<(String, JSON)>),
    RawStr(String),
    RawStrings(Vec<String>),

    Ignored
}

impl Add for ParsedValue {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::JSON(lhs), Self::JSON(rhs)) => {
                Self::JSON(lhs + rhs)
            },
            (Self::Members(lhs), Self::Members(rhs)) => {
                Self::Members(lhs.into_iter().chain(rhs.into_iter()).collect())
            }
            (Self::RawStr(lhs), Self::RawStr(rhs)) => {
                Self::RawStr(format!("{}{}", lhs, rhs))
            },
            (Self::RawStrings(lhs), Self::RawStrings(rhs)) => {
                Self::RawStrings(lhs.into_iter().chain(rhs.into_iter()).collect())
            }

            (lhs, Self::Ignored) => lhs,
            (Self::Ignored, rhs) => rhs,

            (lhs, rhs) => {
                panic!("Internal error: can't concatenate {:?} and {:?}", lhs, rhs)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseResult {
    pub value: ParsedValue,
    buffer: Rc<RefCell<Buffer>>,
    length: usize
}

impl Add for ParseResult {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        ParseResult { value: self.value + rhs.value, length: self.length + rhs.length, buffer: self.buffer }
    }
}

impl ParseResult {
    fn rollback(&self) {
        self.buffer.borrow_mut().rollback(self.length)
    }

    pub fn and(&self, f: &dyn Fn() -> ParseResult) -> Self {
        match &self.value {
            ParsedValue::Err(_) => {
                self.clone()
            },

            _ => {
                let rhs = f();
                match &rhs.value {
                    ParsedValue::Err(..) => {
                        self.rollback();
                        rhs
                    },
                    _ => self.clone() + rhs
                }
            }
        }
    }

    pub fn or(&self, f: &dyn Fn() -> ParseResult) -> Self {
        match self.value {
            ParsedValue::Err(..) => {
                self.rollback();
                f()
            },
            _ => self.clone()
        }
    }

    pub fn map(&self, f: &dyn Fn(&ParsedValue, &usize) -> Self) -> Self {
        match self.value {
            ParsedValue::Err(_) => self.clone(),
            _ => f(&self.value, &self.length)
        }
    }

    pub fn map_value(&self, f: &dyn Fn(&ParsedValue) -> ParsedValue) -> Self {
        self.map(&|value, _length| Self { value: f(value), length: self.length, buffer: self.buffer.clone() })
    }

    pub fn map_to_value(&self, value: ParsedValue) -> Self {
        self.map_value(&|_json| value.clone())
    }

    pub fn map_to_raw_str(&self, s: &str) -> Self {
        self.map_to_value(ParsedValue::RawStr(String::from(s)))
    }

    pub fn ignore(&self) -> Self {
        self.map_to_value(ParsedValue::Ignored)
    }

    pub fn map_to_json_value(&self, value: JSON) -> Self {
        self.map_to_value(ParsedValue::JSON(value))
    }

    pub fn as_array(&self) -> Self {
        self.map_value(&|value| {
            match &value {
                ParsedValue::JSON(json) => ParsedValue::JSON(JSON::Array(vec![json.clone()])),
                ParsedValue::RawStr(s) => ParsedValue::RawStrings(vec![s.clone()]),
                ParsedValue::Member(key, value) => ParsedValue::Members(vec![(key.clone(), value.clone())]),
                ParsedValue::Err(_) => value.clone(),
                _ => panic!("internal error: can't wrap {:?} into array", value)
            }
        })
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
            ParsedValue::JSON(json) => Ok(json),
            ParsedValue::Err(err) => {
                let range_end = self.buffer.borrow().pos;
                let range_start = range_end - result.length;
                let range = range_start..range_end;
                Err(ParseError { message: err, range })
            },
            _ => Err(ParseError { message: String::from("internal error"), range: 0..self.buffer.borrow().len() })
        }
    }

    fn try_json(&self) -> ParseResult {
        self.try_element()
    }

    fn try_value(&self) -> ParseResult {
        self.try_object()
            .or(&|| self.try_array() )
            .or(&|| self.try_string() )
            .or(&|| self.try_number() )
            .or(&|| self.try_raw_string("true").map_to_json_value(JSON::True) )
            .or(&|| self.try_raw_string("false").map_to_json_value(JSON::False) )
            .or(&|| self.try_raw_string("null").map_to_json_value(JSON::Null) )
    }

    fn try_object(&self) -> ParseResult {
        self.try_raw_string("{").ignore()
            .and(&|| self.try_members().or(&|| self.try_ws() ) )
            .and(&|| self.try_raw_string("}").ignore() )
            .map_value(&|value| {
                match value {
                    ParsedValue::Members(members) => ParsedValue::JSON(JSON::Object(members.clone())),
                    _ => panic!("internal error: incorrect format of object members {:?}", value)
                }
            })
    }

    fn try_members(&self) -> ParseResult {
        self.try_member().as_array()
            .and(&||
                (
                self.try_raw_string(",").ignore()
                    .and(&|| self.try_members())
                ).or(&|| self.build_members(vec![], 0) )
            )
    }

    fn try_member(&self) -> ParseResult {
        self.try_ws()
            .and(&|| self.try_string().as_array() )
            .and(&|| self.try_ws() )
            .and(&|| self.try_raw_string(":").ignore() )
            .and(&|| self.try_ws() )
            .and(&|| self.try_element().as_array() )
            .map_value(&|value| {
                match value {
                    ParsedValue::JSON(JSON::Array(items)) if items.len() == 2 => {
                        let (key, value) = (items[0].clone(), items[1].clone());
                        if let JSON::String(key) = key {
                            ParsedValue::Member(key, value)
                        } else {
                            panic!("internal error: incorrect member {:#?}", value)
                        }
                    },
                    _ => panic!("internal error: incorrect member {:#?}", value)
                }
            })
    }

    fn try_array(&self) -> ParseResult {
        self.try_raw_string("[").ignore()
            .and(&|| self.try_elements().or(&|| self.try_ws() ) )
            .and(&|| self.try_raw_string("]").ignore() )
    }

    fn try_elements(&self) -> ParseResult {
        self.try_element()
            .as_array()
            .and(&||
                self.try_raw_string(",").ignore()
                    .and(&|| self.try_elements() )
                    .or(&|| self.build_json(JSON::Array(vec![]), 0) )
            )
    }

    fn try_element(&self) -> ParseResult {
        self.try_ws()
            .and(&|| self.try_value() )
            .and(&|| self.try_ws() )
    }

    fn try_string(&self) -> ParseResult {
        self.try_raw_string("\"").map_to_raw_str("")
            .and(&|| self.try_characters() )
            .and(&|| self.try_raw_string("\"").map_to_raw_str("") )
            .map_value(&|value| {
                match value {
                    ParsedValue::RawStr(s) => ParsedValue::JSON(JSON::String(s.clone())),
                    _ => panic!("internal error: wrong string value: {:?}", value)
                }
            })
    }

    fn try_characters(&self) -> ParseResult {
        self.try_character()
            .and(&|| self.try_characters() )
            .or(&||
                self.build_raw_str("", 0)
            )
    }

    fn try_character(&self) -> ParseResult {
        self.try_raw_regex(Regex::new(r#"[^"\\]+"#).unwrap())
            .or(&||
                self.try_raw_string("\\")
                    .map_to_raw_str("")
                    .and(&|| self.try_escape())
            )
    }

    fn try_escape(&self) -> ParseResult {
        self.try_raw_string("\"").map_to_raw_str("\"")
            .or(&|| self.try_raw_string("\\)").map_to_raw_str("\\") )
            .or(&|| self.try_raw_string("/").map_to_raw_str("/") )
            .or(&|| self.try_raw_string("b").map_to_raw_str(r"\b") )
            .or(&|| self.try_raw_string("f").map_to_raw_str(r"\f") )
            .or(&|| self.try_raw_string("n").map_to_raw_str("\n") )
            .or(&|| self.try_raw_string("r").map_to_raw_str("\r") )
            .or(&|| self.try_raw_string("t").map_to_raw_str("\t") )
            .or(&||
                self.try_raw_string("u")
                    .map_to_raw_str("")
                    .and(&|| self.try_hex() )
                    .and(&|| self.try_hex() )
                    .and(&|| self.try_hex() )
                    .and(&|| self.try_hex() )
                    .map_value(&|value| {
                        match value {
                            ParsedValue::RawStr(s) => {
                                let codepoint = u32::from_str_radix(s, 16).unwrap();
                                let s = std::char::from_u32(codepoint).unwrap().to_string();
                                ParsedValue::RawStr(s)
                            },
                            _ => panic!("internal error: wrong escape type {:#?}", value)
                        }
                    })
            )
    }

    fn try_hex(&self) -> ParseResult {
        self.try_raw_regex(Regex::new("[0-9A-Fa-f]").unwrap())
    }

    fn try_number(&self) -> ParseResult {
        self.try_integer()
            .and(&|| self.try_fraction() )
            .and(&|| self.try_exponent() )
            .map_value(&|value| {
                match value {
                    ParsedValue::RawStr(s) => ParsedValue::JSON(JSON::Number(s.clone())),
                    _ => panic!("internal error: wrong integer value {:?}", value)
                }
            })
    }

    fn try_integer(&self) -> ParseResult {
        self.try_raw_string("-")
            .or(&|| self.build_raw_str("", 0) )
            .and(&||
                (self.try_onenine().and(&|| self.try_digits() ))
                .or(&|| self.try_digit() )
            )
    }

    fn try_digits(&self) -> ParseResult {
        self.try_digit().and(&|| self.try_digits().or(&|| self.build_raw_str("", 0) ) )
    }

    fn try_digit(&self) -> ParseResult {
        self.try_raw_string("0").or(&|| self.try_onenine())
    }

    fn try_onenine(&self) -> ParseResult {
        self.try_raw_regex(Regex::new("[1-9]").unwrap())
    }

    fn try_fraction(&self) -> ParseResult {
        (self.try_raw_string(".").and(&|| self.try_digits() ))
            .or(&|| self.build_ignored() )
    }

    fn try_exponent(&self) -> ParseResult {
        self.try_raw_regex(Regex::new("[eE]").unwrap())
            .and(&|| self.try_sign() )
            .and(&|| self.try_digits() )
            .or(&|| self.build_ignored() )
    }

    fn try_sign(&self) -> ParseResult {
        self.try_raw_string("-")
            .or(&|| self.try_raw_string("+") )
            .or(&|| self.build_ignored() )
    }


    fn try_ws(&self) -> ParseResult {
        self.try_raw_regex(Regex::new(r"\s*").unwrap()).ignore()
    }

    fn try_raw_string(&self, s: &str) -> ParseResult {
        match self.buffer.try_borrow_mut().unwrap().scan_str(s) {
            Ok((matched, length)) => self.build_raw_str(&matched, length),
            Err(message) => self.build_err(message)
        }
    }

    fn try_raw_regex(&self, re: Regex) -> ParseResult {
        match self.buffer.try_borrow_mut().unwrap().scan_re(re) {
            Ok((matched, length)) => self.build_raw_str(&matched, length),
            Err(message) => self.build_err(message)
        }
    }

    fn build_json(&self, json: JSON, length: usize) -> ParseResult {
        ParseResult { value: ParsedValue::JSON(json), length, buffer: Rc::clone(&self.buffer) }
    }

    fn build_err(&self, err: String) -> ParseResult {
        ParseResult { value: ParsedValue::Err(err), length: 0, buffer: Rc::clone(&self.buffer) }
    }

    fn build_members(&self, items: Vec<(String, JSON)>, length: usize) -> ParseResult {
        ParseResult { value: ParsedValue::Members(items), length, buffer: Rc::clone(&self.buffer) }
    }

    fn build_raw_str(&self, s: &str, length: usize) -> ParseResult {
        ParseResult { value: ParsedValue::RawStr(String::from(s)), length, buffer: Rc::clone(&self.buffer) }
    }

    fn build_ignored(&self, ) -> ParseResult {
        ParseResult { value: ParsedValue::Ignored, length: 0, buffer: Rc::clone(&self.buffer) }
    }
}
