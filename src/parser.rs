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
    pub value: Result<T, String>,
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

impl<T> ParseResult<T> where T: Clone {
    fn rollback(&self) {
        self.buffer.borrow_mut().rollback(self.length)
    }

    pub fn and<TNext>(&self, fnext: &dyn Fn() -> ParseResult<TNext>) -> ParseResult<(T, TNext)> where TNext: Clone {
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
                        let value= (prev.clone(), next.clone());

                        ParseResult { value: Ok(value), length: prev_length + next_length, buffer }
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
            Ok(value) => f(&value)
        }
    }

    pub fn map_value<TNext>(&self, f: &dyn Fn(&T) -> TNext) -> ParseResult<TNext> {
        self.map(&|value|
            ParseResult { value: Ok(f(value)), length: self.length, buffer: self.buffer.clone() }
        )
    }

    pub fn map_to_value<TNext>(&self, value: TNext) -> ParseResult<TNext> where TNext: Clone {
        self.map_value(&|_json| value.clone())
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
            Ok(json) => Ok(json),
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
            .or(&|| self.try_string().map_value(&|value| JSON::String(value.clone()) ) )
            .or(&|| self.try_number() )
            .or(&|| self.try_raw_string("true").map_to_value(JSON::True) )
            .or(&|| self.try_raw_string("false").map_to_value(JSON::False) )
            .or(&|| self.try_raw_string("null").map_to_value(JSON::Null) )
    }

    fn try_object(&self) -> ParseResult<JSON> {
        self.try_raw_string("{")
            .and(&|| self.try_members().or(&|| self.skip_ws().map_to_value(vec![]) ) )
            .and(&|| self.try_raw_string("}") )
            .map_value(&|((_lcurly, items), _rcurly)| {
                JSON::Object(items.clone())
            })
    }

    fn try_members(&self) -> ParseResult<Vec<JsonObjectMember>> {
        self.try_member().map_value::<JsonObjectMember>(&|(key, value)| (key.clone(), value.clone()) )
            .and(&||
                (
                    self.try_raw_string(",")
                        .and(&|| self.try_members())
                        .map_value(&|(_comma, members)| members.clone() )
                )
                .or(&|| ParseResult { value: Ok(vec![]), buffer: self.buffer.clone(), length: 0 } )
            )
            .map_value(&|(member, members)| {
                let mut result = vec![member.clone()];
                for member in members.into_iter() {
                    result.push(member.clone());
                }
                result
            })
    }

    fn try_member(&self) -> ParseResult<JsonObjectMember> {
        self.skip_ws()
            .and(&|| self.try_string() )
            .and(&|| self.skip_ws() )
            .and(&|| self.try_raw_string(":") )
            .and(&|| self.skip_ws() )
            .and(&|| self.try_element() )
            .map_value(&|(((((_ws, key) , _ws2), _colon), _ws3), value)| {
                (key.clone(), value.clone())
            })
    }

    fn try_array(&self) -> ParseResult<JSON> {
        self.skip_string("[")
            .and(&|| self.try_array_items().or(&|| self.skip_ws().map_to_value(vec![]) ) )
            .and(&|| self.try_raw_string("]") )
            .map_value(&|((_lbrace, items), _rbrace)| JSON::Array(items.clone()) )
    }

    fn try_array_items(&self) -> ParseResult<Vec<JSON>> {
        self.try_element()
            .map_value(&|element| element.clone() )
            .and(&||
                self.try_raw_string(",")
                    .and(&|| self.try_array_items() )
                    .map_value(&|(_comma, items)| items.clone())
                    .or(&|| ParseResult { value: Ok(vec![]), buffer: self.buffer.clone(), length: 0 } )
            )
            .map_value(&|(item, items)| {
                let mut result = vec![item.clone()];
                for item in items.into_iter() {
                    result.push(item.clone());
                }
                result
            })
    }

    fn try_element(&self) -> ParseResult<JSON> {
        self.skip_ws()
            .and(&|| self.try_value() )
            .and(&|| self.skip_ws() )
            .map_value(&|((_ws1, value), _ws2)| value.clone() )
    }

    fn try_string(&self) -> ParseResult<String> {
        self.try_raw_string("\"").map_to_value(String::from(""))
            .and(&|| self.try_characters() )
            .and(&|| self.try_raw_string("\"").map_to_value(String::from("")) )
            .map_value(&|((_sep1, string), _sep2)| string.clone() )
    }

    fn try_characters(&self) -> ParseResult<String> {
        self.try_character()
            .and(&|| self.try_characters() )
            .map_value(&|(char, chars)| format!("{}{}", char, chars) )
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
                    .map_value(&|(_escape, string)| string.clone() )
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
                    .map_value(&|((((_u, part1), part2), part3), part4)| {
                        let value = format!("{}{}{}{}", part1, part2, part3, part4);
                        let codepoint = u32::from_str_radix(&value, 16).unwrap();
                        std::char::from_u32(codepoint).unwrap().to_string()
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
            .map_value(&|((int, frac), exp)| JSON::Number(format!("{}{}{}", int, frac, exp)) )
    }

    fn try_integer(&self) -> ParseResult<String> {
        self.try_raw_string("-")
            .or(&|| self.build_raw_str("", 0) )
            .and(&||
                (self.try_onenine().and(&|| self.try_digits() ))
                .map_value(&|(digit, digits)| format!("{}{}", digit, digits))
                .or(&|| self.try_digit() )
            )
            .map_value(&|(sign, digits)| format!("{}{}", sign, digits) )
    }

    fn try_digits(&self) -> ParseResult<String> {
        self.try_digit()
            .and(&|| self.try_digits().or(&|| self.build_raw_str("", 0) ) )
            .map_value(&|(digit, digits)| format!("{}{}", digit, digits) )
    }

    fn try_digit(&self) -> ParseResult<String> {
        self.try_raw_string("0").or(&|| self.try_onenine())
    }

    fn try_onenine(&self) -> ParseResult<String> {
        self.try_raw_regex(Regex::new("[1-9]").unwrap())
    }

    fn try_fraction(&self) -> ParseResult<String> {
        (self.try_raw_string(".").and(&|| self.try_digits() ))
            .map_value(&|(dot, digits)| format!("{}{}", dot, digits))
            .or(&|| self.build_raw_str("", 0) )
    }

    fn try_exponent(&self) -> ParseResult<String> {
        self.try_raw_regex(Regex::new("[eE]").unwrap())
            .and(&|| self.try_sign() )
            .and(&|| self.try_digits() )
            .map_value(&|((e, sign), digits)| format!("{}{}{}", e, sign, digits) )
            .or(&|| self.build_raw_str("", 0) )
    }

    fn try_sign(&self) -> ParseResult<String> {
        self.try_raw_string("-")
            .or(&|| self.try_raw_string("+") )
            .or(&|| self.build_raw_str("", 0) )
    }

    fn skip_ws(&self) -> ParseResult<()> {
        self.try_raw_regex(Regex::new(r"\s*").unwrap()).map_to_value(())
    }

    fn try_raw_string(&self, s: &str) -> ParseResult<String> {
        match self.buffer.try_borrow_mut().unwrap().scan_str(s) {
            Ok((matched, length)) => self.build_raw_str(&matched, length),
            Err(message) => self.build_err(message)
        }
    }

    fn skip_string(&self, s: &str) -> ParseResult<()> {
        self.try_raw_string(s).map_to_value(())
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
        ParseResult { value: Ok(s.into()), length, buffer: self.buffer.clone() }
    }
}
