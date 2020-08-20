mod buffer;
use buffer::{Buffer};

mod parser;
pub use parser::Parser;

mod error;
pub use error::ParseError;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum JSON {
    Null,
    False,
    True,
    Number(String),
    String(String),
    Array(Vec<JSON>),
    Object(Vec<(String, JSON)>)
}

pub fn parse(input: &str) -> Result<JSON, ParseError> {
    Parser::new(input).parse()
}

#[cfg(test)]
mod tests {
    use super::{JSON, parse};

    #[test]
    fn it_works() {
        let input = r#"
            {
                "whitespaces":" ",
                "null": null,
                "true": true,
                "false": false,
                "numbers": [
                    1,
                    -12,
                    1.2,
                    -1.2,
                    1e3,
                    -1e3,
                    1.2e3,
                    -1.2e3,
                    1E3,
                    -1E3,
                    1.2E3,
                    -1.2E3,
                    1e-3,
                    -1e-3,
                    1.2e-3,
                    -1.2e-3,
                    1E-3,
                    -1E-3,
                    1.2E-3,
                    -1.2E-3
                ],
                "strings": "\b \f \n \r \t \u0123 \u4567 \u890A \uBCDE \uffff some text",
                "array": [1,2],
                "object": {"key":"value"}
            }
        "#;
        let result = parse(input);

        let expected = JSON::Object(vec![
                (
                    String::from("whitespaces"),
                    JSON::String(String::from(" "))
                ),
                (
                    String::from("null"),
                    JSON::Null
                ),
                (
                    String::from("true"),
                    JSON::True
                ),
                (
                    String::from("false"),
                    JSON::False
                ),
                (
                    String::from("numbers"),
                    JSON::Array(vec![
                        JSON::Number(String::from("1") ),
                        JSON::Number(String::from("-12") ),
                        JSON::Number(String::from("1.2") ),
                        JSON::Number(String::from("-1.2") ),
                        JSON::Number(String::from("1e3") ),
                        JSON::Number(String::from("-1e3") ),
                        JSON::Number(String::from("1.2e3") ),
                        JSON::Number(String::from("-1.2e3") ),
                        JSON::Number(String::from("1E3") ),
                        JSON::Number(String::from("-1E3") ),
                        JSON::Number(String::from("1.2E3") ),
                        JSON::Number(String::from("-1.2E3") ),
                        JSON::Number(String::from("1e-3") ),
                        JSON::Number(String::from("-1e-3") ),
                        JSON::Number(String::from("1.2e-3") ),
                        JSON::Number(String::from("-1.2e-3") ),
                        JSON::Number(String::from("1E-3") ),
                        JSON::Number(String::from("-1E-3") ),
                        JSON::Number(String::from("1.2E-3") ),
                        JSON::Number(String::from("-1.2E-3") ),
                    ]),
                ),
                (
                    String::from("strings"),
                    JSON::String(
                        String::from("\\b \\f \n \r \t ģ 䕧 褊 볞 \u{ffff} some text"),
                    ),
                ),
                (
                    String::from("array"),
                    JSON::Array(vec![
                        JSON::Number(String::from("1")),
                        JSON::Number(String::from("2")),
                    ]),
                ),
                (
                    String::from("object"),
                    JSON::Object(vec![
                        (String::from("key"), JSON::String(String::from("value"))),
                    ]),
                ),
            ],
        );

        match result {
            Ok(result) => assert_eq!(result, expected),
            Err(err) => panic!("parse error, {}", err)
        }
    }
}
