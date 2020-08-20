
use std::fmt;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub range: std::ops::Range<usize>
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ParseError: {} at {}..{}", self.message, self.range.start, self.range.end)
    }
}

impl std::error::Error for ParseError {}
