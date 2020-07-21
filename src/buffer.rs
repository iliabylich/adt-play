use regex::{Regex, escape};

#[derive(Clone, Debug)]
pub struct Buffer {
    string: String,
    pub(crate) pos: usize
}

impl From<&str> for Buffer {
    fn from(input: &str) -> Buffer {
        Buffer {
            string: input.to_owned(),
            pos: 0
        }
    }
}

impl Buffer {
    pub fn len(&self) -> usize {
        self.string.len()
    }

    pub fn scan_str(&mut self, s: &str) -> Result<(String, usize), String> {
        let re = Regex::new(&escape(s)).expect("Invalid regex source");
        self.scan_re(re)
    }

    pub fn scan_re(&mut self, re: Regex) -> Result<(String, usize), String> {
        let re = Regex::new(&format!(r"\A{}", re.as_str())).unwrap();
        let rest = &self.string[self.pos..];

        match re.find(rest) {
            Some(matched) => {
                self.pos += matched.range().len();
                Ok((matched.as_str().to_owned(), matched.range().len()))
            },
            None => {
                Err(format!("failed to capture {}", re))
            }
        }
    }

    pub fn rollback(&mut self, length: usize) {
        self.pos -= length;
    }
}
