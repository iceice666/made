#[derive(Debug, Clone)]
pub struct SourceHolder {
    source: String,
    pub start: usize,
    pub current: usize,
    pub line: usize,
}

impl SourceHolder {
    pub fn new(source: String) -> Self {
        SourceHolder {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn get_lexeme(&self) -> &str {
        &self.source[self.start..self.current]
    }

    pub fn reset_start(&mut self) {
        self.start = self.current
    }

    // return current, increase index
    pub fn advance(&mut self) -> Option<char> {
        let ch = self.source.chars().nth(self.current)?;
        self.current += 1;
        if ch == '\n' {
            self.line += 1;
        }

        Some(ch)
    }

    pub fn peek(&mut self) -> Option<char> {
        self.source.chars().nth(self.current)
    }
}
