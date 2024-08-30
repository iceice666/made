#[derive(Debug, Clone)]
pub struct Source {
    source: String,
    pub start: usize,
    pub current: usize,
    pub line: usize,
}

impl Source {
    pub fn new(source: String) -> Self {
        Source {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn get_lexeme(&self) -> &str {
        &self.source[self.start..self.current]
    }

    pub fn reset(&mut self) {
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
    
    pub fn consume(&mut self) {
        self.advance();
    }

    pub fn peek(&mut self) -> Option<char> {
        self.source.chars().nth(self.current)
    }

    pub fn has_next(&mut self) -> bool {
        self.current < self.source.len()
    }
}

