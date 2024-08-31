use crate::lexer::Token;
use std::collections::VecDeque;

#[derive(Debug)]
pub(crate) struct Source {
    source: VecDeque<Token>,
}

impl Source {
    fn advance(&mut self) -> Option<Token> {
        self.source.pop_front()
    }

    fn peek(&mut self) -> Option<&Token> {
        self.source.front()
    }

    fn consume(&mut self) {
        self.advance();
    }

    fn has_next(&mut self) -> bool {
        !self.source.is_empty()
    }
}
