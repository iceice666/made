use crate::lexer::{Token, TokenType};
use std::collections::VecDeque;

use super::error::{ParseError, ParseResult};

#[derive(Debug)]
pub(crate) struct Source {
    tokens: VecDeque<Token>,
}

impl Source {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into(),
        }
    }

    pub(crate) fn advance(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }

    pub(crate) fn peek(&self) -> Option<&Token> {
        self.tokens.front()
    }

    pub(crate) fn check(&self, token_type: TokenType) -> bool {
        match self.peek() {
            Some(token) => matches!(&token.r#type, token_type),
            None => false,
        }
    }

    pub(crate) fn consume(&mut self) -> ParseResult<()> {
        match self.advance() {
            Some(_) => Ok(()),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    pub(crate) fn has_next(&self) -> bool {
        !self.tokens.is_empty()
    }
}
