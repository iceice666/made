// use miette::{Diagnostic, SourceSpan};
use crate::made::utils::source::SourceHolder;
use thiserror::Error;

pub(crate) type LexResult<T> = Result<T, LexerError>;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error(transparent)]
    IoError(#[from] std::io::Error),

    #[error("Unexpected character: {1}")]
    UnexpectedCharacter(SourceHolder, char),

    #[error("Unterminated string literal")]
    UndeterminedStringLiteral(SourceHolder),

    #[error("Unterminated char literal")]
    UndeterminedCharLiteral(SourceHolder),

    #[error("Empty character declaration")]
    EmptyCharDeclaration(SourceHolder),
}

impl LexerError {
    pub fn print_error(&self) {
        println!("{:#?}", self);
    }
}
