use super::source::SourceHolder;
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

pub(crate) type LexResult<T> = Result<T, LexicalError>;

type Source = SourceHolder;

#[derive(Error, Debug, Diagnostic)]
pub enum LexicalError {
    #[error(transparent)]
    IoError(#[from] std::io::Error),

    #[error("Unexpected character: {1}")]
    UnexpectedCharacter(Source, char),

    #[error("Unterminated string literal")]
    UndeterminedStringLiteral(Source),

    #[error("Unterminated char literal")]
    UndeterminedCharLiteral(Source),

    #[error("Empty character declaration")]
    EmptyCharDeclaration(Source),

    #[error("Invalid escape sequence: {1}")]
    InvalidEscapeSequence(Source, char),

    #[error("Invalid number literal")]
    MalformedNumber(Source),

    #[error("How you supposed to get here")]
    HowDidYouGetHere(Source),

    #[error("nop")]
    Nop,

    #[error("Reached end of file")]
    #[diagnostic(code(made::lexer::error::eof))]
    Eof(Source),
}

impl LexicalError {
    pub fn print_error(&self) {
        println!("{:#?}", self);
    }
}
