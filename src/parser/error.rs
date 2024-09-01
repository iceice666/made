use miette::Diagnostic;
use thiserror::Error;
use crate::lexer;

pub(crate) type ParseResult<T> = Result<T, ParseError>;

#[derive(Error, Debug, Diagnostic)]
pub(crate) enum ParseError {
    #[error("Unexpected token: {0}")]
    UnexpectedToken(lexer::Token),

    #[error("Unexpected end of file")]
    UnexpectedEOF,
}
