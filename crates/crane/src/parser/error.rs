use crate::ast::Span;
use crate::lexer::{LexError, LexErrorKind};

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParseErrorKind {
    LexError(LexErrorKind),
    Error(String),
}

impl std::fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::LexError(error) => error.to_string(),
                Self::Error(message) => message.to_string(),
            }
        )
    }
}

impl From<LexError> for ParseError {
    fn from(value: LexError) -> Self {
        Self {
            kind: ParseErrorKind::LexError(value.kind),
            span: value.span,
        }
    }
}
