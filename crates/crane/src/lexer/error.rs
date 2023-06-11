use std::ops::Range;

use thiserror::Error;

#[derive(Error, Debug, PartialEq, Clone, Default)]
pub struct LexError {
    pub kind: LexErrorKind,

    pub span: Range<usize>,
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Error, PartialEq, Clone, Default)]
pub enum LexErrorKind {
    #[default]
    #[error("unknown")]
    Unknown,
}
