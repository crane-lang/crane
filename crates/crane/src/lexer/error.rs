use serde::{Serialize, Deserialize};
use thiserror::Error;

use crate::ast::Span;

#[derive(Error, Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub span: Span,
}

impl Default for LexError {
    fn default() -> Self {
        Self {
            kind: Default::default(),
            span: Span::new(0, 0),
        }
    }
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Error, PartialEq, Clone, Default, Serialize, Deserialize)]
pub enum LexErrorKind {
    #[default]
    #[error("unknown")]
    Unknown,
}
