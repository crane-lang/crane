use serde::{Serialize, Deserialize};

use crate::ast::{Ident, Span};

#[derive(Debug, Serialize, Deserialize)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Span,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum TypeErrorKind {
    UnknownFunction { name: Ident },
    Error(String),
}
