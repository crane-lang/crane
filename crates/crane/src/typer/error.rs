use serde::{Deserialize, Serialize};

use crate::ast::{Span, TyPath};

#[derive(Debug, Serialize, Deserialize)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Span,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum TypeErrorKind {
    UnknownFunction(TyPath),
    Error(String),
}
