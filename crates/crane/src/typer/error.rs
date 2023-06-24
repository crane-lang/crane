use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use thin_vec::ThinVec;

use crate::ast::{Span, TyPath};

#[derive(Debug, Serialize, Deserialize)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Span,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum TypeErrorKind {
    InvalidFunctionName {
        reason: String,
        suggestion: SmolStr,
    },
    InvalidTypeName {
        reason: String,
        suggestion: SmolStr,
    },
    UnknownModule {
        path: TyPath,
        options: ThinVec<TyPath>,
    },
    UnknownFunction {
        path: TyPath,
        options: ThinVec<TyPath>,
    },
    UnknownType {
        path: TyPath,
        options: ThinVec<TyPath>,
    },
    Error(String),
}
