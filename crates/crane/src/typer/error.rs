use serde::{Deserialize, Serialize};
use thin_vec::ThinVec;

use crate::ast::{Span, TyPath};

#[derive(Debug, Serialize, Deserialize)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Span,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum TypeErrorKind {
    UnknownModule {
        path: TyPath,
        options: ThinVec<TyPath>,
    },
    UnknownFunction {
        path: TyPath,
        options: ThinVec<TyPath>,
    },
    Error(String),
}
