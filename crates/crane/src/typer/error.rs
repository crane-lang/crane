use std::ops::Range;

use crate::ast::Ident;

#[derive(Debug)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Range<usize>,
}

#[derive(Debug)]
pub enum TypeErrorKind {
    UnknownFunction { name: Ident },
}
