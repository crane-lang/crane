use std::hash::{Hash, Hasher};

use smol_str::SmolStr;

use crate::ast::Span;

/// An identifier.
#[derive(Debug, Eq, Clone)]
pub struct Ident {
    pub name: SmolStr,
    pub span: Span,
}

impl PartialEq for Ident {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
