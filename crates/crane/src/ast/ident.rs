use std::hash::{Hash, Hasher};

use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

use crate::ast::Span;

/// An identifier.
#[derive(Debug, Eq, Clone, Serialize, Deserialize)]
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn partial_eq_impl_ignores_spans() {
        let name = SmolStr::new_inline("foo");

        let ident_a = Ident {
            name: name.clone(),
            span: Span::new(0, 3),
        };

        let ident_b = Ident {
            name,
            span: Span::new(4, 7),
        };

        // SANITY: Ensure spans are not equal.
        assert_ne!(ident_a.span, ident_b.span);

        assert_eq!(ident_a, ident_b)
    }

    #[test]
    fn hash_impl_ignores_spans() {
        use std::collections::hash_map::DefaultHasher;

        fn hash_ident(ident: &Ident) -> u64 {
            let mut hasher = DefaultHasher::new();

            ident.hash(&mut hasher);
            hasher.finish()
        }

        let name = SmolStr::new_inline("foo");

        let ident_a = Ident {
            name: name.clone(),
            span: Span::new(0, 3),
        };

        let ident_b = Ident {
            name,
            span: Span::new(4, 7),
        };

        // SANITY: Ensure spans are not equal.
        assert_ne!(ident_a.span, ident_b.span);

        assert_eq!(hash_ident(&ident_a), hash_ident(&ident_b))
    }
}
