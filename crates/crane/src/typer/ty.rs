use std::ops::Deref;
use std::sync::Arc;

use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use thin_vec::ThinVec;

/// A type in the type system.
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct Ty(Arc<TyKind>);

impl Deref for Ty {
    type Target = TyKind;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Ty {
    pub fn new(kind: TyKind) -> Self {
        Self(Arc::new(kind))
    }
}

/// The kind of a [`Ty`].
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub enum TyKind {
    /// A user-defined type.
    UserDefined {
        /// The module in which the type resides.
        module: SmolStr,

        /// The name of the type.
        name: SmolStr,
    },

    /// A function type.
    Fn { args: ThinVec<Ty>, return_ty: Ty },
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Tests the size of [`Type`] to ensure it doesn't unintentionally get bigger.
    #[test]
    fn test_type_size() {
        use std::mem::size_of;

        insta::assert_snapshot!(size_of::<TyKind>().to_string(), @"48");
    }
}
