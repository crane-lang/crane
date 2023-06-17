use std::sync::Arc;

use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use thin_vec::ThinVec;

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub enum Type {
    /// A user-defined type.
    UserDefined {
        /// The module in which the type resides.
        module: SmolStr,

        /// The name of the type.
        name: SmolStr,
    },

    /// A function type.
    Fn {
        args: ThinVec<Arc<Type>>,
        return_ty: Arc<Type>,
    },
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::*;

    /// Tests the size of [`Type`] to ensure it doesn't unintentionally get bigger.
    #[test]
    #[cfg(target_pointer_width = "64")]
    fn test_type_size() {
        insta::assert_snapshot!(size_of::<Type>().to_string(), @"48");
    }
}
