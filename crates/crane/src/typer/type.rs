use std::sync::Arc;

use smol_str::SmolStr;
use thin_vec::ThinVec;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    /// A user-defined type.
    UserDefined {
        /// The module in which the type resides.
        module: SmolStr,

        /// The name of the type.
        name: SmolStr,
    },

    Fn {
        args: ThinVec<Arc<Type>>,
        return_ty: Arc<Type>,
    },
}
