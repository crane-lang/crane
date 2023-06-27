use std::sync::Arc;

use smol_str::SmolStr;

use crate::typer::Type;

pub struct TyContext {
    pub unit: Arc<Type>,
    pub uint64: Arc<Type>,
    pub string: Arc<Type>,
}

impl TyContext {
    pub fn new() -> Self {
        Self {
            unit: Arc::new(Type::UserDefined {
                module: SmolStr::new_inline("std::prelude"),
                name: SmolStr::new_inline("()"),
            }),
            string: Arc::new(Type::UserDefined {
                module: SmolStr::new_inline("std::prelude"),
                name: SmolStr::new_inline("String"),
            }),
            uint64: Arc::new(Type::UserDefined {
                module: SmolStr::new_inline("std::prelude"),
                name: SmolStr::new_inline("Uint64"),
            }),
        }
    }
}
