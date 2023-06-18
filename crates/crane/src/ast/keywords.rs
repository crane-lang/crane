use smol_str::SmolStr;

use crate::ast::{Ident, DUMMY_SPAN};

pub const FN: Ident = Ident {
    name: SmolStr::new_inline("fn"),
    span: DUMMY_SPAN,
};

pub const LET: Ident = Ident {
    name: SmolStr::new_inline("let"),
    span: DUMMY_SPAN,
};

pub const MOD: Ident = Ident {
    name: SmolStr::new_inline("mod"),
    span: DUMMY_SPAN,
};

pub const PUB: Ident = Ident {
    name: SmolStr::new_inline("pub"),
    span: DUMMY_SPAN,
};

pub const STRUCT: Ident = Ident {
    name: SmolStr::new_inline("struct"),
    span: DUMMY_SPAN,
};

pub const UNION: Ident = Ident {
    name: SmolStr::new_inline("union"),
    span: DUMMY_SPAN,
};
