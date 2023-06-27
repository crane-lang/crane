use crate::interned::Interned;

/// A type in the type system.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Ty<'ctx>(Interned<'ctx, TyKind<'ctx>>);

/// The kind of a type in the type system.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum TyKind<'ctx> {
    /// An unsigned integer type.
    Uint(UintTy),

    /// An algebraic data type (ADT).
    Adt(AdtDecl<'ctx>),
}

/// An unsigned integer type.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum UintTy {
    /// A 64-bit unsigned integer.
    U64,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct AdtDecl<'ctx>(pub Interned<'ctx, AdtDeclData>);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct AdtDeclData {}
