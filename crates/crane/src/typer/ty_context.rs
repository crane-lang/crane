use typed_arena::Arena as TypedArena;

use crate::interned::Interned;
use crate::typer::ty::{Ty, TyKind};
use crate::typer::UintTy;

#[derive(Default)]
pub struct Arena<'ctx> {
    pub tys: TypedArena<TyKind<'ctx>>,
}

impl<'ctx> Arena<'ctx> {
    pub fn intern_ty(&self, kind: TyKind<'ctx>) -> Ty {
        Ty(Interned::new_unchecked(self.tys.alloc(kind)))
    }
}

pub struct CommonTypes<'ctx> {
    pub unit: Ty<'ctx>,
    pub uint64: Ty<'ctx>,
}

pub struct TyContext<'ctx> {
    pub arena: &'ctx Arena<'ctx>,
    pub types: CommonTypes<'ctx>,
}

impl<'ctx> TyContext<'ctx> {
    pub fn new(arena: &'ctx Arena<'ctx>) -> Self {
        Self {
            arena,
            types: CommonTypes {
                unit: arena.intern_ty(TyKind::Unit),
                uint64: arena.intern_ty(TyKind::Uint(UintTy::U64)),
            },
        }
    }
}