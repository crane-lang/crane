use std::borrow::Borrow;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};

use typed_arena::Arena as TypedArena;

use crate::interned::Interned;
use crate::typer::ty::{Ty, TyKind};
use crate::typer::{FnSig, UintTy};

#[derive(Default)]
pub struct Arena<'ctx> {
    pub tys: TypedArena<TyKind<'ctx>>,
    pub ty_lists: TypedArena<Vec<Ty<'ctx>>>,
}

/// A value interned in an [`InternedSet`].
struct InternedInSet<'ctx, T: ?Sized>(&'ctx T);

impl<'ctx, T: 'ctx + ?Sized> Clone for InternedInSet<'ctx, T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<'ctx, T: 'ctx + ?Sized> Copy for InternedInSet<'ctx, T> {}

impl<'ctx> Borrow<TyKind<'ctx>> for InternedInSet<'ctx, TyKind<'ctx>> {
    fn borrow(&self) -> &TyKind<'ctx> {
        &self.0
    }
}

impl<'ctx> PartialEq for InternedInSet<'ctx, TyKind<'ctx>> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<'ctx> Eq for InternedInSet<'ctx, TyKind<'ctx>> {}

impl<'ctx> Hash for InternedInSet<'ctx, TyKind<'ctx>> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<'ctx> Borrow<Vec<Ty<'ctx>>> for InternedInSet<'ctx, Vec<Ty<'ctx>>> {
    fn borrow(&self) -> &Vec<Ty<'ctx>> {
        &self.0
    }
}

impl<'ctx> PartialEq for InternedInSet<'ctx, Vec<Ty<'ctx>>> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<'ctx> Eq for InternedInSet<'ctx, Vec<Ty<'ctx>>> {}

impl<'ctx> Hash for InternedInSet<'ctx, Vec<Ty<'ctx>>> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

type InternedSet<'ctx, T> = HashSet<InternedInSet<'ctx, T>>;

pub struct Interners<'ctx> {
    arena: &'ctx Arena<'ctx>,

    ty: InternedSet<'ctx, TyKind<'ctx>>,
    ty_list: InternedSet<'ctx, Vec<Ty<'ctx>>>,
}

impl<'ctx> Interners<'ctx> {
    pub fn new(arena: &'ctx Arena<'ctx>) -> Self {
        Self {
            arena,
            ty: Default::default(),
            ty_list: Default::default(),
        }
    }

    pub fn intern_ty(&mut self, kind: TyKind<'ctx>) -> Ty<'ctx> {
        Ty(Interned::new_unchecked(
            &match self.ty.get(&kind) {
                Some(value) => *value,
                None => {
                    let value = InternedInSet(self.arena.tys.alloc(kind));
                    self.ty.insert(value);
                    value
                }
            }
            .0,
        ))
    }

    pub fn intern_fn(&mut self, params: Vec<Ty<'ctx>>, return_ty: Ty<'ctx>) -> Ty<'ctx> {
        let params = &match self.ty_list.get(&params) {
            Some(value) => *value,
            None => {
                let value = InternedInSet(self.arena.ty_lists.alloc(params));
                self.ty_list.insert(value);
                value
            }
        }
        .0;

        self.intern_ty(TyKind::Fn(FnSig { params, return_ty }))
    }
}

pub struct CommonTypes<'ctx> {
    pub unit: Ty<'ctx>,
    pub uint64: Ty<'ctx>,
}

pub struct TyContext<'ctx> {
    pub arena: &'ctx Arena<'ctx>,
    pub interners: Interners<'ctx>,
    pub types: CommonTypes<'ctx>,
}

impl<'ctx> TyContext<'ctx> {
    pub fn new(arena: &'ctx Arena<'ctx>) -> Self {
        let mut interners = Interners::new(arena);

        let types = CommonTypes {
            unit: interners.intern_ty(TyKind::Unit),
            uint64: interners.intern_ty(TyKind::Uint(UintTy::U64)),
        };

        Self {
            arena,
            interners,
            types,
        }
    }
}
