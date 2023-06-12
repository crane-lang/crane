use smol_str::SmolStr;
use thin_vec::ThinVec;

use crate::ast::{Ident, Span};

/// The kind of a [`TyExpr`].
#[derive(Debug, Clone)]
pub enum TyExprKind {
    /// A literal.
    Literal(SmolStr),

    /// A reference to a variable.
    Variable { name: Ident },

    /// A function call.
    Call {
        fun: Box<TyExpr>,
        args: ThinVec<Box<TyExpr>>,
    },
}

/// A typed expression.
#[derive(Debug, Clone)]
pub struct TyExpr {
    pub kind: TyExprKind,
    pub span: Span,
}

/// The kind of a [`TyStmt`].
#[derive(Debug, Clone)]
pub enum TyStmtKind {
    /// An item.
    Item(TyItem),

    /// An expression.
    Expr(TyExpr),
}

/// A typed function definition.
#[derive(Debug, Clone)]
pub struct TyFn {
    pub body: ThinVec<TyStmt>,
}

/// A typed statement.
#[derive(Debug, Clone)]
pub struct TyStmt {
    pub kind: TyStmtKind,
    pub span: Span,
}

/// The kind of a [`TyItem`].
#[derive(Debug, Clone)]
pub enum TyItemKind {
    /// A function declaration (`fn`).
    Fn(Box<TyFn>),
}

/// An item in a [`TyModule`].
#[derive(Debug, Clone)]
pub struct TyItem {
    pub kind: TyItemKind,
    pub name: Ident,
}

/// A module.
#[derive(Debug, Clone)]
pub struct TyModule {
    pub items: ThinVec<TyItem>,
}
