use std::sync::Arc;

use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use thin_vec::ThinVec;

use crate::ast::{Ident, Span};
use crate::typer::Type;

/// The type of an unsigned integer.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TyUint {
    Uint64,
}

/// The type of an integer literal.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TyIntegerLiteral {
    Unsigned(u128, TyUint),
}

/// The kind of a [`TyLiteral`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TyLiteralKind {
    String(SmolStr),
    Integer(TyIntegerLiteral),
}

/// A typed literal.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TyLiteral {
    pub kind: TyLiteralKind,
    pub span: Span,
}

/// The kind of a [`TyExpr`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TyExprKind {
    /// A literal.
    Literal(TyLiteral),

    /// A reference to a variable.
    Variable { name: Ident },

    /// A function call.
    Call {
        fun: Box<TyExpr>,
        args: ThinVec<Box<TyExpr>>,
    },
}

/// A typed expression.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TyExpr {
    pub kind: TyExprKind,
    pub span: Span,
    pub ty: Arc<Type>,
}

/// The kind of a [`TyStmt`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TyStmtKind {
    /// An item.
    Item(TyItem),

    /// An expression.
    Expr(TyExpr),
}

/// A typed function definition.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TyFn {
    pub params: ThinVec<TyFnParam>,
    pub return_ty: Arc<Type>,
    pub body: ThinVec<TyStmt>,
}

/// A parameter to a [`TyFn`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TyFnParam {
    pub name: Ident,
    pub ty: Arc<Type>,
    pub span: Span,
}

/// A typed statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TyStmt {
    pub kind: TyStmtKind,
    pub span: Span,
}

/// The kind of a [`TyItem`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TyItemKind {
    /// A function declaration (`fn`).
    Fn(Box<TyFn>),
}

/// An item in a [`TyModule`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TyItem {
    pub kind: TyItemKind,
    pub name: Ident,
}

/// A module.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TyModule {
    pub items: ThinVec<TyItem>,
}
