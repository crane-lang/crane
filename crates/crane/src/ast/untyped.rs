use smol_str::SmolStr;
use thin_vec::ThinVec;

use crate::ast::{Ident, Span};

/// The kind of an [`Expr`].
#[derive(Debug, Clone)]
pub enum ExprKind {
    /// A literal.
    Literal(Literal),

    /// A reference to a variable.
    Variable { name: Ident },

    /// A function call.
    Call {
        fun: Box<Expr>,
        args: ThinVec<Box<Expr>>,
    },
}

/// The kind of a [`Literal`].
#[derive(Debug, Clone)]
pub enum LiteralKind {
    String,
}

/// A literal.
#[derive(Debug, Clone)]
pub struct Literal {
    pub kind: LiteralKind,
    pub value: SmolStr,
}

/// An expression.
#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

/// The kind of a [`Stmt`].
#[derive(Debug, Clone)]
pub enum StmtKind {
    /// An item.
    Item(Item),

    /// An expression.
    Expr(Expr),
}

/// A function definition.
#[derive(Debug, Clone)]
pub struct Fn {
    pub params: ThinVec<FnParam>,
    pub body: ThinVec<Stmt>,
}

/// A parameter to a [`Fn`].
#[derive(Debug, Clone)]
pub struct FnParam {
    pub name: Ident,
    pub span: Span,
}

/// A statement.
#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

/// The kind of an [`Item`].
#[derive(Debug, Clone)]
pub enum ItemKind {
    /// A function declaration (`fn`).
    Fn(Box<Fn>),
}

/// An item in a [`Module`].
#[derive(Debug, Clone)]
pub struct Item {
    pub kind: ItemKind,
    pub name: Ident,
}

/// A module.
#[derive(Debug, Clone)]
pub struct Module {
    pub items: ThinVec<Item>,
}
