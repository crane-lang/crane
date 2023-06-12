mod source_span;
mod span;

pub use source_span::*;
pub use span::*;

use smol_str::SmolStr;
use thin_vec::ThinVec;

/// An identifier.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Ident(pub SmolStr);

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// The kind of an [`Expr`].
#[derive(Debug, Clone)]
pub enum ExprKind {
    /// A literal.
    Literal(SmolStr),

    /// A reference to a variable.
    Variable { name: Ident },

    /// A function call.
    Call {
        fun: Box<Expr>,
        args: ThinVec<Box<Expr>>,
    },
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
    pub body: ThinVec<Stmt>,
}

/// A statement.
#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
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
