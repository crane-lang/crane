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

/// A field declaration in a `struct` or [`Variant`] of a `union`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TyFieldDecl {
    pub name: Option<Ident>,
    pub ty: Ident,
    pub span: Span,
}

/// The data for a `struct` or [`Variant`] of a `union`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TyVariantData {
    /// A struct variant.
    Struct(ThinVec<TyFieldDecl>),

    /// A tuple variant.
    Tuple(ThinVec<TyFieldDecl>),

    /// A unit variant.
    Unit,
}

impl TyVariantData {
    /// Return the fields of this variant.
    pub fn fields(&self) -> &[TyFieldDecl] {
        match self {
            TyVariantData::Struct(fields) | TyVariantData::Tuple(fields) => fields,
            _ => &[],
        }
    }
}

/// A variant in a `union`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TyVariant {
    pub name: Ident,
    pub data: TyVariantData,
    pub span: Span,
}

/// A `struct` declaration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TyStructDecl(pub TyVariantData);

/// A `union` declaration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TyUnionDecl {
    /// The variants of the `union`.
    pub variants: ThinVec<TyVariant>,
}

/// The kind of a [`TyItem`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TyItemKind {
    // TODO: Remove `Box`?
    /// A function declaration (`fn`).
    Fn(Box<TyFn>),

    /// A struct declaration (`struct`).
    Struct(TyStructDecl),

    /// A union declaration (`union`).
    Union(TyUnionDecl),
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

#[cfg(test)]
mod tests {
    #[cfg(all(target_arch = "x86_64", target_pointer_width = "64"))]
    use super::*;

    /// Tests the size of AST nodes to ensure they don't unintentionally get bigger.
    #[test]
    #[cfg(all(target_arch = "x86_64", target_pointer_width = "64"))]
    fn test_ast_node_sizes() {
        use std::mem::size_of;

        dbg!(size_of::<TyExpr>().to_string());
        dbg!(size_of::<TyExprKind>().to_string());
        dbg!(size_of::<TyFn>().to_string());
        dbg!(size_of::<TyItem>().to_string());
        dbg!(size_of::<TyItemKind>().to_string());
        dbg!(size_of::<TyStmt>().to_string());
        dbg!(size_of::<TyStmtKind>().to_string());

        // insta::assert_snapshot!(size_of::<TyExpr>().to_string(), @"80");
        // insta::assert_snapshot!(size_of::<TyExprKind>().to_string(), @"48");
        // insta::assert_snapshot!(size_of::<TyFn>().to_string(), @"24");
        // insta::assert_snapshot!(size_of::<TyItem>().to_string(), @"56");
        // insta::assert_snapshot!(size_of::<TyItemKind>().to_string(), @"16");
        // insta::assert_snapshot!(size_of::<TyStmt>().to_string(), @"96");
        // insta::assert_snapshot!(size_of::<TyStmtKind>().to_string(), @"80");
    }
}
