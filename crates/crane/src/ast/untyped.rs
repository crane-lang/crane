use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use thin_vec::ThinVec;

use crate::ast::{Ident, Span};

/// The kind of an [`Expr`].
#[derive(Debug, Clone, Serialize, Deserialize)]
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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LiteralKind {
    String,
    Integer,
}

/// A literal.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Literal {
    pub kind: LiteralKind,
    pub value: SmolStr,
}

/// An expression.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

/// The kind of a [`Stmt`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum StmtKind {
    /// An item.
    Item(Item),

    /// An expression.
    Expr(Expr),
}

/// A function definition.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Fn {
    pub params: ThinVec<FnParam>,
    pub return_ty: Option<Ident>,
    pub body: ThinVec<Stmt>,
}

/// A parameter to a [`Fn`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FnParam {
    pub name: Ident,
    pub ty: Ident,
    pub span: Span,
}

/// A statement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

/// A field declaration in a `struct` or [`Variant`] of a `union`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldDecl {
    pub name: Option<Ident>,
    pub ty: Ident,
    pub span: Span,
}

/// The data for a `struct` or [`Variant`] of a `union`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum VariantData {
    /// A struct variant.
    Struct(ThinVec<FieldDecl>),

    /// A tuple variant.
    Tuple(ThinVec<FieldDecl>),

    /// A unit variant.
    Unit,
}

impl VariantData {
    /// Return the fields in this [`VariantData`].
    pub fn fields(&self) -> &[FieldDecl] {
        match self {
            VariantData::Struct(fields) | VariantData::Tuple(fields) => fields,
            VariantData::Unit => &[],
        }
    }
}

/// A variant in a `union`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Variant {
    pub name: Ident,
    pub data: VariantData,
    pub span: Span,
}

/// A `struct` declaration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructDecl(pub VariantData);

/// A `union` declaration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnionDecl {
    /// The variants of the `union`.
    pub variants: ThinVec<Variant>,
}

/// The kind of an [`Item`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ItemKind {
    /// A function declaration (`fn`).
    Fn(Box<Fn>),

    /// A struct declaration (`struct`).
    Struct(StructDecl),

    /// A union declaration (`union`).
    Union(UnionDecl),
}

/// An item in a [`Module`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Item {
    pub kind: ItemKind,
    pub name: Ident,
}

/// A module.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    pub items: ThinVec<Item>,
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use super::*;

    /// Tests the size of AST nodes to ensure they don't unintentionally get bigger.
    #[test]
    #[cfg(target_pointer_width = "64")]
    fn test_ast_node_sizes() {
        insta::assert_snapshot!(size_of::<Expr>().to_string(), @"56");
        insta::assert_snapshot!(size_of::<ExprKind>().to_string(), @"40");
        insta::assert_snapshot!(size_of::<Fn>().to_string(), @"56");
        insta::assert_snapshot!(size_of::<Item>().to_string(), @"56");
        insta::assert_snapshot!(size_of::<ItemKind>().to_string(), @"16");
        insta::assert_snapshot!(size_of::<Stmt>().to_string(), @"80");
        insta::assert_snapshot!(size_of::<StmtKind>().to_string(), @"64");
    }
}
