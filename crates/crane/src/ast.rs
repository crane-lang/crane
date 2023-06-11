use smol_str::SmolStr;
use thin_vec::ThinVec;

/// An identifier.
#[derive(Debug, Clone)]
pub struct Ident(pub SmolStr);

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// The kind of a Crane expression.
#[derive(Debug, Clone)]
pub enum ExprKind {
    /// A reference to a variable.
    Variable { name: Ident },

    /// A function call.
    Call {
        fun: Box<Expr>,
        args: ThinVec<Box<Expr>>,
    },
}

/// A Crane expression.
#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
}

/// The kind of a Crane statement.
#[derive(Debug, Clone)]
pub enum StmtKind {
    /// A function declaration (`fn`).
    Fn(Box<Fn>),

    /// An expression.
    Expr(Expr),
}

/// A function definition.
#[derive(Debug, Clone)]
pub struct Fn {
    pub name: SmolStr,
    pub body: ThinVec<Stmt>,
}

/// A Crane statement.
#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
}
