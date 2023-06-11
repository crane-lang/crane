use thin_vec::ThinVec;

/// The kind of a Crane expression.
#[derive(Debug, Clone)]
pub enum ExprKind {
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
}

/// A function definition.
#[derive(Debug, Clone)]
pub struct Fn {}

/// A Crane statement.
#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
}
