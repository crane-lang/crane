use crate::ast::{Expr, ExprKind, Stmt, StmtKind};

pub struct JsBackend {}

impl JsBackend {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, program: Vec<Stmt>) -> String {
        program
            .into_iter()
            .flat_map(|stmt| self.compile_stmt(stmt))
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn compile_stmt(&self, stmt: Stmt) -> Vec<String> {
        match stmt.kind {
            StmtKind::Fn(fun) => vec![format!(
                r#"
                function {name}() {{
                    {body}
                }}
                "#,
                name = fun.name,
                body = fun
                    .body
                    .into_iter()
                    .map(|stmt| self.compile_stmt(stmt).join("\n"))
                    .collect::<Vec<_>>()
                    .join("\n")
            )],
            StmtKind::Expr(expr) => self.compile_expr(expr),
        }
    }

    fn compile_expr(&self, expr: Expr) -> Vec<String> {
        match expr.kind {
            ExprKind::Variable { name } => vec![name.0.to_string()],
            ExprKind::Call { fun, args } => vec![match fun.kind {
                ExprKind::Variable { name } => format!("{name}()"),
                ExprKind::Call { fun, args } => todo!(),
            }],
        }
    }
}
