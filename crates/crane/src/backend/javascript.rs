use crate::ast::{Stmt, StmtKind};

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
            StmtKind::Fn(fun) => vec![format!(r#"function {name}() {{}}"#, name = fun.name)],
        }
    }
}
