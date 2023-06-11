use crate::ast::{Expr, ExprKind, Item, ItemKind, Stmt, StmtKind};

pub struct JsBackend {}

impl JsBackend {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, program: Vec<Item>) -> String {
        let inline_std = r#"
function print(value) {
    process.stdout.write(value);
}

function println(value) {
    console.log(value);
}
        "#;

        let compiled_program = program
            .into_iter()
            .flat_map(|item| self.compile_item(item))
            .collect::<Vec<_>>()
            .join("\n");

        format!(
            r#"
// Begin Crane standard library.
{inline_std}
// End Crane standard library.

{compiled_program}

// Program entrypoint.
main();
        "#
        )
    }

    fn compile_item(&self, item: Item) -> Vec<String> {
        match item.kind {
            ItemKind::Fn(fun) => vec![format!(
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
        }
    }

    fn compile_stmt(&self, stmt: Stmt) -> Vec<String> {
        match stmt.kind {
            StmtKind::Item(item) => self.compile_item(item),
            StmtKind::Expr(expr) => self.compile_expr(expr),
        }
    }

    fn compile_expr(&self, expr: Expr) -> Vec<String> {
        match expr.kind {
            ExprKind::Literal(literal) => vec![literal.to_string()],
            ExprKind::Variable { name } => vec![name.0.to_string()],
            ExprKind::Call { fun, args } => vec![match fun.kind {
                ExprKind::Variable { name } => format!(
                    "{name}({arg_list})",
                    arg_list = args
                        .into_iter()
                        .map(|arg| self.compile_expr(*arg).join(""))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                _ => todo!(),
            }],
        }
    }
}
