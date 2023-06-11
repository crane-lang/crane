mod environment;
mod error;

pub use error::*;

use std::collections::HashMap;
use std::ops::Range;

use crate::ast::{ExprKind, Ident, ItemKind, Module, StmtKind};

pub type TypeCheckResult<T> = Result<T, TypeError>;

pub struct Typer {
    module_functions: HashMap<Ident, ()>,
}

impl Typer {
    pub fn new() -> Self {
        Self {
            module_functions: HashMap::new(),
        }
    }

    pub fn type_check_module(&mut self, module: Module) -> TypeCheckResult<()> {
        // HACK: Register the functions from `std::io`.
        self.register_function(Ident("print".into()));
        self.register_function(Ident("println".into()));

        for item in &module.items {
            match item.kind {
                ItemKind::Fn(_) => {
                    self.register_function(item.name.clone());
                }
            }
        }

        for item in &module.items {
            match &item.kind {
                ItemKind::Fn(fun) => {
                    for (fn_name, call_expr) in
                        fun.body.iter().filter_map(|stmt| match &stmt.kind {
                            StmtKind::Expr(expr) => match &expr.kind {
                                ExprKind::Call { fun, .. } => match &fun.kind {
                                    ExprKind::Variable { name } => Some((name, expr)),
                                    _ => None,
                                },
                                _ => None,
                            },
                            _ => None,
                        })
                    {
                        self.ensure_function_exists(fn_name, &call_expr.span)?;
                    }
                }
            }
        }

        Ok(())
    }

    fn register_function(&mut self, name: Ident) {
        self.module_functions.insert(name, ());
    }

    fn ensure_function_exists(&self, name: &Ident, span: &Range<usize>) -> TypeCheckResult<()> {
        if let Some(_) = self.module_functions.get(name) {
            return Ok(());
        }

        Err(TypeError {
            kind: TypeErrorKind::UnknownFunction { name: name.clone() },
            span: span.clone(),
        })
    }
}
