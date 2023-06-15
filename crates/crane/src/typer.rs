mod environment;
mod error;
mod r#type;

pub use error::*;
pub use r#type::*;
use thin_vec::ThinVec;

use std::collections::HashMap;
use std::sync::Arc;

use crate::ast::{
    Expr, ExprKind, Fn, Ident, Item, ItemKind, Literal, LiteralKind, Module, Span, Stmt, StmtKind,
    TyExpr, TyExprKind, TyFn, TyFnParam, TyIntegerLiteral, TyItem, TyItemKind, TyLiteral,
    TyLiteralKind, TyModule, TyStmt, TyStmtKind, TyUint,
};

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

    pub fn type_check_module(&mut self, module: Module) -> TypeCheckResult<TyModule> {
        // HACK: Register the functions from `std`.
        self.register_function(Ident("print".into()));
        self.register_function(Ident("println".into()));
        self.register_function(Ident("int_add".into()));

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
                        self.ensure_function_exists(fn_name, call_expr.span)?;
                    }
                }
            }
        }

        let mut typed_items = ThinVec::new();

        for item in module.items {
            typed_items.push(self.infer_item(item)?);
        }

        Ok(TyModule { items: typed_items })
    }

    fn register_function(&mut self, name: Ident) {
        self.module_functions.insert(name, ());
    }

    fn ensure_function_exists(&self, name: &Ident, span: Span) -> TypeCheckResult<()> {
        if let Some(_) = self.module_functions.get(name) {
            return Ok(());
        }

        Err(TypeError {
            kind: TypeErrorKind::UnknownFunction { name: name.clone() },
            span,
        })
    }

    fn infer_item(&self, item: Item) -> TypeCheckResult<TyItem> {
        match item.kind {
            ItemKind::Fn(fun) => Ok(TyItem {
                kind: TyItemKind::Fn(Box::new(self.infer_function(*fun)?)),
                name: item.name,
            }),
        }
    }

    fn infer_function(&self, fun: Fn) -> TypeCheckResult<TyFn> {
        Ok(TyFn {
            params: fun
                .params
                .iter()
                .map(|param| TyFnParam {
                    name: param.name.clone(),
                    ty: Arc::new(Type::UserDefined {
                        module: "std::prelude".into(),
                        name: param.ty.to_string().into(),
                    }),
                    span: param.span,
                })
                .collect::<ThinVec<_>>(),
            body: fun
                .body
                .into_iter()
                .map(|stmt| self.infer_stmt(stmt))
                .collect::<Result<ThinVec<_>, _>>()?,
        })
    }

    fn infer_stmt(&self, stmt: Stmt) -> TypeCheckResult<TyStmt> {
        Ok(TyStmt {
            kind: match stmt.kind {
                StmtKind::Expr(expr) => TyStmtKind::Expr(self.infer_expr(expr)?),
                StmtKind::Item(_) => todo!(),
            },
            span: stmt.span,
        })
    }

    fn infer_expr(&self, expr: Expr) -> TypeCheckResult<TyExpr> {
        match expr.kind {
            ExprKind::Literal(literal) => match literal.kind {
                LiteralKind::String => self.infer_string(literal, expr.span),
                LiteralKind::Integer => self.infer_integer(literal, expr.span),
            },
            ExprKind::Variable { name } => Ok(TyExpr {
                kind: TyExprKind::Variable { name },
                ty: Arc::new(Type::UserDefined {
                    module: "?".into(),
                    name: "?".into(),
                }),
                span: expr.span,
            }),
            ExprKind::Call { fun, args } => Ok(TyExpr {
                kind: TyExprKind::Call {
                    fun: Box::new(self.infer_expr(*fun)?),
                    args: args
                        .into_iter()
                        .map(|expr| self.infer_expr(*expr))
                        .map(|result| result.map(Box::new))
                        .collect::<Result<ThinVec<_>, _>>()?,
                },
                ty: Arc::new(Type::UserDefined {
                    module: "?".into(),
                    name: "?".into(),
                }),
                span: expr.span,
            }),
        }
    }

    fn infer_string(&self, literal: Literal, span: Span) -> TypeCheckResult<TyExpr> {
        Ok(TyExpr {
            kind: TyExprKind::Literal(TyLiteral {
                kind: TyLiteralKind::String(literal.value),
                span,
            }),
            span,
            ty: Arc::new(Type::UserDefined {
                module: "std::prelude".into(),
                name: "String".into(),
            }),
        })
    }

    fn infer_integer(&self, literal: Literal, span: Span) -> TypeCheckResult<TyExpr> {
        let value: u128 = literal.value.parse().expect("Failed to parse integer.");

        Ok(TyExpr {
            kind: TyExprKind::Literal(TyLiteral {
                kind: TyLiteralKind::Integer(TyIntegerLiteral::Unsigned(value, TyUint::Uint64)),
                span,
            }),
            span,
            ty: Arc::new(Type::UserDefined {
                module: "std::prelude".into(),
                name: "Uint64".into(),
            }),
        })
    }
}
