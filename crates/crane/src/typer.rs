mod environment;
mod error;
mod r#type;

pub use error::*;
pub use r#type::*;

use std::collections::HashMap;
use std::sync::Arc;

use thin_vec::ThinVec;

use crate::ast::visitor::{walk_expr, Visitor};
use crate::ast::{
    Expr, ExprKind, Fn, FnParam, Ident, Item, ItemKind, Literal, LiteralKind, Module, Span, Stmt,
    StmtKind, TyExpr, TyExprKind, TyFn, TyFnParam, TyIntegerLiteral, TyItem, TyItemKind, TyLiteral,
    TyLiteralKind, TyModule, TyStmt, TyStmtKind, TyUint, DUMMY_SPAN,
};

pub type TypeCheckResult<T> = Result<T, TypeError>;

pub struct Typer {
    module_functions: HashMap<Ident, ThinVec<TyFnParam>>,
}

impl Typer {
    pub fn new() -> Self {
        Self {
            module_functions: HashMap::new(),
        }
    }

    pub fn type_check_module(&mut self, module: Module) -> TypeCheckResult<TyModule> {
        // HACK: Register the functions from `std`.
        self.register_function(
            Ident {
                name: "print".into(),
                span: DUMMY_SPAN,
            },
            ThinVec::new(),
        );
        self.register_function(
            Ident {
                name: "println".into(),
                span: DUMMY_SPAN,
            },
            ThinVec::new(),
        );
        self.register_function(
            Ident {
                name: "int_add".into(),
                span: DUMMY_SPAN,
            },
            ThinVec::new(),
        );
        self.register_function(
            Ident {
                name: "int_to_string".into(),
                span: DUMMY_SPAN,
            },
            ThinVec::new(),
        );

        for item in &module.items {
            match item.kind {
                ItemKind::Fn(ref fun) => {
                    let params = self.infer_function_params(&fun.params)?;

                    self.register_function(item.name.clone(), params.clone());
                }
            }
        }

        let mut called_fns_collector = CalledFnsCollector::new();

        for item in &module.items {
            called_fns_collector.visit_item(item);
        }

        for called_fn in called_fns_collector.called_fns {
            self.ensure_function_exists(&called_fn)?;
        }

        let mut typed_items = ThinVec::new();

        for item in module.items {
            typed_items.push(self.infer_item(item)?);
        }

        Ok(TyModule { items: typed_items })
    }

    fn register_function(&mut self, name: Ident, params: ThinVec<TyFnParam>) {
        self.module_functions.insert(name, params);
    }

    fn ensure_function_exists(&self, ident: &Ident) -> TypeCheckResult<()> {
        if let Some(_) = self.module_functions.get(ident) {
            return Ok(());
        }

        Err(TypeError {
            kind: TypeErrorKind::UnknownFunction {
                name: ident.clone(),
            },
            span: ident.span,
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
            params: self.infer_function_params(&fun.params)?,
            body: fun
                .body
                .into_iter()
                .map(|stmt| self.infer_stmt(stmt))
                .collect::<Result<ThinVec<_>, _>>()?,
        })
    }

    fn infer_function_params(
        &self,
        params: &ThinVec<FnParam>,
    ) -> TypeCheckResult<ThinVec<TyFnParam>> {
        Ok(params
            .iter()
            .map(|param| TyFnParam {
                name: param.name.clone(),
                ty: Arc::new(Type::UserDefined {
                    module: "std::prelude".into(),
                    name: param.ty.to_string().into(),
                }),
                span: param.span,
            })
            .collect::<ThinVec<_>>())
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
            ExprKind::Call { fun, args } => {
                let callee = self.infer_expr(*fun.clone())?;

                let callee = match &callee.kind {
                    TyExprKind::Variable { name } => Ok(name),
                    _ => Err(TypeError {
                        kind: TypeErrorKind::Error(format!("Not a function.")),
                        span: callee.span,
                    }),
                }?;

                let callee_params =
                    self.module_functions
                        .get(&callee)
                        .ok_or_else(|| TypeError {
                            kind: TypeErrorKind::Error(format!("Function `{}` not found.", callee)),
                            span: callee.span,
                        })?;

                let caller_args = args
                    .into_iter()
                    .map(|expr| self.infer_expr(*expr))
                    .map(|result| result.map(Box::new))
                    .collect::<Result<ThinVec<_>, _>>()?;

                for (param, arg) in callee_params.into_iter().zip(&caller_args) {
                    if param.ty != arg.ty {
                        fn ty_to_string(ty: &Type) -> String {
                            match ty {
                                Type::UserDefined { module, name } => {
                                    format!("{}::{}", module, name)
                                }
                                Type::Fn { args, return_ty } => format!(
                                    "Fn({}) -> {}",
                                    args.iter()
                                        .map(|ty| ty_to_string(ty))
                                        .collect::<Vec<_>>()
                                        .join(", "),
                                    ty_to_string(return_ty)
                                ),
                            }
                        }

                        return Err(TypeError {
                            kind: TypeErrorKind::Error(format!(
                                "Expected `{}` but received `{}`",
                                ty_to_string(&param.ty),
                                ty_to_string(&arg.ty)
                            )),
                            span: arg.span,
                        });
                    }
                }

                Ok(TyExpr {
                    kind: TyExprKind::Call {
                        fun: Box::new(self.infer_expr(*fun)?),
                        args: caller_args,
                    },
                    ty: Arc::new(Type::UserDefined {
                        module: "?".into(),
                        name: "?".into(),
                    }),
                    span: expr.span,
                })
            }
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

struct IdentCollector {
    idents: Vec<Ident>,
}

impl IdentCollector {
    pub fn new() -> Self {
        Self { idents: Vec::new() }
    }
}

impl Visitor for IdentCollector {
    fn visit_ident(&mut self, ident: &Ident) {
        self.idents.push(ident.clone());
    }
}

struct CalledFnsCollector {
    called_fns: Vec<Ident>,
}

impl CalledFnsCollector {
    pub fn new() -> Self {
        Self {
            called_fns: Vec::new(),
        }
    }
}

impl Visitor for CalledFnsCollector {
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Call { fun, .. } => {
                let mut ident_collector = IdentCollector::new();

                walk_expr(&mut ident_collector, fun);

                if let Some(ident) = ident_collector.idents.first() {
                    self.called_fns.push(ident.clone());
                }
            }
            _ => {}
        }

        walk_expr(self, expr);
    }
}
