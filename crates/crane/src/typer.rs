mod environment;
mod error;
mod r#type;

pub use error::*;
pub use r#type::*;

use std::collections::HashMap;
use std::sync::Arc;

use smol_str::SmolStr;
use thin_vec::{thin_vec, ThinVec};

use crate::ast::{
    Expr, ExprKind, Fn, FnParam, Ident, InlineModuleDecl, Item, ItemKind, Literal, LiteralKind,
    Local, LocalKind, Module, ModuleDecl, Package, Span, Stmt, StmtKind, StructDecl, TyExpr,
    TyExprKind, TyFieldDecl, TyFn, TyFnParam, TyIntegerLiteral, TyItem, TyItemKind, TyLiteral,
    TyLiteralKind, TyLocal, TyLocalKind, TyModule, TyPackage, TyPath, TyPathSegment, TyStmt,
    TyStmtKind, TyStructDecl, TyUint, TyUnionDecl, TyVariant, TyVariantData, UnionDecl,
    VariantData, DUMMY_SPAN,
};

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

pub type TypeCheckResult<T> = Result<T, TypeError>;

pub struct Typer {
    module_functions: HashMap<TyPath, (ThinVec<TyFnParam>, Arc<Type>)>,
    scopes: Vec<HashMap<TyPath, Arc<Type>>>,
}

impl Typer {
    pub fn new() -> Self {
        Self {
            module_functions: HashMap::new(),
            scopes: Vec::new(),
        }
    }

    pub fn type_check_package(&mut self, package: Package) -> TypeCheckResult<TyPackage> {
        let mut typed_modules = ThinVec::new();

        for module in package.modules {
            typed_modules.push(self.type_check_module(module)?);
        }

        Ok(TyPackage {
            modules: typed_modules,
        })
    }

    pub fn type_check_module(&mut self, module: Module) -> TypeCheckResult<TyModule> {
        let unit_ty = Arc::new(Type::UserDefined {
            module: SmolStr::new_inline("std::prelude"),
            name: SmolStr::new_inline("()"),
        });

        let string_ty = Arc::new(Type::UserDefined {
            module: SmolStr::new_inline("std::prelude"),
            name: SmolStr::new_inline("String"),
        });

        let uint64_ty = Arc::new(Type::UserDefined {
            module: SmolStr::new_inline("std::prelude"),
            name: SmolStr::new_inline("Uint64"),
        });

        // HACK: Register the functions from `std`.
        self.register_function(
            TyPath {
                segments: thin_vec![
                    TyPathSegment {
                        ident: Ident {
                            name: "std".into(),
                            span: DUMMY_SPAN,
                        }
                    },
                    TyPathSegment {
                        ident: Ident {
                            name: "io".into(),
                            span: DUMMY_SPAN,
                        }
                    },
                    TyPathSegment {
                        ident: Ident {
                            name: "print".into(),
                            span: DUMMY_SPAN,
                        }
                    }
                ],
                span: DUMMY_SPAN,
            },
            thin_vec![TyFnParam {
                name: Ident {
                    name: "value".into(),
                    span: DUMMY_SPAN
                },
                ty: string_ty.clone(),
                span: DUMMY_SPAN
            }],
            unit_ty.clone(),
        );
        self.register_function(
            TyPath {
                segments: thin_vec![
                    TyPathSegment {
                        ident: Ident {
                            name: "std".into(),
                            span: DUMMY_SPAN,
                        }
                    },
                    TyPathSegment {
                        ident: Ident {
                            name: "io".into(),
                            span: DUMMY_SPAN,
                        }
                    },
                    TyPathSegment {
                        ident: Ident {
                            name: "println".into(),
                            span: DUMMY_SPAN,
                        }
                    }
                ],
                span: DUMMY_SPAN,
            },
            thin_vec![TyFnParam {
                name: Ident {
                    name: "value".into(),
                    span: DUMMY_SPAN
                },
                ty: string_ty.clone(),
                span: DUMMY_SPAN
            }],
            unit_ty.clone(),
        );
        self.register_function(
            TyPath {
                segments: thin_vec![
                    TyPathSegment {
                        ident: Ident {
                            name: "std".into(),
                            span: DUMMY_SPAN,
                        }
                    },
                    TyPathSegment {
                        ident: Ident {
                            name: "int".into(),
                            span: DUMMY_SPAN,
                        }
                    },
                    TyPathSegment {
                        ident: Ident {
                            name: "int_add".into(),
                            span: DUMMY_SPAN,
                        }
                    }
                ],
                span: DUMMY_SPAN,
            },
            thin_vec![
                TyFnParam {
                    name: Ident {
                        name: "a".into(),
                        span: DUMMY_SPAN
                    },
                    ty: uint64_ty.clone(),
                    span: DUMMY_SPAN
                },
                TyFnParam {
                    name: Ident {
                        name: "b".into(),
                        span: DUMMY_SPAN
                    },
                    ty: uint64_ty.clone(),
                    span: DUMMY_SPAN
                }
            ],
            uint64_ty.clone(),
        );
        self.register_function(
            TyPath {
                segments: thin_vec![
                    TyPathSegment {
                        ident: Ident {
                            name: "std".into(),
                            span: DUMMY_SPAN,
                        }
                    },
                    TyPathSegment {
                        ident: Ident {
                            name: "int".into(),
                            span: DUMMY_SPAN,
                        }
                    },
                    TyPathSegment {
                        ident: Ident {
                            name: "int_to_string".into(),
                            span: DUMMY_SPAN,
                        }
                    }
                ],
                span: DUMMY_SPAN,
            },
            thin_vec![TyFnParam {
                name: Ident {
                    name: "value".into(),
                    span: DUMMY_SPAN
                },
                ty: uint64_ty,
                span: DUMMY_SPAN
            }],
            string_ty,
        );

        for item in &module.items {
            match item.kind {
                ItemKind::Use(_) => {}
                ItemKind::Fn(ref fun) => {
                    let params = self.infer_function_params(&fun.params)?;

                    let return_ty = fun
                        .return_ty
                        .as_ref()
                        .map(|return_ty| {
                            Arc::new(Type::UserDefined {
                                module: "std::prelude".into(),
                                name: return_ty.to_string().into(),
                            })
                        })
                        .unwrap_or(unit_ty.clone());

                    self.register_function(
                        TyPath {
                            segments: thin_vec![TyPathSegment {
                                ident: item.name.clone()
                            }],
                            span: item.name.span,
                        },
                        params.clone(),
                        return_ty,
                    );
                }
                ItemKind::Struct(_) => {}
                ItemKind::Union(_) => {}
                ItemKind::Module(ref module_decl) => match *module_decl.clone() {
                    ModuleDecl::Loaded(module, InlineModuleDecl::Yes) => {
                        self.type_check_module(module.clone())?;
                    }
                    ModuleDecl::Loaded(_, InlineModuleDecl::No) => todo!(),
                    ModuleDecl::Unloaded => todo!(),
                },
            }
        }

        let mut typed_items = ThinVec::new();

        for item in module.items {
            typed_items.push(self.infer_item(item)?);
        }

        Ok(TyModule { items: typed_items })
    }

    fn register_function(
        &mut self,
        path: TyPath,
        params: ThinVec<TyFnParam>,
        return_ty: Arc<Type>,
    ) {
        self.module_functions.insert(path, (params, return_ty));
    }

    fn ensure_function_exists(
        &self,
        path: &TyPath,
    ) -> TypeCheckResult<(&ThinVec<TyFnParam>, Arc<Type>)> {
        if let Some((params, return_ty)) = self.module_functions.get(path) {
            return Ok((params, return_ty.clone()));
        }

        Err(TypeError {
            kind: TypeErrorKind::UnknownFunction(path.clone()),
            span: path.span,
        })
    }

    fn infer_item(&mut self, item: Item) -> TypeCheckResult<TyItem> {
        match item.kind {
            ItemKind::Use(_) => Ok(TyItem {
                kind: TyItemKind::Use,
                name: item.name,
            }),
            ItemKind::Fn(fun) => Ok(TyItem {
                kind: TyItemKind::Fn(Box::new(self.infer_function(&item.name, *fun)?)),
                name: item.name,
            }),
            ItemKind::Struct(struct_decl) => Ok(TyItem {
                kind: TyItemKind::Struct(self.infer_struct_decl(&struct_decl)?),
                name: item.name,
            }),
            ItemKind::Union(union_decl) => Ok(TyItem {
                kind: TyItemKind::Union(self.infer_union_decl(&union_decl)?),
                name: item.name,
            }),
            ItemKind::Module(module_decl) => Ok(TyItem {
                kind: TyItemKind::Module(self.infer_module_decl(&module_decl)?),
                name: item.name,
            }),
        }
    }

    fn infer_function(&mut self, name: &Ident, fun: Fn) -> TypeCheckResult<TyFn> {
        let path = TyPath {
            segments: thin_vec![TyPathSegment {
                ident: name.clone()
            }],
            span: name.span,
        };

        let (_, return_ty) = self.ensure_function_exists(&path)?;

        let params = self.infer_function_params(&fun.params)?;

        self.scopes
            .push(HashMap::from_iter(params.clone().into_iter().map(
                |param| {
                    (
                        TyPath {
                            segments: thin_vec![TyPathSegment { ident: param.name }],
                            span: param.span,
                        },
                        param.ty,
                    )
                },
            )));

        let body = fun
            .body
            .into_iter()
            .map(|stmt| self.infer_stmt(stmt))
            .collect::<Result<ThinVec<_>, _>>()?;

        if let Some(last_stmt) = body.last() {
            let ty = match &last_stmt.kind {
                TyStmtKind::Local(_) => todo!(),
                TyStmtKind::Expr(expr) => &expr.ty,
                TyStmtKind::Item(_) => todo!(),
            };

            if *ty != return_ty {
                return Err(TypeError {
                    kind: TypeErrorKind::Error(format!(
                        "Expected `{name}` to return {} but got {}",
                        ty_to_string(&return_ty),
                        ty_to_string(&ty)
                    )),
                    span: last_stmt.span,
                });
            }
        }

        let ty_fn = TyFn {
            params,
            return_ty,
            body,
        };

        self.scopes.pop();

        Ok(ty_fn)
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

    fn infer_struct_decl(&self, struct_decl: &StructDecl) -> TypeCheckResult<TyStructDecl> {
        Ok(TyStructDecl(self.infer_variant_data(&struct_decl.0)?))
    }

    fn infer_union_decl(&self, union_decl: &UnionDecl) -> TypeCheckResult<TyUnionDecl> {
        let ty_variants = union_decl
            .variants
            .iter()
            .map(|variant| {
                Ok(TyVariant {
                    name: variant.name.clone(),
                    data: self.infer_variant_data(&variant.data)?,
                    span: variant.span,
                })
            })
            .collect::<Result<ThinVec<_>, _>>()?;

        Ok(TyUnionDecl {
            variants: ty_variants,
        })
    }

    fn infer_module_decl(&mut self, module_decl: &ModuleDecl) -> TypeCheckResult<TyModule> {
        match &module_decl {
            ModuleDecl::Loaded(module, InlineModuleDecl::Yes) => Ok(TyModule {
                items: module
                    .items
                    .iter()
                    .map(|item| self.infer_item(item.clone()))
                    .collect::<Result<ThinVec<_>, _>>()?,
            }),
            ModuleDecl::Loaded(_, InlineModuleDecl::No) => Err(TypeError {
                kind: TypeErrorKind::Error("Only inline modules are supported.".into()),
                span: DUMMY_SPAN,
            }),
            ModuleDecl::Unloaded => Err(TypeError {
                kind: TypeErrorKind::Error("Only inline modules are supported.".into()),
                span: DUMMY_SPAN,
            }),
        }
    }

    fn infer_variant_data(&self, variant_data: &VariantData) -> TypeCheckResult<TyVariantData> {
        Ok(match &variant_data {
            VariantData::Struct(fields) => TyVariantData::Struct(
                fields
                    .into_iter()
                    .map(|field| TyFieldDecl {
                        name: field.name.clone(),
                        ty: field.ty.clone(),
                        span: field.span,
                    })
                    .collect::<ThinVec<_>>(),
            ),
            VariantData::Tuple(_) => todo!(),
            VariantData::Unit => TyVariantData::Unit,
        })
    }

    fn infer_stmt(&mut self, stmt: Stmt) -> TypeCheckResult<TyStmt> {
        Ok(TyStmt {
            kind: match stmt.kind {
                StmtKind::Local(local) => TyStmtKind::Local(Box::new(self.infer_local(*local)?)),
                StmtKind::Expr(expr) => TyStmtKind::Expr(Box::new(self.infer_expr(*expr)?)),
                StmtKind::Item(_) => todo!(),
            },
            span: stmt.span,
        })
    }

    fn infer_local(&mut self, local: Local) -> TypeCheckResult<TyLocal> {
        let ty = match local.kind.init() {
            Some(init) => self.infer_expr(init.clone())?.ty,
            None => Arc::new(Type::UserDefined {
                module: "?".into(),
                name: "?".into(),
            }),
        };

        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                TyPath {
                    segments: thin_vec![TyPathSegment {
                        ident: local.name.clone()
                    }],
                    span: local.span,
                },
                ty.clone(),
            );
        }

        Ok(TyLocal {
            kind: match local.kind {
                LocalKind::Decl => TyLocalKind::Decl,
                LocalKind::Init(init) => TyLocalKind::Init(Box::new(self.infer_expr(*init)?)),
            },
            name: local.name,
            ty: Some(ty),
            span: local.span,
        })
    }

    fn infer_expr(&self, expr: Expr) -> TypeCheckResult<TyExpr> {
        match expr.kind {
            ExprKind::Literal(literal) => match literal.kind {
                LiteralKind::String => self.infer_string(literal, expr.span),
                LiteralKind::Integer => self.infer_integer(literal, expr.span),
            },
            ExprKind::Variable(path) => {
                let path = TyPath {
                    segments: path
                        .segments
                        .into_iter()
                        .map(|segment| TyPathSegment {
                            ident: segment.ident,
                        })
                        .collect::<ThinVec<_>>(),
                    span: path.span,
                };

                let ty = self
                    .scopes
                    .last()
                    .and_then(|scope| scope.get(&path).cloned());

                Ok(TyExpr {
                    kind: TyExprKind::Variable(path),
                    ty: ty.unwrap_or_else(|| {
                        Arc::new(Type::UserDefined {
                            module: "?".into(),
                            name: "?".into(),
                        })
                    }),
                    span: expr.span,
                })
            }
            ExprKind::Call { fun, args } => {
                let callee = self.infer_expr(*fun.clone())?;

                let callee = match &callee.kind {
                    TyExprKind::Variable(path) => Ok(path),
                    _ => Err(TypeError {
                        kind: TypeErrorKind::Error("Not a function.".to_string()),
                        span: callee.span,
                    }),
                }?;

                let (callee_params, callee_return_ty) =
                    self.module_functions.get(callee).ok_or_else(|| TypeError {
                        kind: TypeErrorKind::Error(format!("Function `{callee}` not found.",)),
                        span: callee.span,
                    })?;

                let caller_args = args
                    .into_iter()
                    .map(|expr| self.infer_expr(*expr))
                    .map(|result| result.map(Box::new))
                    .collect::<Result<ThinVec<_>, _>>()?;

                let callee_arity = callee_params.len();
                let caller_arity = caller_args.len();

                if callee_arity != caller_arity {
                    return Err(TypeError {
                        kind: TypeErrorKind::Error(format!("`{callee}` was called with {caller_arity} arguments when it expected {callee_arity}")),
                        span: callee.span
                    });
                }

                for (param, arg) in callee_params.into_iter().zip(&caller_args) {
                    if param.ty != arg.ty {
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
                    ty: callee_return_ty.clone(),
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

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    use super::*;

    #[test]
    pub fn test_typer() {
        insta::glob!("snapshot_inputs/*.crane", |path| {
            let source = std::fs::read_to_string(path).unwrap();

            let lexer = Lexer::new(&source);
            let parser = Parser::new(lexer);

            let items = parser.parse().unwrap();

            let module = Module { items };

            let mut typer = Typer::new();

            insta::assert_yaml_snapshot!(typer.type_check_module(module));
        })
    }
}
