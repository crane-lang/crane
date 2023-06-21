mod environment;
mod error;
mod r#type;

pub use error::*;
pub use r#type::*;

use std::collections::HashMap;
use std::sync::Arc;

use heck::ToSnakeCase;
use smol_str::SmolStr;
use thin_vec::{thin_vec, ThinVec};

use crate::ast::{
    Expr, ExprKind, Fn, FnDecl, FnParam, FnReturnTy, Ident, InlineModuleDecl, Item, ItemKind,
    Literal, LiteralKind, Local, LocalKind, Module, ModuleDecl, Package, PathSegment, Span, Stmt,
    StmtKind, StructDecl, Ty, TyExpr, TyExprKind, TyFieldDecl, TyFn, TyFnParam, TyIntegerLiteral,
    TyItem, TyItemKind, TyKind, TyLiteral, TyLiteralKind, TyLocal, TyLocalKind, TyModule,
    TyPackage, TyPath, TyPathSegment, TyStmt, TyStmtKind, TyStructDecl, TyUint, TyUnionDecl,
    TyVariant, TyVariantData, UnionDecl, UseTree, UseTreeKind, VariantData, DUMMY_SPAN,
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

type ModuleFunctions = HashMap<Ident, (ThinVec<TyFnParam>, Arc<Type>)>;

pub struct Typer {
    modules: HashMap<TyPath, ModuleFunctions>,
    use_map: HashMap<TyPath, TyPath>,
    scopes: Vec<HashMap<TyPath, Arc<Type>>>,

    // Types.
    unit_ty: Arc<Type>,
    string_ty: Arc<Type>,
    uint64_ty: Arc<Type>,
}

impl Typer {
    pub fn new() -> Self {
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

        Self {
            modules: HashMap::new(),
            use_map: HashMap::new(),
            scopes: Vec::new(),
            unit_ty,
            string_ty,
            uint64_ty,
        }
    }

    pub fn type_check_package(&mut self, package: Package) -> TypeCheckResult<TyPackage> {
        // HACK: Register the functions from `std`.
        self.register_std()?;

        self.perform_function_registration_pass(&package)?;

        let mut typed_modules = ThinVec::new();

        for module in package.modules {
            typed_modules.push(self.type_check_module(None, module)?);
        }

        Ok(TyPackage {
            modules: typed_modules,
        })
    }

    fn register_function(
        &mut self,
        module_path: TyPath,
        name: Ident,
        params: ThinVec<TyFnParam>,
        return_ty: Arc<Type>,
    ) -> TypeCheckResult<()> {
        if name.name != name.name.to_snake_case() {
            return Err(TypeError {
                kind: TypeErrorKind::InvalidFunctionName {
                    reason: "Function names must be written in snake_case.".to_string(),
                    suggestion: name.name.to_snake_case().into(),
                },
                span: name.span,
            })?;
        }

        let module = self.modules.entry(module_path).or_default();
        module.insert(name, (params, return_ty));

        Ok(())
    }

    fn ensure_function_exists(
        &self,
        path: &TyPath,
    ) -> TypeCheckResult<(&ThinVec<TyFnParam>, Arc<Type>)> {
        let (TyPathSegment { ident: name }, module_path_segments) =
            path.segments.split_last().unwrap();

        let module_path = TyPath {
            segments: module_path_segments.into(),
            span: path.span,
        };

        let module = self.modules.get(&module_path).ok_or_else(|| TypeError {
            kind: TypeErrorKind::UnknownModule {
                path: module_path,
                options: self.modules.keys().cloned().collect::<ThinVec<_>>(),
            },
            span: path.span,
        })?;

        if let Some((params, return_ty)) = module.get(name) {
            return Ok((params, return_ty.clone()));
        }

        Err(TypeError {
            kind: TypeErrorKind::UnknownFunction {
                path: path.clone(),
                options: module
                    .keys()
                    .map(|name| TyPath {
                        segments: thin_vec![TyPathSegment {
                            ident: name.clone()
                        }],
                        span: name.span,
                    })
                    .collect::<ThinVec<_>>(),
            },
            span: path.span,
        })
    }

    fn register_std(&mut self) -> TypeCheckResult<()> {
        let std_io_path = TyPath {
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
            ],
            span: DUMMY_SPAN,
        };

        let std_int_path = TyPath {
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
            ],
            span: DUMMY_SPAN,
        };

        self.register_function(
            std_io_path.clone(),
            Ident {
                name: "print".into(),
                span: DUMMY_SPAN,
            },
            thin_vec![TyFnParam {
                name: Ident {
                    name: "value".into(),
                    span: DUMMY_SPAN
                },
                ty: self.string_ty.clone(),
                span: DUMMY_SPAN
            }],
            self.unit_ty.clone(),
        )?;
        self.register_function(
            std_io_path,
            Ident {
                name: "println".into(),
                span: DUMMY_SPAN,
            },
            thin_vec![TyFnParam {
                name: Ident {
                    name: "value".into(),
                    span: DUMMY_SPAN
                },
                ty: self.string_ty.clone(),
                span: DUMMY_SPAN
            }],
            self.unit_ty.clone(),
        )?;
        self.register_function(
            std_int_path.clone(),
            Ident {
                name: "int_add".into(),
                span: DUMMY_SPAN,
            },
            thin_vec![
                TyFnParam {
                    name: Ident {
                        name: "a".into(),
                        span: DUMMY_SPAN
                    },
                    ty: self.uint64_ty.clone(),
                    span: DUMMY_SPAN
                },
                TyFnParam {
                    name: Ident {
                        name: "b".into(),
                        span: DUMMY_SPAN
                    },
                    ty: self.uint64_ty.clone(),
                    span: DUMMY_SPAN
                }
            ],
            self.uint64_ty.clone(),
        )?;
        self.register_function(
            std_int_path,
            Ident {
                name: "int_to_string".into(),
                span: DUMMY_SPAN,
            },
            thin_vec![TyFnParam {
                name: Ident {
                    name: "value".into(),
                    span: DUMMY_SPAN
                },
                ty: self.uint64_ty.clone(),
                span: DUMMY_SPAN
            }],
            self.string_ty.clone(),
        )?;

        Ok(())
    }

    fn perform_function_registration_pass(&mut self, package: &Package) -> TypeCheckResult<()> {
        for module in &package.modules {
            self.register_functions_in_module(None, module)?;
        }

        Ok(())
    }

    fn register_functions_in_module(
        &mut self,
        prefix: Option<&ThinVec<TyPathSegment>>,
        module: &Module,
    ) -> TypeCheckResult<()> {
        for item in &module.items {
            match item.kind {
                ItemKind::Use(_) => {}
                ItemKind::Fn(ref fun) => {
                    let (typed_params, return_ty) = self.infer_function_decl(&fun.decl)?;

                    let path_segments = prefix.cloned().unwrap_or(ThinVec::new());

                    let module_path = TyPath {
                        segments: path_segments,
                        span: DUMMY_SPAN,
                    };

                    self.register_function(
                        module_path,
                        item.name.clone(),
                        typed_params,
                        return_ty,
                    )?;
                }
                ItemKind::Struct(_) => {}
                ItemKind::Union(_) => {}
                ItemKind::Module(ref module_decl) => match *module_decl.clone() {
                    ModuleDecl::Loaded(module, InlineModuleDecl::Yes) => {
                        let mut path_segments = prefix.cloned().unwrap_or(ThinVec::new());
                        path_segments.push(TyPathSegment {
                            ident: item.name.clone(),
                        });

                        self.register_functions_in_module(Some(&path_segments), &module)?;
                    }
                    ModuleDecl::Loaded(_, InlineModuleDecl::No) => {}
                    ModuleDecl::Unloaded => {}
                },
            }
        }

        Ok(())
    }

    fn type_check_module(
        &mut self,
        prefix: Option<&ThinVec<TyPathSegment>>,
        module: Module,
    ) -> TypeCheckResult<TyModule> {
        for item in &module.items {
            match item.kind {
                ItemKind::Use(ref use_tree) => {
                    self.infer_use_tree(use_tree)?;
                }
                ItemKind::Fn(ref fun) => {
                    let mut path_segments = prefix.cloned().unwrap_or(ThinVec::new());
                    path_segments.push(TyPathSegment {
                        ident: item.name.clone(),
                    });

                    let path = TyPath {
                        segments: path_segments,
                        span: item.name.span,
                    };

                    self.infer_function(&path, *fun.clone())?;
                }
                ItemKind::Struct(_) => {}
                ItemKind::Union(_) => {}
                ItemKind::Module(ref module_decl) => match *module_decl.clone() {
                    ModuleDecl::Loaded(module, InlineModuleDecl::Yes) => {
                        let mut path_segments = prefix.cloned().unwrap_or(ThinVec::new());
                        path_segments.push(TyPathSegment {
                            ident: item.name.clone(),
                        });

                        self.type_check_module(Some(&path_segments), module.clone())?;
                    }
                    ModuleDecl::Loaded(_, InlineModuleDecl::No) => todo!(),
                    ModuleDecl::Unloaded => todo!(),
                },
            }
        }

        let mut typed_items = ThinVec::new();

        for item in module.items {
            typed_items.push(self.infer_item(prefix, item)?);
        }

        Ok(TyModule { items: typed_items })
    }

    fn infer_ty(&mut self, ty: Ty) -> TypeCheckResult<Arc<Type>> {
        Ok(match ty.kind {
            TyKind::Path(path) => {
                let (PathSegment { ident }, _) = path.segments.split_last().unwrap();

                Arc::new(Type::UserDefined {
                    module: "std::prelude".into(),
                    name: ident.to_string().into(),
                })
            }
            TyKind::Fn(fn_ty) => {
                let (params, return_ty) = self.infer_function_decl(&fn_ty.decl)?;

                Arc::new(Type::Fn {
                    args: params.iter().map(|param| param.ty.clone()).collect(),
                    return_ty,
                })
            }
        })
    }

    fn infer_item(
        &mut self,
        prefix: Option<&ThinVec<TyPathSegment>>,
        item: Item,
    ) -> TypeCheckResult<TyItem> {
        match item.kind {
            ItemKind::Use(use_tree) => {
                self.infer_use_tree(&use_tree)?;

                Ok(TyItem {
                    kind: TyItemKind::Use,
                    name: item.name,
                })
            }
            ItemKind::Fn(fun) => {
                let mut path_segments = prefix.cloned().unwrap_or(ThinVec::new());
                path_segments.push(TyPathSegment {
                    ident: item.name.clone(),
                });

                let path = TyPath {
                    segments: path_segments,
                    span: item.name.span,
                };

                Ok(TyItem {
                    kind: TyItemKind::Fn(Box::new(self.infer_function(&path, *fun)?)),
                    name: item.name,
                })
            }
            ItemKind::Struct(struct_decl) => Ok(TyItem {
                kind: TyItemKind::Struct(self.infer_struct_decl(&struct_decl)?),
                name: item.name,
            }),
            ItemKind::Union(union_decl) => Ok(TyItem {
                kind: TyItemKind::Union(self.infer_union_decl(&union_decl)?),
                name: item.name,
            }),
            ItemKind::Module(module_decl) => {
                let mut path_segments = prefix.cloned().unwrap_or(ThinVec::new());
                path_segments.push(TyPathSegment {
                    ident: item.name.clone(),
                });

                Ok(TyItem {
                    kind: TyItemKind::Module(
                        self.infer_module_decl(Some(&path_segments), &module_decl)?,
                    ),
                    name: item.name,
                })
            }
        }
    }

    fn infer_use_tree(&mut self, use_tree: &UseTree) -> TypeCheckResult<()> {
        match use_tree.kind {
            UseTreeKind::Single => {
                let (PathSegment { ident }, module_path_segments) =
                    use_tree.prefix.segments.split_last().unwrap();

                let module_path = TyPath {
                    segments: module_path_segments
                        .iter()
                        .map(|segment| TyPathSegment {
                            ident: segment.ident.clone(),
                        })
                        .collect::<ThinVec<_>>(),
                    span: DUMMY_SPAN,
                };

                let mut fn_path_segments = module_path.segments.clone();
                fn_path_segments.push(TyPathSegment {
                    ident: ident.clone(),
                });

                let fn_path = TyPath {
                    segments: fn_path_segments,
                    span: ident.span,
                };

                self.ensure_function_exists(&fn_path)?;

                let imported_path = TyPath {
                    segments: thin_vec![TyPathSegment {
                        ident: ident.clone()
                    }],
                    span: ident.span,
                };

                self.use_map.insert(imported_path, fn_path);
            }
        }

        Ok(())
    }

    fn infer_function(&mut self, path: &TyPath, fun: Fn) -> TypeCheckResult<TyFn> {
        let (_, return_ty) = self.ensure_function_exists(&path)?;

        let params = self.infer_function_params(&fun.decl.params)?;

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
                        "Expected `{path}` to return {} but got {}",
                        ty_to_string(&return_ty),
                        ty_to_string(&ty)
                    )),
                    span: last_stmt.span,
                });
            }
        }

        let ty_fn = TyFn {
            path: path.clone(),
            params,
            return_ty,
            body,
        };

        self.scopes.pop();

        Ok(ty_fn)
    }

    fn infer_function_decl(
        &mut self,
        function_decl: &FnDecl,
    ) -> TypeCheckResult<(ThinVec<TyFnParam>, Arc<Type>)> {
        let params = self.infer_function_params(&function_decl.params)?;

        let return_ty = match function_decl.return_ty {
            FnReturnTy::Unit => self.unit_ty.clone(),
            FnReturnTy::Ty(ref ty) => self.infer_ty(*ty.clone())?,
        };

        Ok((params, return_ty))
    }

    fn infer_function_params(
        &mut self,
        params: &ThinVec<FnParam>,
    ) -> TypeCheckResult<ThinVec<TyFnParam>> {
        params
            .iter()
            .map(|param| {
                Ok(TyFnParam {
                    name: param.name.clone(),
                    ty: self.infer_ty(*param.ty.clone())?,
                    span: param.span,
                })
            })
            .collect::<Result<ThinVec<_>, _>>()
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

    fn infer_module_decl(
        &mut self,
        prefix: Option<&ThinVec<TyPathSegment>>,
        module_decl: &ModuleDecl,
    ) -> TypeCheckResult<TyModule> {
        match &module_decl {
            ModuleDecl::Loaded(module, InlineModuleDecl::Yes) => Ok(TyModule {
                items: module
                    .items
                    .iter()
                    .map(|item| self.infer_item(prefix, item.clone()))
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
                        ty: match &field.ty.kind {
                            TyKind::Path(path) => {
                                let (PathSegment { ident }, _) =
                                    path.segments.split_last().unwrap();

                                ident.clone()
                            }
                            TyKind::Fn(fn_ty) => {
                                todo!()
                            }
                        },
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

                // Check the variable's path against the items brought into scope by `use`.
                // If we find an item that's been brought into scope we can use that as the alias.
                let path = if let Some(use_path) = self.use_map.get(&path) {
                    use_path.clone()
                } else {
                    path
                };

                let ty = self
                    .scopes
                    .last()
                    .and_then(|scope| scope.get(&path).cloned());

                let ty = ty.or_else(|| {
                    let function = self.ensure_function_exists(&path).ok();

                    function.map(|(params, return_ty)| {
                        Arc::new(Type::Fn {
                            args: params.iter().map(|param| param.ty.clone()).collect(),
                            return_ty,
                        })
                    })
                });

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

                let callee_path = match &callee.kind {
                    TyExprKind::Variable(path) => Ok(path),
                    _ => Err(TypeError {
                        kind: TypeErrorKind::Error("Not a function.".to_string()),
                        span: callee.span,
                    }),
                }?;

                // Check the callee's path against the items brought into scope by `use`.
                // If we find an item that's been brought into scope we can use that as the alias.
                let callee_path = if let Some(use_path) = self.use_map.get(&callee_path) {
                    use_path
                } else {
                    callee_path
                };

                let callee_from_params = self
                    .scopes
                    .last()
                    .and_then(|scope| scope.get(&callee_path))
                    .and_then(|ty| match &*ty.clone() {
                        Type::Fn { args, return_ty } => Some((
                            args.iter()
                                .map(|ty| TyFnParam {
                                    name: Ident {
                                        name: "".into(),
                                        span: DUMMY_SPAN,
                                    },
                                    ty: ty.clone(),
                                    span: DUMMY_SPAN,
                                })
                                .collect::<ThinVec<_>>(),
                            return_ty.clone(),
                        )),
                        _ => None,
                    });

                let (callee_params, callee_return_ty) = if let Some(callee) = callee_from_params {
                    callee
                } else {
                    self.ensure_function_exists(callee_path)
                        .map(|(params, return_ty)| (params.clone(), return_ty))?
                };

                let caller_args = args
                    .into_iter()
                    .map(|expr| self.infer_expr(*expr))
                    .map(|result| result.map(Box::new))
                    .collect::<Result<ThinVec<_>, _>>()?;

                let callee_arity = callee_params.len();
                let caller_arity = caller_args.len();

                if callee_arity != caller_arity {
                    return Err(TypeError {
                        kind: TypeErrorKind::Error(format!("`{callee_path}` was called with {caller_arity} arguments when it expected {callee_arity}")),
                        span: callee_path.span
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
                        fun: Box::new(TyExpr {
                            kind: TyExprKind::Variable(callee_path.clone()),
                            ty: callee.ty,
                            span: callee.span,
                        }),
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
            ty: self.string_ty.clone(),
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
            ty: self.uint64_ty.clone(),
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

            let package = Package {
                modules: thin_vec![module],
            };

            let mut typer = Typer::new();

            insta::assert_yaml_snapshot!(typer.type_check_package(package));
        })
    }
}
