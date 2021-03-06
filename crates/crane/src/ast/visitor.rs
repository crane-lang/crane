use crate::ast::{
    Expr, ExprKind, FieldDecl, Fn, FnDecl, FnParam, FnReturnTy, Ident, Item, ItemKind, Local,
    ModuleDecl, Path, PathSegment, Stmt, StmtKind, StructDecl, StructExprField, Ty, UnionDecl,
    UseTree, UseTreeKind, Variant, VariantData,
};

pub trait Visitor: Sized {
    fn visit_ident(&mut self, _ident: &Ident) {}

    fn visit_ty(&mut self, _ty: &Ty) {}

    fn visit_item(&mut self, item: &Item) {
        walk_item(self, item);
    }

    fn visit_use_tree(&mut self, use_tree: &UseTree) {
        walk_use_tree(self, use_tree);
    }

    fn visit_path(&mut self, path: &Path) {
        walk_path(self, path);
    }

    fn visit_path_segment(&mut self, path_segment: &PathSegment) {
        walk_path_segment(self, path_segment);
    }

    fn visit_fn(&mut self, fun: &Fn) {
        walk_fn(self, fun);
    }

    fn visit_fn_decl(&mut self, fun_decl: &FnDecl) {
        walk_fn_decl(self, fun_decl);
    }

    fn visit_fn_param(&mut self, param: &FnParam) {
        walk_fn_param(self, param);
    }

    fn visit_fn_return_ty(&mut self, return_ty: &FnReturnTy) {
        walk_fn_return_ty(self, return_ty);
    }

    fn visit_struct_decl(&mut self, struct_decl: &StructDecl) {
        walk_struct_decl(self, struct_decl);
    }

    fn visit_union_decl(&mut self, union_decl: &UnionDecl) {
        walk_union_decl(self, union_decl);
    }

    fn visit_module_decl(&mut self, module_decl: &ModuleDecl) {
        walk_module_decl(self, module_decl);
    }

    fn visit_variant(&mut self, variant: &Variant) {
        walk_variant(self, variant);
    }

    fn visit_variant_data(&mut self, variant_data: &VariantData) {
        walk_variant_data(self, variant_data);
    }

    fn visit_field_decl(&mut self, field_decl: &FieldDecl) {
        walk_field_decl(self, field_decl);
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        walk_stmt(self, stmt);
    }

    fn visit_local(&mut self, local: &Local) {
        walk_local(self, local);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr);
    }

    fn visit_struct_expr_field(&mut self, field: &StructExprField) {
        walk_struct_expr_field(self, field);
    }
}

pub fn walk_item<V: Visitor>(visitor: &mut V, item: &Item) {
    visitor.visit_ident(&item.name);

    match &item.kind {
        ItemKind::Use(use_tree) => {
            visitor.visit_use_tree(use_tree);
        }
        ItemKind::Fn(fun) => {
            visitor.visit_fn(fun);
        }
        ItemKind::Struct(struct_decl) => {
            visitor.visit_struct_decl(struct_decl);
        }
        ItemKind::Union(union_decl) => {
            visitor.visit_union_decl(union_decl);
        }
        ItemKind::Module(module_decl) => {
            visitor.visit_module_decl(module_decl);
        }
    }
}

pub fn walk_use_tree<V: Visitor>(visitor: &mut V, use_tree: &UseTree) {
    visitor.visit_path(&use_tree.prefix);

    match &use_tree.kind {
        UseTreeKind::Single => {}
    }
}

pub fn walk_path<V: Visitor>(visitor: &mut V, path: &Path) {
    for segment in &path.segments {
        visitor.visit_path_segment(segment);
    }
}

pub fn walk_path_segment<V: Visitor>(visitor: &mut V, path_segment: &PathSegment) {
    visitor.visit_ident(&path_segment.ident);
}

pub fn walk_fn<V: Visitor>(visitor: &mut V, fun: &Fn) {
    visitor.visit_fn_decl(&fun.decl);

    for stmt in &fun.body {
        visitor.visit_stmt(stmt);
    }
}

pub fn walk_fn_decl<V: Visitor>(visitor: &mut V, fun_decl: &FnDecl) {
    for param in &fun_decl.params {
        visitor.visit_fn_param(param);
    }

    visitor.visit_fn_return_ty(&fun_decl.return_ty);
}

pub fn walk_fn_param<V: Visitor>(visitor: &mut V, param: &FnParam) {
    visitor.visit_ident(&param.name);
    visitor.visit_ty(&param.ty);
}

pub fn walk_fn_return_ty<V: Visitor>(visitor: &mut V, return_ty: &FnReturnTy) {
    match &return_ty {
        FnReturnTy::Ty(ty) => {
            visitor.visit_ty(ty);
        }
        FnReturnTy::Unit => {}
    }
}

pub fn walk_struct_decl<V: Visitor>(visitor: &mut V, struct_decl: &StructDecl) {
    visitor.visit_variant_data(&struct_decl.0);
}

pub fn walk_union_decl<V: Visitor>(visitor: &mut V, union_decl: &UnionDecl) {
    for variant in &union_decl.variants {
        visitor.visit_variant(variant);
    }
}

pub fn walk_module_decl<V: Visitor>(visitor: &mut V, module_decl: &ModuleDecl) {
    match &module_decl {
        ModuleDecl::Loaded(module, _) => {
            for item in &module.items {
                visitor.visit_item(item);
            }
        }
        ModuleDecl::Unloaded => {}
    }
}

pub fn walk_variant<V: Visitor>(visitor: &mut V, variant: &Variant) {
    visitor.visit_ident(&variant.name);
    visitor.visit_variant_data(&variant.data);
}

pub fn walk_variant_data<V: Visitor>(visitor: &mut V, variant_data: &VariantData) {
    for field in variant_data.fields() {
        visitor.visit_field_decl(field);
    }
}

pub fn walk_field_decl<V: Visitor>(visitor: &mut V, field: &FieldDecl) {
    if let Some(name) = &field.name {
        visitor.visit_ident(name);
    }

    visitor.visit_ty(&field.ty);
}

pub fn walk_stmt<V: Visitor>(visitor: &mut V, stmt: &Stmt) {
    match &stmt.kind {
        StmtKind::Local(local) => visitor.visit_local(local),
        StmtKind::Item(item) => visitor.visit_item(item),
        StmtKind::Expr(expr) => visitor.visit_expr(expr),
    }
}

pub fn walk_local<V: Visitor>(visitor: &mut V, local: &Local) {
    visitor.visit_ident(&local.name);

    if let Some(ty) = &local.ty {
        visitor.visit_ty(ty);
    }
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &Expr) {
    match &expr.kind {
        ExprKind::Literal(_) => {}
        ExprKind::Variable(path) => visitor.visit_path(path),
        ExprKind::Call { fun, args } => {
            visitor.visit_expr(fun);

            for arg in args {
                visitor.visit_expr(arg);
            }
        }
        ExprKind::Struct(struct_expr) => {
            visitor.visit_path(&struct_expr.path);

            for field in &struct_expr.fields {
                visitor.visit_struct_expr_field(&field);
            }
        }
    }
}

pub fn walk_struct_expr_field<V: Visitor>(visitor: &mut V, field: &StructExprField) {
    visitor.visit_expr(&field.expr);
    visitor.visit_ident(&field.name);
}
