use crate::ast::{
    Expr, ExprKind, FieldDecl, Fn, FnParam, Ident, Item, ItemKind, Stmt, StmtKind, StructDecl,
    UnionDecl, Variant, VariantData,
};

pub trait Visitor: Sized {
    fn visit_ident(&mut self, _ident: &Ident) {}

    fn visit_ty(&mut self, _ty: &Ident) {}

    fn visit_item(&mut self, item: &Item) {
        walk_item(self, item);
    }

    fn visit_fn(&mut self, fun: &Fn) {
        walk_fn(self, fun);
    }

    fn visit_fn_param(&mut self, param: &FnParam) {
        walk_fn_param(self, param);
    }

    fn visit_struct_decl(&mut self, struct_decl: &StructDecl) {
        walk_struct_decl(self, struct_decl);
    }

    fn visit_union_decl(&mut self, union_decl: &UnionDecl) {
        walk_union_decl(self, union_decl);
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

    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr);
    }
}

pub fn walk_item<V: Visitor>(visitor: &mut V, item: &Item) {
    visitor.visit_ident(&item.name);

    match &item.kind {
        ItemKind::Fn(fun) => {
            visitor.visit_fn(fun);
        }
        ItemKind::Struct(struct_decl) => {
            visitor.visit_struct_decl(struct_decl);
        }
        ItemKind::Union(union_decl) => {
            visitor.visit_union_decl(union_decl);
        }
    }
}

pub fn walk_fn<V: Visitor>(visitor: &mut V, fun: &Fn) {
    for param in &fun.params {
        visitor.visit_fn_param(param);
    }

    for stmt in &fun.body {
        visitor.visit_stmt(stmt);
    }
}

pub fn walk_fn_param<V: Visitor>(visitor: &mut V, param: &FnParam) {
    visitor.visit_ident(&param.name);
    visitor.visit_ty(&param.ty);
}

pub fn walk_struct_decl<V: Visitor>(visitor: &mut V, struct_decl: &StructDecl) {
    visitor.visit_variant_data(&struct_decl.0);
}

pub fn walk_union_decl<V: Visitor>(visitor: &mut V, union_decl: &UnionDecl) {
    for variant in &union_decl.variants {
        visitor.visit_variant(variant);
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
        StmtKind::Item(item) => visitor.visit_item(item),
        StmtKind::Expr(expr) => visitor.visit_expr(expr),
    }
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &Expr) {
    match &expr.kind {
        ExprKind::Literal(_) => {}
        ExprKind::Variable { name } => visitor.visit_ident(name),
        ExprKind::Call { fun, args } => {
            visitor.visit_expr(fun);

            for arg in args {
                visitor.visit_expr(arg);
            }
        }
    }
}
