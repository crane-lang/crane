use crate::ast::{Expr, ExprKind, Fn, FnParam, Ident, Item, ItemKind, Stmt, StmtKind};

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
            visitor.visit_fn(&fun);
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
