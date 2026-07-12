use crate::ast;

pub trait Visitor: Sized {
    fn visit_expr(&mut self, expr: &ast::Expr) {
        walk_expr(self, &expr);
    }

    fn visit_constant(&mut self, _: &ast::Constant) {
        /* Nothing */
    }

    fn visit_identifier(&mut self, _: String) {
        /* Nothing */
    }

    fn visit_literal(&mut self, lit: &ast::StringLiteral) {
        if let ast::StringLiteral::StringLiteral(s) = lit.clone() {
            self.visit_identifier(s);
        }
    }

    fn visit_generic_selection(
        &mut self,
        base: &ast::Expr,
        associations: Vec<&ast::GenericAssociation>,
    ) {
        self.visit_expr(base);
        associations
            .iter()
            .for_each(|assoc| self.visit_generic_association(*assoc));
    }

    fn visit_generic_association(&mut self, assoc: &ast::GenericAssociation) {
        if let ast::AssociationTy::Ty(_ty) = &assoc.association_ty {
            // TODO
        }

        self.visit_expr(&assoc.expr);
    }

    fn visit_bin_op(&mut self, _: &ast::BinOpKind, l: &ast::Expr, r: &ast::Expr) {
        self.visit_expr(l);
        self.visit_expr(r);
    }

    fn visit_seq(&mut self, exprs: Vec<&ast::Expr>) {
        exprs.iter().for_each(|expr| self.visit_expr(*expr));
    }

    fn visit_unary_op(&mut self, _: &ast::UnaryOpKind, expr: &ast::Expr) {
        self.visit_expr(expr);
    }

    fn visit_ternary(&mut self, cond: &ast::Expr, branch: &ast::Expr, no_branch: &ast::Expr) {
        self.visit_expr(cond);
        self.visit_expr(branch);
        self.visit_expr(no_branch);
    }

    fn visit_index(&mut self, expr: &ast::Expr, index: &ast::Expr) {
        self.visit_expr(expr);
        self.visit_expr(index);
    }

    fn visit_call(&mut self, expr: &ast::Expr, args: Option<Vec<&ast::Expr>>) {
        self.visit_expr(expr);

        if let Some(args) = args {
            self.visit_seq(args);
        }
    }

    fn visit_access(&mut self, _: bool, expr: &ast::Expr, field: String) {
        self.visit_expr(expr);
        self.visit_identifier(field);
    }
}

fn walk_expr<V: Visitor>(v: &mut V, expr: &ast::Expr) {
    match expr {
        ast::Expr::Identifier(_, id) => v.visit_identifier(id.clone()),
        ast::Expr::Constant(_, constant) => v.visit_constant(constant),
        ast::Expr::Literal(_, literal) => v.visit_literal(literal),
        ast::Expr::GenericSelection(_, base, associations) => v.visit_generic_selection(
            &base,
            associations.iter().map(|assoc| &**assoc).collect::<Vec<_>>(),
        ),
        ast::Expr::Seq(_, exprs) => {
            v.visit_seq(exprs.iter().map(|expr| &**expr).collect::<Vec<_>>());
        },
        ast::Expr::UnaryOp(_, kind, expr) => v.visit_unary_op(kind, expr),
        ast::Expr::BinOp(_, kind, l, r) => v.visit_bin_op(kind, &l, &r),
        ast::Expr::Ternary(_, cond, branch, no_branch) => v.visit_ternary(cond, branch, no_branch),
        ast::Expr::Index(_, expr, index) => v.visit_index(expr, index),
        ast::Expr::Call(_, expr, args) => {
            let mut params = None;

            if let Some(args) = args {
                params = Some(args.iter().map(|arg| &**arg).collect::<Vec<_>>());
            }

            v.visit_call(expr, params);
        },
        ast::Expr::Access(_, ptr, expr, field) => v.visit_access(*ptr, expr, field.clone()),
    }
}
