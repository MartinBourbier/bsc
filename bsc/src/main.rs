use bsc_lexer::{Lexer, logos::LogosLexer};
use bsc_parse::{grammar, visit::Visitor};

struct PPVisitor;

impl Visitor for PPVisitor {
    fn visit_constant(&mut self, val: &bsc_parse::ast::Constant) {
        match val {
            bsc_parse::ast::Constant::Integer(n) => print!("{n}"),
            bsc_parse::ast::Constant::Float(n) => print!("{n}"),
        }
    }

    fn visit_identifier(&mut self, ident: String) {
        print!("{ident}")
    }

    fn visit_literal(&mut self, lit: &bsc_parse::ast::StringLiteral) {
        if let bsc_parse::ast::StringLiteral::StringLiteral(s) = lit.clone() {
            print!("\"");
            self.visit_identifier(s);
            print!("\"");
        } else {
            print!("__func__");
        }
    }

    fn visit_bin_op(&mut self, kind: &bsc_parse::ast::BinOpKind, l: &bsc_parse::ast::Expr, r: &bsc_parse::ast::Expr) {
        self.visit_expr(l);
        print!(" {kind} ");
        self.visit_expr(r);
    }

    fn visit_seq(&mut self, exprs: Vec<&bsc_parse::ast::Expr>) {
        for i in 0..exprs.len() {
            self.visit_expr(exprs[i]);

            if i != exprs.len() - 1 {
                print!(", ")
            }
        }
    }

    fn visit_unary_op(&mut self, kind: &bsc_parse::ast::UnaryOpKind, expr: &bsc_parse::ast::Expr) {
        print!("{kind}");
        self.visit_expr(expr);
    }

    fn visit_ternary(&mut self, cond: &bsc_parse::ast::Expr, branch: &bsc_parse::ast::Expr, no_branch: &bsc_parse::ast::Expr) {
        self.visit_expr(cond);
        print!(" ? ");
        self.visit_expr(branch);
        print!(" : ");
        self.visit_expr(no_branch);
    }

    fn visit_index(&mut self, expr: &bsc_parse::ast::Expr, index: &bsc_parse::ast::Expr) {
        self.visit_expr(expr);
        print!("[");
        self.visit_expr(index);
        print!("]");
    }

    fn visit_call(&mut self, expr: &bsc_parse::ast::Expr, args: Option<Vec<&bsc_parse::ast::Expr>>) {
        self.visit_expr(expr);

        print!("(");
        if let Some(args) = args {
            self.visit_seq(args);
        }
        print!(")");
    }

    fn visit_access(&mut self, is_ptr: bool, expr: &bsc_parse::ast::Expr, field: String) {
        self.visit_expr(expr);

        if is_ptr {
            print!("->");
        } else {
            print!(".");
        }

        self.visit_identifier(field);
    }
}

fn main() {
    // let mut errors = Vec::new();
    // let res = grammar::TranslationUnitParser::new()
    //     .parse(
    //         &mut errors,
    //         LogosLexer::new(r#"(_Generic (toto, default : test))"#),
    //     )
    //     .unwrap();

    // println!("{:?}", errors);
    // println!("{}", res);
    // println!("{:?}", res);

    let mut errors = Vec::new();
    let mut res = grammar::TranslationUnitParser::new()
        .parse(&mut errors, LogosLexer::new(r#"(expr.field, expr->field, toto(), toto(tata, tutu)[thing])"#))
        .unwrap();

    let mut pp_visitor = PPVisitor;
    pp_visitor.visit_expr(&mut res);

    // println!("{:?}", errors);
    // println!("{:?}", res);
}
