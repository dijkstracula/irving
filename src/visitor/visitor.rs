#![allow(unused_variables)]

use crate::ast::actions::*;
use crate::ast::declarations::*;
use crate::ast::expressions::*;
use crate::ast::logic::Fmla;
use crate::ast::statements::*;
use crate::ast::toplevels::*;

// TODO: Is the standard double-dispatch approach idiomatic for rust?
// TODO: Feels like nodes need to control recursing into children.

pub trait StatementVisitor<T, E> {
    fn visit_prog(&mut self, p: &Prog) -> Result<T, E>;

    // Statements
    fn visit_if(&mut self, p: &If) -> Result<T, E>;
    fn visit_while(&mut self, p: &While) -> Result<T, E>;
    fn visit_action_sequence(&mut self, actions: &Vec<Action>) -> Result<T, E>;

    // Actions
    fn visit_assert(&mut self, a: &AssertAction) -> Result<T, E>;
    fn visit_assign(&mut self, a: &AssignAction) -> Result<T, E>;
    fn visit_assume(&mut self, a: &AssumeAction) -> Result<T, E>;
    fn visit_call(&mut self, e: &AppExpr) -> Result<T, E>;
    fn visit_ensure(&mut self, e: &EnsureAction) -> Result<T, E>;
    fn visit_requires(&mut self, e: &RequiresAction) -> Result<T, E>;

    // Declarations
    fn visit_action_decl(&mut self, action: &ActionDecl) -> Result<T, E>;
    fn visit_after(&mut self, action: &AfterDecl) -> Result<T, E>;
    fn visit_alias(&mut self, name: &Symbol, val: &Expr) -> Result<T, E>;
    fn visit_axiom(&mut self, axiom: &Fmla) -> Result<T, E>;
    fn visit_before(&mut self, action: &BeforeDecl) -> Result<T, E>;
    fn visit_export(&mut self, action: &ExportDecl) -> Result<T, E>;
    fn visit_function(&mut self, fun: &FunctionDecl) -> Result<T, E>;
    fn visit_globals(&mut self, defs: &Vec<Decl>) -> Result<T, E>;
    fn visit_import(&mut self, action: &ImportDecl) -> Result<T, E>;
    fn visit_isolate(&mut self, action: &IsolateDecl) -> Result<T, E>;
    fn visit_include(&mut self, module: &Symbol) -> Result<T, E>;
    fn visit_instance(&mut self, inst: &InstanceDecl) -> Result<T, E>;
    fn visit_invariant(&mut self, inv: &Fmla) -> Result<T, E>;
    fn visit_module(&mut self, module: &ModuleDecl) -> Result<T, E>;
    fn visit_object(&mut self, obj: &ObjectDecl) -> Result<T, E>;
    fn visit_relation(&mut self, obj: &Relation) -> Result<T, E>;
    fn visit_vardecl(&mut self, term: &Term) -> Result<T, E>;
    fn visit_typedecl(&mut self, ident: &TypeName, sort: &Sort) -> Result<T, E>;

    // auto-visitation for intermediary AST nodes


    fn visit_stmt(&mut self, s: &Stmt) -> Result<T, E> {
        match s {
            Stmt::ActionSequence(aa) => self.visit_action_sequence(aa),
            Stmt::If(i) => self.visit_if(i),
            Stmt::While(w) => self.visit_while(w),
        }
    }

    fn visit_action(&mut self, a: &Action) -> Result<T, E> {
        match a {
            Action::Assert(a) => self.visit_assert(a),
            Action::Assign(a) => self.visit_assign(a),
            Action::Assume(a) => self.visit_assume(a),
            Action::Call(e) => self.visit_call(e),
            Action::Ensure(en) => self.visit_ensure(en),
            Action::Requires(req) => self.visit_requires(req)
        }
    }

    fn visit_decl(&mut self, decl: &Decl) -> Result<T, E> {
        match decl {
            Decl::Action(a) => self.visit_action_decl(a),
            Decl::AfterAction(a) => self.visit_after(a),
            Decl::Alias(name, val) => self.visit_alias(name, val),
            Decl::Axiom(x) => self.visit_axiom(x),
            Decl::BeforeAction(a) => self.visit_before(a),
            Decl::Export(e) => self.visit_export(e),
            Decl::Function(f) => self.visit_function(f),
            Decl::Globals(g) => self.visit_globals(g),
            Decl::Import(i) => self.visit_import(i),
            Decl::Isolate(i) => self.visit_isolate(i),
            Decl::Include(i) => self.visit_include(i),
            Decl::Instance(i) => self.visit_instance(i),
            Decl::Instantiate { name, prms } => todo!(),
            Decl::Interpretation { itype, ctype } => todo!(),
                Decl::Invariant(i) => self.visit_invariant(i),
            Decl::Module(m) => self.visit_module(m),
            Decl::Object(o) => self.visit_object(o),
            Decl::Relation(r) => self.visit_relation(r),
            Decl::Stmts(stmts) => {
                // This will panic if there are no statements in the block.
                // TODO: check the grammar to ensure this isn't actually possible.
                let mut t: Option<T> = None;
                for stmt in stmts {
                    t = Some(self.visit_stmt(stmt)?);
                }
                Ok(t.unwrap())
            }
            Decl::Var(v) => self.visit_vardecl(v),
            Decl::Type(t) => self.visit_typedecl(&t.ident, &t.sort),
        }
    }


}

pub trait ExpressionVisitor<T, E> {
    // Expressions
    fn visit_app(&mut self, a: &AppExpr) -> Result<T, E>;
    fn visit_binop(&mut self, lhs: &Expr, op: &Verb, rhs: &Expr) -> Result<T, E>;
    fn visit_boolean(&mut self, b: bool) -> Result<T, E>;
    fn visit_formula(&mut self, fmla: &Fmla) -> Result<T, E>;
    fn visit_identifier(&mut self, ident: &Ident) -> Result<T, E>;
    fn visit_index(&mut self, idx: &IndexExpr) -> Result<T, E>;
    fn visit_number(&mut self, n: &i64) -> Result<T, E>;
    fn visit_param(&mut self, p: &Param) -> Result<T, E>;
    fn visit_unaryop(&mut self, op: &Verb, expr: &Expr) -> Result<T, E>;
    fn visit_term(&mut self, term: &Term) -> Result<T, E>;
    fn visit_this(&mut self) -> Result<T, E>;

    fn visit_expr(&mut self, e: &Expr) -> Result<T, E> {
        match e {
            Expr::App(app) => self.visit_app(app),
            Expr::BinOp{lhs, op, rhs} => self.visit_binop(&lhs, op, &rhs),
            Expr::Boolean(b) => self.visit_boolean(*b),
//            Expr::Formula(fmla) => self.visit_formula(fmla),
            Expr::Identifier(ident) => self.visit_identifier(ident),
            Expr::Index(idx) => self.visit_index(idx),
            Expr::Number(i) => self.visit_number(i),
            Expr::UnaryOp { op, expr } => self.visit_unaryop(op, expr),
            Expr::Term(t) => self.visit_term(t),
            Expr::This => self.visit_this(),
        }
    }
}