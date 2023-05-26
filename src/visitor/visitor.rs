#![allow(unused_variables)]

use crate::ast::actions::*;
use crate::ast::declarations::*;
use crate::ast::expressions::*;
use crate::ast::logic::*;
use crate::ast::statements::*;
use crate::ast::toplevels::*;

use super::control::Control::Remove;
use super::control::Control::Continue;
use super::control::VisitorResult;

// TODO: Is the standard double-dispatch approach idiomatic for rust?
// TODO: Feels like nodes need to control recursing into children.

pub trait Visitor<E> {
    fn visit_prog(&mut self, p: &mut Prog) -> VisitorResult<E> {
        self.visit_isolate(&mut p.top)?;
        Ok(Continue)
    }

    // Statements
    fn visit_if(&mut self, p: &mut If) -> VisitorResult<E> {
        self.visit_expr(&mut p.tst)?;
        self.visit_vec(&mut p.thn, |slf, stmt| slf.visit_stmt(stmt))?;
        if let Some(els) = &mut p.els {
            self.visit_vec(els, |slf, stmt| slf.visit_stmt(stmt))?;
        }
        Ok(Continue)
    }
    fn visit_while(&mut self, w: &mut While) -> VisitorResult<E> {
        self.visit_expr(&mut w.test)?;
        self.visit_vec(&mut w.doit, |slf, stmt| slf.visit_stmt(stmt))?;
        Ok(Continue)
    }

    // Actions
    fn visit_assert(&mut self, a: &mut AssertAction) -> VisitorResult<E> {
        self.visit_expr(&mut a.pred)?;
        Ok(Continue)
    }
    fn visit_assign(&mut self, a: &mut AssignAction) -> VisitorResult<E> {
        self.visit_expr(&mut a.lhs)?;
        self.visit_expr(&mut a.rhs)?;
        Ok(Continue)
    }
    fn visit_assume(&mut self, a: &mut AssumeAction) -> VisitorResult<E> {
        self.visit_expr(&mut a.pred)?;
        Ok(Continue)
    }
    fn visit_call(&mut self, e: &mut AppExpr) -> VisitorResult<E> {
        self.visit_expr(&mut e.func)?;
        self.visit_vec(&mut e.args, |slf, arg| slf.visit_expr(arg))?;
        Ok(Continue)
    }
    fn visit_ensure(&mut self, e: &mut EnsureAction) -> VisitorResult<E> {
        self.visit_formula(&mut e.pred)?;
        Ok(Continue)
    }
    fn visit_requires(&mut self, e: &mut RequiresAction) -> VisitorResult<E> {
        self.visit_formula(&mut e.pred)?;
        Ok(Continue)
    }

    // Declarations
    fn visit_action_decl(&mut self, action: &mut ActionDecl) -> VisitorResult<E> {
        self.visit_vec(&mut action.params, |slf, p| slf.visit_param(p))?;
        if let Some(p) = &mut action.ret {
            self.visit_param(p)?;
        }
        if let Some(body) = &mut action.body {
            self.visit_vec(body, |slf, p| slf.visit_stmt(p))?;
        }
        Ok(Continue)
    }
    fn visit_after(&mut self, action: &mut AfterDecl) -> VisitorResult<E> {
        if let Some(params) = &mut action.params {
            self.visit_vec(params, |slf, p| slf.visit_param(p))?;
        }
        if let Some(r) = &mut action.ret {
            self.visit_param(r)?;
        }
        self.visit_vec(&mut action.body, |slf, stmt| slf.visit_stmt(stmt))?;
        Ok(Continue)
    }
    fn visit_alias(&mut self, name: &Symbol, val: &mut Expr) -> VisitorResult<E> {
        self.visit_expr(val)?;
        Ok(Continue)
    }
    fn visit_axiom(&mut self, axiom: &mut Fmla) -> VisitorResult<E> {
        self.visit_formula(axiom)?;
        Ok(Continue)
    }
    fn visit_before(&mut self, action: &mut BeforeDecl) -> VisitorResult<E> {
        if let Some(params) = &mut action.params {
            self.visit_vec(params, |slf, p| slf.visit_param(p))?;
        }
        for stmt in &mut action.body {
            self.visit_stmt(stmt)?;
        }
        Ok(Continue)
    }
    fn visit_export(&mut self, action: &mut ExportDecl) -> VisitorResult<E> {
        match action {
            ExportDecl::Action(action) => { self.visit_action_decl(action)?; }
            ExportDecl::ForwardRef(_) =>  { }
        }
        Ok(Continue)
    }
    fn visit_function(&mut self, fun: &mut FunctionDecl) -> VisitorResult<E> {
        //self.visit_symbol(&mut fun.name)?;
        self.visit_symbol(&mut fun.ret)?;
        self.visit_vec(&mut fun.params, |slf, p| slf.visit_param(p))?;
        Ok(Continue)
    }
    fn visit_globals(&mut self, defs: &mut Vec<Decl>) -> VisitorResult<E> {
        self.visit_vec(defs, |slf, d| slf.visit_decl(d))?;
        Ok(Continue)
    }
    fn visit_import(&mut self, action: &mut ImportDecl) -> VisitorResult<E> {
        self.visit_vec(&mut action.params, |slf, p| slf.visit_param(p))?;
        Ok(Continue)
    }
    fn visit_isolate(&mut self, action: &mut IsolateDecl) -> VisitorResult<E> {
        self.visit_vec(&mut action.params, |slf, p| slf.visit_param(p))?;
        self.visit_vec(&mut action.body, |slf, d| slf.visit_decl(d))?;
        Ok(Continue)
    }
    fn visit_include(&mut self, module: &mut Symbol) -> VisitorResult<E> {
        Ok(Continue)
    }
    fn visit_instance(&mut self, inst: &mut InstanceDecl) -> VisitorResult<E> {
        self.visit_vec(&mut inst.args, |slf, p| slf.visit_param(p))?;
        Ok(Continue)
    }
    fn visit_invariant(&mut self, inv: &mut Fmla) -> VisitorResult<E> {
        self.visit_formula(inv)?;
        Ok(Continue)
    }

    fn visit_module(&mut self, module: &mut ModuleDecl) -> VisitorResult<E> {
        self.visit_vec(&mut module.params, |slf, p| slf.visit_param(p))?;
        self.visit_vec(&mut module.body, |slf, d| slf.visit_decl(d))?;
        Ok(Continue)
    }
    fn visit_object(&mut self, obj: &mut ObjectDecl) -> VisitorResult<E> {
        self.visit_vec(&mut obj.params, |slf, p| slf.visit_param(p))?;
        self.visit_vec(&mut obj.body, |slf, d| slf.visit_decl(d))?;
        Ok(Continue)
    }
    fn visit_relation(&mut self, obj: &mut Relation) -> VisitorResult<E> {
        self.visit_vec(&mut obj.params, |slf, p| slf.visit_param(p))?;
        Ok(Continue)
    }
    fn visit_vardecl(&mut self, term: &mut Term) -> VisitorResult<E> {
        self.visit_identifier(&mut term.id)?;
        Ok(Continue)
    }
    fn visit_typedecl(&mut self, ident: &TypeName, sort: &mut Sort) -> VisitorResult<E> {
        Ok(Continue)
    }


    // auto-visitation for intermediary AST nodes


    fn visit_stmt(&mut self, s: &mut Stmt) -> VisitorResult<E> {
        match s {
            Stmt::ActionSequence(aa) => {
                for a in aa {
                    self.visit_action(a)?;
                }
                Ok(Continue)
            }
            Stmt::If(i) => self.visit_if(i),
            Stmt::While(w) => self.visit_while(w),
        }
    }

    fn visit_action(&mut self, a: &mut Action) -> VisitorResult<E> {
        match a {
            Action::Assert(a) => self.visit_assert(a),
            Action::Assign(a) => self.visit_assign(a),
            Action::Assume(a) => self.visit_assume(a),
            Action::Call(e) => { self.visit_app(e)?;
                Ok(Continue)
            }
            Action::Ensure(en) => self.visit_ensure(en),
            Action::Requires(req) => self.visit_requires(req)
        }
    }

    fn visit_decl(&mut self, decl: &mut Decl) -> VisitorResult<E> {
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
                for stmt in stmts {
                    self.visit_stmt(stmt)?;
                }
                Ok(Continue)
            }
            Decl::Var(v) => self.visit_vardecl(v),
            Decl::Type(t) => self.visit_typedecl(&t.ident, &mut t.sort),
        }
    }


    // Expressions
    fn visit_app(&mut self, a: &mut AppExpr) -> VisitorResult<E> {
        self.visit_expr(&mut a.func)?;
        for arg in &mut a.args {
            self.visit_expr(arg)?;
        }
        Ok(Continue)
    }
    fn visit_binop(&mut self, lhs: &mut Expr, op: &Verb, rhs: &mut Expr) -> VisitorResult<E> {
        self.visit_expr(lhs)?;
        self.visit_expr(rhs)?;
        Ok(Continue)
    }
    fn visit_boolean(&mut self, b: &mut bool) -> VisitorResult<E> {
        Ok(Continue)
    }
    fn visit_formula(&mut self, fmla: &mut Fmla) -> VisitorResult<E> {
        match fmla {
            Fmla::Forall(Forall { vars, fmla }) => {
                self.visit_vec(vars, |slf, v| slf.visit_param(v))?;
                self.visit_formula(fmla)
            }
            Fmla::Exists(Exists { vars, fmla }) => {
                self.visit_vec(vars, |slf, v| slf.visit_param(v))?;
                self.visit_formula(fmla)
            }
            Fmla::Pred(expr) => { 
                self.visit_expr(expr)?;
                Ok(Continue)
            }
        }
    }
    fn visit_identifier(&mut self, ident: &mut Ident) -> VisitorResult<E> {
        Ok(Continue)
    }
    fn visit_index(&mut self, idx: &mut IndexExpr) -> VisitorResult<E> {
        self.visit_expr(&mut idx.lhs)?;
        self.visit_expr(&mut idx.idx)?;
        Ok(Continue)
    }
    fn visit_number(&mut self, n: &mut i64) -> VisitorResult<E> {
        Ok(Continue)
    }
    fn visit_param(&mut self, p: &mut Param) -> VisitorResult<E> {
        if let Some(sort) = &mut p.sort {
            self.visit_identifier(sort)?;
        }
        Ok(Continue)
    }
    fn visit_unaryop(&mut self, op: &Verb, expr: &mut Expr) -> VisitorResult<E> {
        self.visit_expr(expr)
    }
    fn visit_symbol(&mut self, sym: &mut Symbol) -> VisitorResult<E> {
        Ok(Continue)
    }
    fn visit_term(&mut self, term: &mut Term) -> VisitorResult<E> {
        self.visit_identifier(&mut term.id)?;
        match &mut term.sort {
            None => Ok(Continue),
            Some(sort) => self.visit_identifier(sort)
        }
    }
    fn visit_this(&mut self) -> VisitorResult<E> {
        Ok(Continue)
    }

    fn visit_expr(&mut self, e: &mut Expr) -> VisitorResult<E> {
        match e {
            Expr::App(app) => self.visit_app(app),
            Expr::BinOp{lhs, op, rhs} => self.visit_binop(lhs, op, rhs),
            Expr::Boolean(b) => self.visit_boolean(b),
//            Expr::Formula(fmla) => self.visit_formula(fmla),
            Expr::Identifier(ident) => self.visit_identifier(ident),
            Expr::Index(idx) => self.visit_index(idx),
            Expr::Number(i) => self.visit_number(i),
            Expr::UnaryOp { op, expr } => self.visit_unaryop(op, expr),
            Expr::Term(t) => self.visit_term(t),
            Expr::This => self.visit_this(),
        }
    }


    fn visit_vec_interleaved<F, FT, T>(&mut self, ts: &mut Vec<T>, mut f: FT, mut sep_f: F) -> VisitorResult<E> 
            where
        FT: FnMut(&mut Self, &mut T) -> VisitorResult<E>,
        F: FnMut (&mut Self) -> Result<(),E> {
        for (i, t) in ts.into_iter().enumerate() {
            if i > 0 {
                sep_f(self)?;
            }
            match f(self, t)? {
                Continue => continue,
                Remove => todo!(),
            }
        }
        Ok(Continue)
    }

    fn visit_vec<FT, T>(&mut self, ts: &mut Vec<T>, mut f: FT) -> VisitorResult<E> 
            where
        FT: FnMut(&mut Self, &mut T) -> VisitorResult<E> {
        for (i, t) in ts.into_iter().enumerate() {
            match f(self, t)? {
                Continue => continue,
                Remove => todo!(),
            }
        }
        Ok(Continue)
    }
}