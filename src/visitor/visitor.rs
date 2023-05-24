#![allow(unused_variables)]

use crate::ast::actions::*;
use crate::ast::declarations::*;
use crate::ast::expressions::*;
use crate::ast::logic::*;
use crate::ast::statements::*;
use crate::ast::toplevels::*;

//use super::control::Control;

// TODO: Is the standard double-dispatch approach idiomatic for rust?
// TODO: Feels like nodes need to control recursing into children.

pub trait Visitor<T, E> where T: Default {
    fn visit_prog(&mut self, p: &mut Prog) -> Result<T, E> {
        self.visit_isolate(&mut p.top)
    }

    // Statements
    fn visit_if(&mut self, p: &mut If) -> Result<T, E>;
    fn visit_while(&mut self, p: &mut While) -> Result<T, E>;
    fn visit_action_sequence(&mut self, actions: &mut Vec<Action>) -> Result<T, E>;

    // Actions
    fn visit_assert(&mut self, a: &mut AssertAction) -> Result<T, E> {
        self.visit_expr(&mut a.pred)
    }
    fn visit_assign(&mut self, a: &mut AssignAction) -> Result<T, E> {
        self.visit_expr(&mut a.lhs)?;
        self.visit_expr(&mut a.rhs)
    }
    fn visit_assume(&mut self, a: &mut AssumeAction) -> Result<T, E> {
        self.visit_expr(&mut a.pred)
    }
    fn visit_call(&mut self, e: &mut AppExpr) -> Result<T, E> {
        let func_t = self.visit_expr(&mut e.func)?;

        let mut t: Option<T> = None;
        for arg in &mut e.args {
            t = Some(self.visit_expr(arg)?);
        }
        Ok(t.unwrap_or(func_t))
    }
    fn visit_ensure(&mut self, e: &mut EnsureAction) -> Result<T, E> {
        self.visit_formula(&mut e.pred)
    }
    fn visit_requires(&mut self, e: &mut RequiresAction) -> Result<T, E> {
        self.visit_formula(&mut e.pred)
    }

    // Declarations
    fn visit_action_decl(&mut self, action: &mut ActionDecl) -> Result<T, E> {
        let mut t: Option<T> = None;
        for param in &mut action.params {
            t = Some(self.visit_param(param)?);
        }
        if let Some(p) = &mut action.ret {
            t = Some(self.visit_param(p)?);
        }
        if let Some(body) = &mut action.body {
            for stmt in body {
                t = Some(self.visit_stmt(stmt)?);
            }
        }
        Ok(t.unwrap_or_default())
    }
    fn visit_after(&mut self, action: &mut AfterDecl) -> Result<T, E> {
        let mut t: Option<T> = None;

        if let Some(params) = &mut action.params {
            for param in params {
                t = Some(self.visit_param(param)?);
            }
        }
        if let Some(r) = &mut action.ret {
            t = Some(self.visit_param(r)?);
        }
        for stmt in &mut action.body {
            t = Some(self.visit_stmt(stmt)?);
        }
        Ok(t.unwrap_or_default())
    }
    fn visit_alias(&mut self, name: &Symbol, val: &mut Expr) -> Result<T, E> {
        self.visit_expr(val)
    }
    fn visit_axiom(&mut self, axiom: &mut Fmla) -> Result<T, E> {
        self.visit_formula(axiom)
    }
    fn visit_before(&mut self, action: &mut BeforeDecl) -> Result<T, E> {
        let mut t: Option<T> = None;
        if let Some(params) = &mut action.params {
            for param in params {
                t = Some(self.visit_param(param)?);
            }
        }
        for stmt in &mut action.body {
            t = Some(self.visit_stmt(stmt)?);
        }
        Ok(t.unwrap_or_default())
    }
    fn visit_export(&mut self, action: &mut ExportDecl) -> Result<T, E> {
        match action {
            ExportDecl::Action(action) => self.visit_action_decl(action),
            ExportDecl::ForwardRef(_) => Ok(T::default())
        }
    }
    fn visit_function(&mut self, fun: &mut FunctionDecl) -> Result<T, E> {
        let mut t: Option<T> = None;
        for param in &mut fun.params {
            t = Some(self.visit_param(param)?);
        }
        Ok(t.unwrap_or_default())
    }
    fn visit_globals(&mut self, defs: &mut Vec<Decl>) -> Result<T, E> {
        let mut t: Option<T> = None;
        for decl in defs {
            t = Some(self.visit_decl(decl)?);
        }
        Ok(t.unwrap_or_default())
    }
    fn visit_import(&mut self, action: &mut ImportDecl) -> Result<T, E> {
        let mut t: Option<T> = None;
        for param in &mut action.params {
            t = Some(self.visit_param(param)?);
        }
        Ok(t.unwrap_or_default())
    }
    fn visit_isolate(&mut self, action: &mut IsolateDecl) -> Result<T, E> {
        let mut t: Option<T> = None;
        for param in &mut action.params {
            t = Some(self.visit_param(param)?);
        }
        for decl in &mut action.body {
            t = Some(self.visit_decl(decl)?);
        }
        Ok(t.unwrap_or_default())
    }
    fn visit_include(&mut self, module: &mut Symbol) -> Result<T, E> {
        Ok(T::default())
    }
    fn visit_instance(&mut self, inst: &mut InstanceDecl) -> Result<T, E> {
        let mut t: Option<T> = None;
        for arg in &mut inst.args {
            t = Some(self.visit_param(arg)?);
        }
        Ok(t.unwrap_or_default())

    }
    fn visit_invariant(&mut self, inv: &mut Fmla) -> Result<T, E> {
        self.visit_formula(inv)
    }
    fn visit_module(&mut self, module: &mut ModuleDecl) -> Result<T, E> {
        let mut t: Option<T> = None;
        for param in &mut module.params {
            t = Some(self.visit_param(param)?);
        }
        for decl in &mut module.body {
            t = Some(self.visit_decl(decl)?);
        }
        Ok(t.unwrap_or_default())
    }
    fn visit_object(&mut self, obj: &mut ObjectDecl) -> Result<T, E> {
        let mut t: Option<T> = None;
        for param in &mut obj.params {
            t = Some(self.visit_param(param)?);
        }
        for decl in &mut obj.body {
            t = Some(self.visit_decl(decl)?);
        }
        Ok(t.unwrap_or_default())
    }
    fn visit_relation(&mut self, obj: &mut Relation) -> Result<T, E> {
        let mut t: Option<T> = None;
        for param in &mut obj.params {
            t = Some(self.visit_param(param)?);
        }
        Ok(t.unwrap_or_default())
    }
    fn visit_vardecl(&mut self, term: &mut Term) -> Result<T, E> {
        self.visit_identifier(&mut term.id)
    }
    fn visit_typedecl(&mut self, ident: &TypeName, sort: &mut Sort) -> Result<T, E>;


    // auto-visitation for intermediary AST nodes


    fn visit_stmt(&mut self, s: &mut Stmt) -> Result<T, E> {
        match s {
            Stmt::ActionSequence(aa) => self.visit_action_sequence(aa),
            Stmt::If(i) => self.visit_if(i),
            Stmt::While(w) => self.visit_while(w),
        }
    }

    fn visit_action(&mut self, a: &mut Action) -> Result<T, E> {
        match a {
            Action::Assert(a) => self.visit_assert(a),
            Action::Assign(a) => self.visit_assign(a),
            Action::Assume(a) => self.visit_assume(a),
            Action::Call(e) => self.visit_app(e),
            Action::Ensure(en) => self.visit_ensure(en),
            Action::Requires(req) => self.visit_requires(req)
        }
    }

    fn visit_decl(&mut self, decl: &mut Decl) -> Result<T, E> {
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
            Decl::Type(t) => self.visit_typedecl(&t.ident, &mut t.sort),
        }
    }


    // Expressions
    fn visit_app(&mut self, a: &mut AppExpr) -> Result<T, E> {
        let mut t: Option<T> = Some(self.visit_expr(&mut a.func)?);
        for arg in &mut a.args {
            t = Some(self.visit_expr(arg)?);
        }
        Ok(t.unwrap())
    }
    fn visit_binop(&mut self, lhs: &mut Expr, op: &Verb, rhs: &mut Expr) -> Result<T, E> {
        self.visit_expr(lhs)?;
        self.visit_expr(rhs)
    }
    fn visit_boolean(&mut self, b: &mut bool) -> Result<T, E> {
        Ok(T::default())
    }
    fn visit_formula(&mut self, fmla: &mut Fmla) -> Result<T, E> {
        match fmla {
            Fmla::Forall(Forall { vars, fmla }) => {
                for var in vars {
                    self.visit_param(var)?;
                }
                self.visit_formula(fmla)
            }
            Fmla::Exists(Exists { vars, fmla }) => {
                for var in vars {
                    self.visit_param(var)?;
                }
                self.visit_formula(fmla)
            }
            Fmla::Pred(expr) => self.visit_expr(expr)
        }
    }
    fn visit_identifier(&mut self, ident: &mut Ident) -> Result<T, E> {
        Ok(T::default())
    }
    fn visit_index(&mut self, idx: &mut IndexExpr) -> Result<T, E> {
        self.visit_expr(&mut idx.lhs)?;
        self.visit_expr(&mut idx.idx)
    }
    fn visit_number(&mut self, n: &mut i64) -> Result<T, E> {
        Ok(T::default())
    }
    fn visit_param(&mut self, p: &mut Param) -> Result<T, E> {
        match &mut p.sort {
            None => Ok(T::default()),
            Some(sort) => self.visit_identifier(sort)
        }
    }
    fn visit_unaryop(&mut self, op: &Verb, expr: &mut Expr) -> Result<T, E> {
        self.visit_expr(expr)
    }
    fn visit_term(&mut self, term: &mut Term) -> Result<T, E> {
        self.visit_identifier(&mut term.id)?;
        match &mut term.sort {
            None => Ok(T::default()),
            Some(sort) => self.visit_identifier(sort)
        }
    }
    fn visit_this(&mut self) -> Result<T, E> {
        Ok(T::default())
    }

    fn visit_expr(&mut self, e: &mut Expr) -> Result<T, E> {
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


    fn visit_vec_interleaved<F, FU, U>(&mut self, us: &mut Vec<U>, mut f: FU, mut sep_f: F) -> Result<T, E> 
            where
        FU: FnMut(&mut Self, &mut U) -> Result<T, E>,
        F: FnMut (&mut Self) -> Result<T,E> {
        let mut t: Option<T> = None;
        let mut sep = false;
        for u in us {
            if sep {
                sep_f(self)?;
            } else {
                sep = true;
            }
            t = Some(f(self, u)?);
        }
        Ok(t.unwrap_or_default())
    }

    fn visit_vec<F, FU, U>(&mut self, us: &mut Vec<U>, f: FU, sep_f: F) -> Result<T, E> 
            where
        FU: FnMut(&mut Self, &mut U) -> Result<T, E> {
        self.visit_vec_interleaved(us, f, |_| Ok(T::default()))
    }
}