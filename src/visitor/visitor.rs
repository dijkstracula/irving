#![allow(unused_variables)]

use crate::ast::actions::*;
use crate::ast::declarations::*;
use crate::ast::expressions::*;
use crate::ast::logic::*;
use crate::ast::statements::*;
use crate::ast::toplevels::*;
use crate::typechecker::sorts::IvySort;

use super::control::Control::Continue;
use super::control::Control::Remove;
use super::control::VisitorResult;

// TODO: Is the standard double-dispatch approach idiomatic for rust?
// TODO: Feels like nodes need to control recursing into children.

pub trait Visitor<T, E>
where
    T: Default,
{
    fn visit_prog(&mut self, p: &mut Prog) -> VisitorResult<T, E> {
        self.visit_isolate(&mut p.top)
    }

    // Statements
    fn visit_if(&mut self, p: &mut If) -> VisitorResult<T, E> {
        self.visit_expr(&mut p.tst)?;
        self.visit_vec(&mut p.thn, |slf, stmt| slf.visit_stmt(stmt))?;
        if let Some(els) = &mut p.els {
            self.visit_vec(els, |slf, stmt| slf.visit_stmt(stmt))?;
        }
        Ok(Continue(T::default()))
    }
    fn visit_while(&mut self, w: &mut While) -> VisitorResult<T, E> {
        self.visit_expr(&mut w.test)?;
        self.visit_vec(&mut w.doit, |slf, stmt| slf.visit_stmt(stmt))
    }

    // Actions
    fn visit_assert(&mut self, a: &mut AssertAction) -> VisitorResult<T, E> {
        self.visit_expr(&mut a.pred)
    }
    fn visit_assign(&mut self, a: &mut AssignAction) -> VisitorResult<T, E> {
        self.visit_expr(&mut a.lhs)?;
        self.visit_expr(&mut a.rhs)
    }
    fn visit_assume(&mut self, a: &mut AssumeAction) -> VisitorResult<T, E> {
        self.visit_expr(&mut a.pred)
    }
    fn visit_call(&mut self, e: &mut AppExpr) -> VisitorResult<T, E> {
        self.visit_expr(&mut e.func)?;
        self.visit_vec(&mut e.args, |slf, arg| slf.visit_expr(arg))
    }
    fn visit_ensure(&mut self, e: &mut EnsureAction) -> VisitorResult<T, E> {
        self.visit_formula(&mut e.pred)
    }
    fn visit_requires(&mut self, e: &mut RequiresAction) -> VisitorResult<T, E> {
        self.visit_formula(&mut e.pred)
    }

    // Declarations
    fn visit_action_decl(&mut self, action: &mut ActionDecl) -> VisitorResult<T, E> {
        self.visit_vec(&mut action.params, |slf, p| slf.visit_param(p))?;
        if let Some(p) = &mut action.ret {
            self.visit_param(p)?;
        }
        if let Some(body) = &mut action.body {
            self.visit_vec(body, |slf, p| slf.visit_stmt(p))?;
        }
        Ok(Continue(T::default()))
    }
    fn visit_after(&mut self, action: &mut AfterDecl) -> VisitorResult<T, E> {
        if let Some(params) = &mut action.params {
            self.visit_vec(params, |slf, p| slf.visit_param(p))?;
        }
        if let Some(r) = &mut action.ret {
            self.visit_param(r)?;
        }
        self.visit_vec(&mut action.body, |slf, stmt| slf.visit_stmt(stmt))
    }
    fn visit_alias(&mut self, name: &Symbol, val: &mut Expr) -> VisitorResult<T, E> {
        self.visit_expr(val)
    }
    fn visit_attribute(&mut self, expr: &mut Expr) -> VisitorResult<T, E> {
        self.visit_expr(expr)
    }
    fn visit_axiom(&mut self, axiom: &mut Fmla) -> VisitorResult<T, E> {
        self.visit_formula(axiom)
    }
    fn visit_before(&mut self, action: &mut BeforeDecl) -> VisitorResult<T, E> {
        if let Some(params) = &mut action.params {
            self.visit_vec(params, |slf, p| slf.visit_param(p))?;
        }
        self.visit_vec(&mut action.body, |slf, stmt| slf.visit_stmt(stmt))
    }
    fn visit_common(&mut self, decls: &mut Vec<Decl>) -> VisitorResult<T, E> {
        self.visit_vec(decls, |slf, d| slf.visit_decl(d))
    }
    fn visit_export(&mut self, action: &mut ExportDecl) -> VisitorResult<T, E> {
        match action {
            ExportDecl::Action(action) => self.visit_action_decl(action),
            ExportDecl::ForwardRef(_) => Ok(Continue(T::default())),
        }
    }
    fn visit_function(&mut self, fun: &mut FunctionDecl) -> VisitorResult<T, E> {
        //self.visit_symbol(&mut fun.name)?;
        self.visit_symbol(&mut fun.ret)?;
        self.visit_vec(&mut fun.params, |slf, p| slf.visit_param(p))
    }
    fn visit_globals(&mut self, defs: &mut Vec<Decl>) -> VisitorResult<T, E> {
        self.visit_vec(defs, |slf, d| slf.visit_decl(d))
    }
    fn visit_implement_action(&mut self, action: &mut ImplementDecl) -> VisitorResult<T, E> {
        self.visit_vec(&mut action.params, |slf, p| slf.visit_param(p))?;
        if let Some(p) = &mut action.ret {
            self.visit_param(p)?;
        }
        if let Some(body) = &mut action.body {
            self.visit_vec(body, |slf, p| slf.visit_stmt(p))?;
        }
        Ok(Continue(T::default()))
    }
    fn visit_implementation(&mut self, decls: &mut Vec<Decl>) -> VisitorResult<T, E> {
        self.visit_vec(decls, |slf, d| slf.visit_decl(d))
    }
    fn visit_import(&mut self, action: &mut ImportDecl) -> VisitorResult<T, E> {
        self.visit_vec(&mut action.params, |slf, p| slf.visit_param(p))
    }
    fn visit_isolate(&mut self, action: &mut IsolateDecl) -> VisitorResult<T, E> {
        self.visit_vec(&mut action.params, |slf, p| slf.visit_param(p))?;
        self.visit_vec(&mut action.body, |slf, d| slf.visit_decl(d))
    }
    fn visit_include(&mut self, module: &mut Symbol) -> VisitorResult<T, E> {
        Ok(Continue(T::default()))
    }
    fn visit_instance(&mut self, inst: &mut InstanceDecl) -> VisitorResult<T, E> {
        self.visit_vec(&mut inst.args, |slf, p| slf.visit_param(p))
    }
    fn visit_invariant(&mut self, inv: &mut Fmla) -> VisitorResult<T, E> {
        self.visit_formula(inv)
    }

    fn visit_module(&mut self, module: &mut ModuleDecl) -> VisitorResult<T, E> {
        self.visit_vec(&mut module.params, |slf, p| slf.visit_param(p))?;
        self.visit_vec(&mut module.body, |slf, d| slf.visit_decl(d))
    }
    fn visit_object(&mut self, obj: &mut ObjectDecl) -> VisitorResult<T, E> {
        self.visit_vec(&mut obj.params, |slf, p| slf.visit_param(p))?;
        self.visit_vec(&mut obj.body, |slf, d| slf.visit_decl(d))
    }
    fn visit_relation(&mut self, obj: &mut Relation) -> VisitorResult<T, E> {
        self.visit_vec(&mut obj.params, |slf, p| slf.visit_param(p))
    }
    fn visit_specification(&mut self, decls: &mut Vec<Decl>) -> VisitorResult<T, E> {
        self.visit_vec(decls, |slf, d| slf.visit_decl(d))
    }
    fn visit_vardecl(&mut self, term: &mut Term) -> VisitorResult<T, E> {
        self.visit_symbol(&mut term.id)?;
        if let Some(sort) = &mut term.sort {
            self.visit_identifier(sort)?;
        }
        Ok(Continue(T::default()))
    }
    fn visit_typedecl(&mut self, ident: &TypeName, sort: &mut IvySort) -> VisitorResult<T, E> {
        Ok(Continue(T::default()))
    }

    // auto-visitation for intermediary AST nodes

    fn visit_stmt(&mut self, s: &mut Stmt) -> VisitorResult<T, E> {
        match s {
            Stmt::ActionSequence(aa) => self.visit_vec(aa, |slf, a| slf.visit_action(a)),
            Stmt::If(i) => self.visit_if(i),
            Stmt::While(w) => self.visit_while(w),
        }
    }

    fn visit_action(&mut self, a: &mut Action) -> VisitorResult<T, E> {
        match a {
            Action::Assert(a) => self.visit_assert(a),
            Action::Assign(a) => self.visit_assign(a),
            Action::Assume(a) => self.visit_assume(a),
            Action::Call(e) => self.visit_app(e),
            Action::Ensure(en) => self.visit_ensure(en),
            Action::Requires(req) => self.visit_requires(req),
        }
    }

    fn visit_decl(&mut self, decl: &mut Decl) -> VisitorResult<T, E> {
        match decl {
            Decl::Action(a) => self.visit_action_decl(a),
            Decl::AfterAction(a) => self.visit_after(a),
            Decl::Alias(name, val) => self.visit_alias(name, val),
            Decl::Attribute(attr) => self.visit_attribute(attr),
            Decl::Axiom(x) => self.visit_axiom(x),
            Decl::BeforeAction(a) => self.visit_before(a),
            Decl::Common(decls) => todo!(),
            Decl::Export(e) => self.visit_export(e),
            Decl::Function(f) => self.visit_function(f),
            Decl::Globals(g) => self.visit_globals(g),
            Decl::Import(i) => self.visit_import(i),
            Decl::Isolate(i) => self.visit_isolate(i),
            Decl::Include(i) => self.visit_include(i),
            Decl::Implement(a) => self.visit_implement_action(a),
            Decl::Implementation(decls) => todo!(),
            Decl::Instance(i) => self.visit_instance(i),
            Decl::Instantiate { name, prms } => todo!(),
            Decl::Interpretation { itype, ctype } => todo!(),
            Decl::Invariant(i) => self.visit_invariant(i),
            Decl::Module(m) => self.visit_module(m),
            Decl::Object(o) => self.visit_object(o),
            Decl::Relation(r) => self.visit_relation(r),
            Decl::Specification(decls) => todo!(),
            Decl::Stmts(stmts) => self.visit_vec(stmts, |slf, stmt| slf.visit_stmt(stmt)),
            Decl::Var(v) => self.visit_vardecl(v),
            Decl::Type(t) => self.visit_typedecl(&t.ident, &mut t.sort),
        }
    }

    // Expressions
    fn visit_app(&mut self, a: &mut AppExpr) -> VisitorResult<T, E> {
        self.visit_expr(&mut a.func)?;
        self.visit_vec(&mut a.args, |slf, arg| slf.visit_expr(arg))
    }
    fn visit_binop(&mut self, lhs: &mut Expr, op: &Verb, rhs: &mut Expr) -> VisitorResult<T, E> {
        self.visit_expr(lhs)?;
        self.visit_expr(rhs)
    }
    fn visit_boolean(&mut self, b: &mut bool) -> VisitorResult<T, E> {
        Ok(Continue(T::default()))
    }
    fn visit_field_access(&mut self, lhs: &mut Expr, rhs: &mut Symbol) -> VisitorResult<T, E> {
        self.visit_expr(lhs)?;
        self.visit_symbol(rhs)
    }
    fn visit_formula(&mut self, fmla: &mut Fmla) -> VisitorResult<T, E> {
        match fmla {
            Fmla::Forall(Forall { vars, fmla }) => {
                self.visit_vec(vars, |slf, v| slf.visit_param(v))?;
                self.visit_formula(fmla)
            }
            Fmla::Exists(Exists { vars, fmla }) => {
                self.visit_vec(vars, |slf, v| slf.visit_param(v))?;
                self.visit_formula(fmla)
            }
            Fmla::Pred(expr) => self.visit_expr(expr),
        }
    }
    fn visit_identifier(&mut self, ident: &mut Ident) -> VisitorResult<T, E> {
        Ok(Continue(T::default()))
    }
    fn visit_index(&mut self, idx: &mut IndexExpr) -> VisitorResult<T, E> {
        self.visit_expr(&mut idx.lhs)?;
        self.visit_expr(&mut idx.idx)
    }
    fn visit_number(&mut self, n: &mut i64) -> VisitorResult<T, E> {
        Ok(Continue(T::default()))
    }
    fn visit_param(&mut self, p: &mut Param) -> VisitorResult<T, E> {
        if let Some(sort) = &mut p.sort {
            self.visit_identifier(sort)?;
        }
        Ok(Continue(T::default()))
    }
    fn visit_unaryop(&mut self, op: &Verb, expr: &mut Expr) -> VisitorResult<T, E> {
        self.visit_expr(expr)
    }
    fn visit_symbol(&mut self, sym: &mut Symbol) -> VisitorResult<T, E> {
        Ok(Continue(T::default()))
    }
    fn visit_term(&mut self, term: &mut Term) -> VisitorResult<T, E> {
        self.visit_symbol(&mut term.id)?;
        match &mut term.sort {
            None => Ok(Continue(T::default())),
            Some(sort) => self.visit_identifier(sort),
        }
    }
    fn visit_this(&mut self) -> VisitorResult<T, E> {
        Ok(Continue(T::default()))
    }

    fn visit_expr(&mut self, e: &mut Expr) -> VisitorResult<T, E> {
        match e {
            Expr::App(app) => self.visit_app(app),
            Expr::BinOp { lhs, op, rhs } => self.visit_binop(lhs, op, rhs),
            Expr::Boolean(b) => self.visit_boolean(b),
            Expr::FieldAccess { record, field } => self.visit_field_access(record, field),
            Expr::Index(idx) => self.visit_index(idx),
            Expr::Number(i) => self.visit_number(i),
            Expr::Symbol(s) => self.visit_symbol(s),
            Expr::UnaryOp { op, expr } => self.visit_unaryop(op, expr),
            Expr::Term(t) => self.visit_term(t),
            Expr::This => self.visit_this(),
        }
    }

    fn visit_vec<FU, U>(&mut self, us: &mut Vec<U>, mut f: FU) -> VisitorResult<T, E>
    where
        FU: FnMut(&mut Self, &mut U) -> VisitorResult<T, E>,
    {
        let owned = std::mem::take(us);
        let mut t: Option<T> = None;

        let mut controls = owned
            .into_iter()
            .map(|mut elem| match f(self, &mut elem) {
                Ok(Continue(res)) => {
                    t = Some(res);
                    Ok(Some(elem))
                }
                Ok(Remove) => Ok(None),
                Err(e) => Err(e),
            })
            .collect::<Result<Vec<_>, E>>()?;
        controls.retain(|o| o.is_some());
        *us = controls.into_iter().map(|o| o.unwrap()).collect();

        // XXX: Remove if len(ts) == 0?
        // XXX: or if we never get a Some() value in u?
        match t {
            None => Ok(Continue(T::default())),
            Some(t) => Ok(Continue(t)),
        }
    }
}
