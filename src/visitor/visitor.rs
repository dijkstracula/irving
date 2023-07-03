#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]

use crate::ast::actions::*;
use crate::ast::declarations::*;
use crate::ast::expressions;
use crate::ast::expressions::*;
use crate::ast::logic::*;
use crate::ast::statements::*;
use crate::ast::toplevels::*;
use crate::typechecker::sorts::IvySort;

use super::control::ControlMut;
use super::VisitorResult;

pub trait Visitor<T>
where
    T: Default,
{
    // Top levels
    fn begin_prog(&mut self, _ast: &mut Prog) -> VisitorResult<T, Prog> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_prog(&mut self, _ast: &mut Prog) -> VisitorResult<T, Prog> {
        Ok(ControlMut::Produce(T::default()))
    }

    // Statements

    fn action_seq(&mut self, ast: &mut Vec<Action>) -> VisitorResult<T, Stmt> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_if(&mut self, _ast: &mut If) -> VisitorResult<T, Stmt> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_if(&mut self, _ast: &mut If) -> VisitorResult<T, Stmt> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_while(&mut self, _ast: &mut While) -> VisitorResult<T, Stmt> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_while(&mut self, _ast: &mut While) -> VisitorResult<T, Stmt> {
        Ok(ControlMut::Produce(T::default()))
    }

    // Actions
    fn begin_assert(&mut self, _ast: &mut AssertAction) -> VisitorResult<T, Action> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_assert(&mut self, _ast: &mut AssertAction) -> VisitorResult<T, Action> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_assign(&mut self, _ast: &mut AssignAction) -> VisitorResult<T, Action> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_assign(&mut self, _ast: &mut AssignAction) -> VisitorResult<T, Action> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_assume(&mut self, _ast: &mut AssumeAction) -> VisitorResult<T, Action> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_assume(&mut self, _ast: &mut AssumeAction) -> VisitorResult<T, Action> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_call(&mut self, _ast: &mut AppExpr) -> VisitorResult<T, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_call(&mut self, _ast: &mut AppExpr) -> VisitorResult<T, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_ensure(&mut self, _ast: &mut EnsureAction) -> VisitorResult<T, Action> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_ensure(&mut self, _ast: &mut EnsureAction) -> VisitorResult<T, Action> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_requires(&mut self, _ast: &mut RequiresAction) -> VisitorResult<T, Action> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_requires(&mut self, _ast: &mut RequiresAction) -> VisitorResult<T, Action> {
        Ok(ControlMut::Produce(T::default()))
    }

    // Declarations

    fn begin_action_decl(&mut self, _ast: &mut ActionDecl) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_action_decl(
        &mut self,
        _ast: &mut ActionDecl,
        _name: T,
        _params: Vec<T>,
        ret: Option<T>,
        _body: Option<Vec<T>>,
    ) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_after_decl(&mut self, _ast: &mut AfterDecl) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_after_decl(&mut self, _ast: &mut AfterDecl) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_axiom_decl(&mut self, _ast: &mut Fmla) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_axiom_decl(&mut self, _ast: &mut Fmla) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_alias_decl(&mut self, _sym: &mut Symbol, _e: &mut Expr) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_alias_decl(
        &mut self,
        _sym: &mut Symbol,
        _e: &mut Expr,
        _sym_res: T,
        _e_res: T,
    ) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_attribute_decl(&mut self, _ast: &mut Expr) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_attribute_decl(&mut self, _ast: &mut Expr) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_before_decl(&mut self, _ast: &mut BeforeDecl) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_before_decl(&mut self, _ast: &mut BeforeDecl) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_common_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_common_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_export_decl(&mut self, _ast: &mut ExportDecl) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_export_decl(&mut self, _ast: &mut ExportDecl) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_function_decl(&mut self, _ast: &mut FunctionDecl) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_function_decl(&mut self, _ast: &mut FunctionDecl) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_global_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_global_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_implement_decl(&mut self, _ast: &mut ImplementDecl) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_implement_decl(
        &mut self,
        _ast: &mut ImplementDecl,
        _name: T,
        _params: Option<Vec<T>>,
        ret: Option<T>,
        _body: Option<Vec<T>>,
    ) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_implementation_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_implementation_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_import_decl(&mut self, _ast: &mut ImportDecl) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_import_decl(
        &mut self,
        _ast: &mut ImportDecl,
        _n: T,
        _p: Vec<T>,
    ) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_include_decl(&mut self, _ast: &mut Symbol) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_include_decl(&mut self, _ast: &mut Symbol) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_instance_decl(&mut self, _ast: &mut InstanceDecl) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_instance_decl(&mut self, _ast: &mut InstanceDecl) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_invariant_decl(&mut self, _ast: &mut Fmla) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_invariant_decl(&mut self, _ast: &mut Fmla) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_isolate_decl(&mut self, _ast: &mut IsolateDecl) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_isolate_decl(
        &mut self,
        _ast: &mut IsolateDecl,
        _n: T,
        _p: Vec<T>,
        _b: Vec<T>,
    ) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_module_decl(&mut self, _ast: &mut ModuleDecl) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_module_decl(
        &mut self,
        _ast: &mut ModuleDecl,
        _n: T,
        _p: Vec<T>,
        _b: Vec<T>,
    ) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_normalized_module_decl(
        &mut self,
        _ast: &mut NormalizedIsolateDecl,
    ) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_normalized_module_decl(
        &mut self,
        _ast: &mut NormalizedIsolateDecl,
        _n: T,
        _p: Vec<T>,
        _i: Vec<T>,
        _s: Vec<T>,
        _ci: Vec<T>,
        _cs: Vec<T>,
    ) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_object_decl(&mut self, _ast: &mut ObjectDecl) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_object_decl(&mut self, _ast: &mut ObjectDecl) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_relation(&mut self, _ast: &mut Relation) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_relation(
        &mut self,
        _ast: &mut Relation,
        _n: T,
        _ps: Vec<T>,
    ) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_specification(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_specification(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_typedecl(&mut self, _ast: &mut Type) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_typedecl(&mut self, _ast: &mut Type) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_vardecl(&mut self, _ast: &mut Term) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_vardecl(
        &mut self,
        _ast: &mut Term,
        _id_t: T,
        _sort_t: Option<T>,
    ) -> VisitorResult<T, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    // Quantified formulas

    fn begin_exists(&mut self, _ast: &mut Exists) -> VisitorResult<T, Fmla> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_exists(&mut self, _ast: &mut Exists) -> VisitorResult<T, Fmla> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_forall(&mut self, _ast: &mut Forall) -> VisitorResult<T, Fmla> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_forall(&mut self, _ast: &mut Forall) -> VisitorResult<T, Fmla> {
        Ok(ControlMut::Produce(T::default()))
    }

    // Expressions

    fn begin_app(&mut self, _ast: &mut AppExpr) -> VisitorResult<T, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_app(&mut self, _ast: &mut AppExpr, _f: T, _args: Vec<T>) -> VisitorResult<T, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_binop(&mut self, _ast: &mut BinOp) -> VisitorResult<T, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_binop(
        &mut self,
        _ast: &mut BinOp,
        _lhs_ret: T,
        _op_ret: T,
        _rhs_ret: T,
    ) -> VisitorResult<T, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_field_access(&mut self, _lhs: &mut Expr, rhs: &mut Symbol) -> VisitorResult<T, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_field_access(&mut self, _lhs: &mut Expr, rhs: &mut Symbol) -> VisitorResult<T, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_index(&mut self, _ast: &mut IndexExpr) -> VisitorResult<T, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_index(&mut self, _lhs: &mut IndexExpr) -> VisitorResult<T, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_term(&mut self, _expr: &mut Term) -> VisitorResult<T, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_term(&mut self, _expr: &mut Term) -> VisitorResult<T, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_unary_op(&mut self, _op: &mut Verb, _rhs: &mut Expr) -> VisitorResult<T, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_unary_op(&mut self, _op: &mut Verb, _rhs: &mut Expr) -> VisitorResult<T, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }

    // Terminals

    fn boolean(&mut self, b: &mut bool) -> VisitorResult<T, bool> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn identifier(&mut self, _i: &mut Ident) -> VisitorResult<T, Ident> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn number(&mut self, _n: &mut i64) -> VisitorResult<T, i64> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn param(&mut self, _p: &mut Param) -> VisitorResult<T, Param> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn sort(&mut self, _s: &mut IvySort) -> VisitorResult<T, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn symbol(&mut self, _s: &mut Symbol) -> VisitorResult<T, Symbol> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn verb(&mut self, v: &mut Verb) -> VisitorResult<T, Verb> {
        Ok(ControlMut::Produce(T::default()))
    }
}

/// Something that can be visited by a Visitor.
pub trait Visitable<T, U = T>
where
    Self: Sized,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<U, Self>;
}

impl<T> Visitable<T> for Prog
where
    T: Default,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        visitor.begin_prog(self)?.and_then(|_| {
            let _d = self.top.visit(visitor)?.modifying(&mut self.top);
            visitor.finish_prog(self)
        })
    }
}

impl<T> Visitable<T> for Action
where
    T: Default,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        match self {
            Action::Assert(action) => visitor
                .begin_assert(action)?
                .map(|_| action.pred.visit(visitor)?.modifying(&mut action.pred))?
                .and_then(|_| visitor.finish_assert(action)),
            Action::Assign(action) => visitor
                .begin_assign(action)?
                .map::<_, T, expressions::Expr>(|_| {
                    action.lhs.visit(visitor)?.modifying(&mut action.lhs)
                })
                .map(|_| action.rhs.visit(visitor)?.modifying(&mut action.rhs))?
                .and_then(|_| visitor.finish_assign(action)),
            Action::Assume(action) => visitor
                .begin_assume(action)?
                .map(|_| action.pred.visit(visitor)?.modifying(&mut action.pred))?
                .and_then(|_| visitor.finish_assume(action)),
            Action::Call(action) => todo!(),
            /*
            visitor
                .begin_call(action)?
                .map(|_| action.func.visit(visitor)?.modifying(&mut action.func))?
                .map(|f| action.args.visit(visitor)?.modifying(&mut action.args))?
                .map(|a| visitor.finish_call(action)?.modifying(self)),
                */
            Action::Ensure(action) => visitor
                .begin_ensure(action)?
                .map(|_| action.pred.visit(visitor)?.modifying(&mut action.pred))?
                .and_then(|_| visitor.finish_ensure(action)),
            Action::Requires(action) => visitor
                .begin_requires(action)?
                .map(|_| action.pred.visit(visitor)?.modifying(&mut action.pred))?
                .and_then(|_| visitor.finish_requires(action)),
        }
    }
}

impl<T> Visitable<T> for Expr
where
    T: Default,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        let t = match self {
            Expr::App(expr) => visitor.begin_app(expr)?.and_then(|_| {
                let func = expr.func.visit(visitor)?.modifying(&mut expr.func)?;
                let args = expr.args.visit(visitor)?.modifying(&mut expr.args)?;
                visitor.finish_app(expr, func, args)
            }),
            Expr::BinOp(expr) => visitor.begin_binop(expr)?.and_then(|foo| {
                let l = expr.lhs.visit(visitor)?.modifying(&mut expr.lhs)?;
                let o = visitor.verb(&mut expr.op)?.modifying(&mut expr.op)?;
                let r = expr.rhs.visit(visitor)?.modifying(&mut expr.rhs)?;
                visitor.finish_binop(expr, l, o, r)
            }),
            Expr::Boolean(b) => visitor
                .boolean(b)?
                .modifying(b)
                .map(|t| ControlMut::Produce(t)),
            Expr::FieldAccess(FieldAccess {
                ref mut record,
                ref mut field,
            }) => visitor.begin_field_access(record, field)?.and_then(|_| {
                let r = record.visit(visitor)?.modifying(record);
                let f = field.visit(visitor)?.modifying(field);
                visitor.finish_field_access(record, field)
            }),
            Expr::Index(expr) => visitor
                .begin_index(expr)?
                .and_then(|_| expr.lhs.visit(visitor))?
                .and_then(|_| expr.idx.visit(visitor))?
                .and_then(|_| visitor.finish_index(expr)),
            Expr::Number(n) => {
                let t = visitor.number(n)?.modifying(n)?;
                Ok(ControlMut::Produce(t))
            }
            Expr::Symbol(i) => i
                .visit(visitor)?
                .modifying(i)
                .map(|t| ControlMut::Produce(t)),
            Expr::UnaryOp {
                ref mut op,
                ref mut expr,
            } => visitor.begin_unary_op(op, expr)?.and_then(|_| {
                let _ = visitor.verb(op)?.modifying(op)?;
                let _ = expr.visit(visitor)?.modifying(expr)?;
                visitor.finish_unary_op(op, expr)
            }),
            Expr::Term(t) => visitor.begin_term(t)?.and_then(|_| {
                let _ = t.id.visit(visitor)?.modifying(&mut t.id);
                let _ = t.sort.as_mut().map(|s| s.visit(visitor)?.modifying(s));
                visitor.finish_term(t)
            }),
            Expr::This => Expr::Symbol("this".into()).visit(visitor),
        }?
        .modifying(self)?;
        Ok(ControlMut::Produce(t))
    }
}

impl<T> Visitable<T> for Fmla
where
    T: Default,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        let t = match self {
            Fmla::Forall(fmla) => visitor.begin_forall(fmla)?.and_then(|_| {
                let _vars = fmla.vars.visit(visitor)?.modifying(&mut fmla.vars);
                let _flma = fmla.fmla.visit(visitor)?.modifying(&mut fmla.fmla);
                visitor.finish_forall(fmla)
            }),
            Fmla::Exists(fmla) => visitor.begin_exists(fmla)?.and_then(|_| {
                let _vars = fmla.vars.visit(visitor)?.modifying(&mut fmla.vars);
                let _flma = fmla.fmla.visit(visitor)?.modifying(&mut fmla.fmla);
                visitor.finish_exists(fmla)
            }),
            Fmla::Pred(expr) => expr
                .visit(visitor)?
                .modifying(expr)
                .map(|t| ControlMut::Produce(t)),
        }?
        .modifying(self)?;
        Ok(ControlMut::Produce(t))
    }
}

impl<T> Visitable<T> for Stmt
where
    T: Default,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        let t = match self {
            Stmt::ActionSequence(seq) => visitor.action_seq(seq),
            Stmt::If(stmt) => visitor.begin_if(stmt)?.and_then(|_| {
                let _test = stmt.tst.visit(visitor)?.modifying(&mut stmt.tst);
                let _then = stmt.thn.visit(visitor)?.modifying(&mut stmt.thn);
                let _else = stmt
                    .els
                    .as_mut()
                    .map(|stmts| stmts.visit(visitor)?.modifying(stmts));
                visitor.finish_if(stmt)
            }),
            Stmt::While(stmt) => visitor.begin_while(stmt)?.and_then(|_| {
                let _test = stmt.test.visit(visitor)?.modifying(&mut stmt.test);
                let _doit = stmt.doit.visit(visitor)?.modifying(&mut stmt.doit);
                visitor.finish_while(stmt)
            }),
        }?
        .modifying(self)?;
        Ok(ControlMut::Produce(t))
    }
}

impl<T> Visitable<T> for Decl
where
    T: Default,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        let t = match self {
            Decl::Action(decl) => visitor.begin_action_decl(decl)?.and_then(|_| {
                let n = visitor.symbol(&mut decl.name)?.modifying(&mut decl.name)?;
                let params = decl.params.visit(visitor)?.modifying(&mut decl.params)?;
                let ret = decl
                    .ret
                    .as_mut()
                    .map(|mut r| r.visit(visitor)?.modifying(&mut r))
                    .transpose()?;
                let body = decl
                    .body
                    .as_mut()
                    .map(|mut b| b.visit(visitor)?.modifying(&mut b))
                    .transpose()?;
                visitor.finish_action_decl(decl, n, params, ret, body)
            }),
            Decl::AfterAction(decl) => visitor.begin_after_decl(decl)?.and_then(|_| {
                let _n = decl.name.visit(visitor)?.modifying(&mut decl.name)?;
                let _p = decl
                    .params
                    .as_mut()
                    .map(|ps| ps.visit(visitor)?.modifying(ps));
                let _b = decl.body.visit(visitor)?.modifying(&mut decl.body)?;
                visitor.finish_after_decl(decl)
            }),
            Decl::Alias(sym, expr) => visitor.begin_alias_decl(sym, expr)?.and_then(|_| {
                let s = sym.visit(visitor)?.modifying(sym)?;
                let e = expr.visit(visitor)?.modifying(expr)?;
                visitor.finish_alias_decl(sym, expr, s, e)
            }),
            Decl::Attribute(decl) => visitor.begin_attribute_decl(decl)?.and_then(|_| {
                let _d = decl.visit(visitor)?.modifying(decl);
                visitor.finish_attribute_decl(decl)
            }),
            Decl::Axiom(fmla) => visitor.begin_axiom_decl(fmla)?.and_then(|_| {
                let _f = fmla.visit(visitor)?.modifying(fmla);
                visitor.finish_axiom_decl(fmla)
            }),
            Decl::BeforeAction(decl) => visitor.begin_before_decl(decl)?.and_then(|_| {
                let _n = decl.name.visit(visitor)?.modifying(&mut decl.name);
                let _p = decl.params.as_mut().map(|p| p.visit(visitor)?.modifying(p));
                let _b = decl.body.visit(visitor)?.modifying(&mut decl.body);
                visitor.finish_before_decl(decl)
            }),
            Decl::Common(decl) => visitor.begin_common_decl(decl)?.and_then(|_| {
                let _d = decl.visit(visitor)?.modifying(decl)?;
                visitor.finish_common_decl(decl)
            }),
            Decl::Export(decl) => visitor.begin_export_decl(decl)?.and_then(|_| match decl {
                ExportDecl::Action(decl) => visitor.begin_action_decl(decl)?.and_then(|_| {
                    let n = visitor.symbol(&mut decl.name)?.modifying(&mut decl.name)?;
                    let params = decl.params.visit(visitor)?.modifying(&mut decl.params)?;
                    let ret = decl
                        .ret
                        .as_mut()
                        .map(|mut r| r.visit(visitor)?.modifying(&mut r))
                        .transpose()?;
                    let body = decl
                        .body
                        .as_mut()
                        .map(|mut b| b.visit(visitor)?.modifying(&mut b))
                        .transpose()?;
                    visitor.finish_action_decl(decl, n, params, ret, body)
                }),
                ExportDecl::ForwardRef(sym) => sym
                    .visit(visitor)?
                    .modifying(sym)
                    .map(|t| ControlMut::Produce(t)),
            }),
            Decl::Function(_) => todo!(),
            Decl::Globals(decl) => visitor.begin_global_decl(decl)?.and_then(|_| {
                let _d = decl.visit(visitor)?.modifying(decl);
                visitor.finish_global_decl(decl)
            }),
            Decl::Implement(decl) => visitor.begin_implement_decl(decl)?.and_then(|_| {
                let n = decl.name.visit(visitor)?.modifying(&mut decl.name)?;
                let params = decl
                    .params
                    .as_mut()
                    .map(|mut p| p.visit(visitor)?.modifying(&mut p))
                    .transpose()?;
                let ret = decl
                    .ret
                    .as_mut()
                    .map(|mut r| r.visit(visitor)?.modifying(&mut r))
                    .transpose()?;
                let body = decl
                    .body
                    .as_mut()
                    .map(|mut b| b.visit(visitor)?.modifying(&mut b))
                    .transpose()?;

                visitor.finish_implement_decl(decl, n, params, ret, body)
            }),
            Decl::Implementation(decl) => visitor.begin_implementation_decl(decl)?.and_then(|_| {
                let _d = decl.visit(visitor)?.modifying(decl)?;
                visitor.finish_implementation_decl(decl)
            }),
            Decl::Import(decl) => visitor.begin_import_decl(decl)?.and_then(|_| {
                let n = decl.name.visit(visitor)?.modifying(&mut decl.name)?;
                let p = decl.params.visit(visitor)?.modifying(&mut decl.params)?;
                visitor.finish_import_decl(decl, n, p)
            }),
            Decl::Isolate(decl) => visitor.begin_isolate_decl(decl)?.and_then(|_| {
                let _n = decl.name.visit(visitor)?.modifying(&mut decl.name)?;
                let _p = decl.params.visit(visitor)?.modifying(&mut decl.params)?;
                let _b = decl.body.visit(visitor)?.modifying(&mut decl.body)?;
                visitor.finish_isolate_decl(decl, _n, _p, _b)
            }),
            Decl::Include(decl) => visitor
                .begin_include_decl(decl)?
                .and_then(|_| visitor.finish_include_decl(decl)),
            Decl::Instance(decl) => visitor.begin_instance_decl(decl)?.and_then(|_| {
                let _n = decl.name.visit(visitor)?.modifying(&mut decl.name);
                let _s = decl.sort.visit(visitor)?.modifying(&mut decl.sort);
                let _a = decl.args.visit(visitor)?.modifying(&mut decl.args);
                visitor.finish_instance_decl(decl)
            }),
            Decl::Instantiate { name, prms } => todo!(),
            Decl::Interpretation { itype, ctype } => todo!(),
            Decl::Invariant(decl) => visitor.begin_invariant_decl(decl)?.and_then(|_| {
                let _f = decl.visit(visitor)?.modifying(decl);
                visitor.finish_invariant_decl(decl)
            }),
            Decl::Module(decl) => visitor.begin_module_decl(decl)?.and_then(|_| {
                let n = decl.name.visit(visitor)?.modifying(&mut decl.name)?;
                let p = decl
                    .params
                    .iter_mut()
                    .map(|p| p.visit(visitor)?.modifying(p))
                    .collect::<Result<Vec<_>, _>>()?;
                let b = decl.body.visit(visitor)?.modifying(&mut decl.body)?;
                visitor.finish_module_decl(decl, n, p, b)
            }),
            Decl::Noop => return Ok(ControlMut::Produce(T::default())),
            Decl::NormalizedIsolate(decl) => {
                visitor.begin_normalized_module_decl(decl)?.and_then(|_| {
                    let n = decl.name.visit(visitor)?.modifying(&mut decl.name)?;
                    let p = decl.params.visit(visitor)?.modifying(&mut decl.params)?;

                    let i = decl
                        .impl_decls
                        .visit(visitor)?
                        .modifying(&mut decl.impl_decls)?;
                    let s = decl
                        .spec_decls
                        .visit(visitor)?
                        .modifying(&mut decl.spec_decls)?;
                    let ci = decl
                        .common_impl_decls
                        .visit(visitor)?
                        .modifying(&mut decl.impl_decls)?;
                    let cs = decl
                        .common_spec_decls
                        .visit(visitor)?
                        .modifying(&mut decl.spec_decls)?;
                    visitor.finish_normalized_module_decl(decl, n, p, i, s, ci, cs)
                })
            }
            Decl::Object(decl) => visitor.begin_object_decl(decl)?.and_then(|_| {
                let _n = decl.name.visit(visitor)?.modifying(&mut decl.name);
                let _p = decl.params.visit(visitor)?.modifying(&mut decl.params);
                let _b = decl.body.visit(visitor)?.modifying(&mut decl.body);
                visitor.finish_object_decl(decl)
            }),
            Decl::Relation(decl) => visitor.begin_relation(decl)?.and_then(|_| {
                let n = decl.name.visit(visitor)?.modifying(&mut decl.name)?;
                let p = decl.params.visit(visitor)?.modifying(&mut decl.params)?;
                visitor.finish_relation(decl, n, p)
            }),
            Decl::Specification(decl) => visitor.begin_specification(decl)?.and_then(|_| {
                let _d = decl.visit(visitor)?.modifying(decl)?;
                visitor.finish_specification(decl)
            }),
            Decl::Stmts(stmts) => {
                let mut _t = stmts.visit(visitor)?.modifying(stmts)?;
                Ok(ControlMut::Produce(T::default()))
            }
            Decl::Var(decl) => visitor.begin_vardecl(decl)?.and_then(|_| {
                let id = decl.id.visit(visitor)?.modifying(&mut decl.id)?;
                let sort = decl
                    .sort
                    .as_mut()
                    .map(|s| s.visit(visitor)?.modifying(s))
                    .transpose()?;
                visitor.finish_vardecl(decl, id, sort)
            }),
            Decl::Type(decl) => visitor.begin_typedecl(decl)?.and_then(|_| {
                let _i = match &mut decl.ident {
                    TypeName::Name(n) => n.visit(visitor)?.modifying(n),
                    TypeName::This => todo!(),
                }?;
                let _s = visitor.sort(&mut decl.sort)?.modifying(&mut decl.sort);
                visitor.finish_typedecl(decl)
            }),
        }?
        .modifying(self)?;
        Ok(ControlMut::Produce(t))
    }
}

impl<T> Visitable<T> for Symbol
where
    T: Default,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        visitor.symbol(self)
    }
}

impl<T> Visitable<T> for Ident
where
    T: Default,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        visitor.identifier(self)
    }
}

impl<T> Visitable<T> for Param
where
    T: Default,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        visitor.param(self)
    }
}

// Implementations for compound nodes

impl<T> Visitable<T, Vec<T>> for Vec<Expr>
where
    T: Default,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<Vec<T>, Self> {
        let mut res = vec![];
        for node in self {
            match node.visit(visitor)? {
                ControlMut::Produce(t) => res.push(t),
                ControlMut::SkipSiblings(t) => {
                    res.push(t);
                    break;
                }
                ControlMut::Mutation(repl, t) => {
                    *node = repl;
                    res.push(t);
                }
            };
        }
        Ok(ControlMut::Produce(res))
    }
}

impl<T> Visitable<T, Vec<T>> for Vec<Stmt>
where
    T: Default,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<Vec<T>, Self> {
        let mut res = vec![];
        for node in self {
            match node.visit(visitor)? {
                ControlMut::Produce(t) => res.push(t),
                ControlMut::SkipSiblings(t) => {
                    res.push(t);
                    break;
                }
                ControlMut::Mutation(Stmt::ActionSequence(actions), t) if actions.len() == 0 => {
                    continue;
                }
                ControlMut::Mutation(repl, t) => {
                    *node = repl;
                    res.push(t);
                }
            };
        }
        Ok(ControlMut::Produce(res))
    }
}

impl<T> Visitable<T, Vec<T>> for Vec<Decl>
where
    T: Default,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<Vec<T>, Self> {
        let mut res = vec![];
        for node in self {
            match node {
                Decl::Noop => continue,
                _ => (),
            };
            match node.visit(visitor)? {
                ControlMut::Produce(t) => res.push(t),
                ControlMut::SkipSiblings(t) => {
                    res.push(t);
                    break;
                }
                ControlMut::Mutation(Decl::Common(decls), t)
                | ControlMut::Mutation(Decl::Globals(decls), t)
                | ControlMut::Mutation(Decl::Implementation(decls), t)
                | ControlMut::Mutation(Decl::Specification(decls), t)
                    if decls.len() == 0 =>
                {
                    continue;
                }
                ControlMut::Mutation(repl, t) => {
                    *node = repl;
                    res.push(t);
                }
            };
        }
        Ok(ControlMut::Produce(res))
    }
}

impl<T> Visitable<T, Vec<T>> for Vec<Action>
where
    T: Default,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<Vec<T>, Self> {
        let mut res = vec![];
        for node in self {
            match node.visit(visitor)? {
                ControlMut::Produce(t) => res.push(t),
                ControlMut::SkipSiblings(t) => {
                    res.push(t);
                    break;
                }
                ControlMut::Mutation(repl, t) => {
                    *node = repl;
                    res.push(t);
                }
            };
        }
        Ok(ControlMut::Produce(res))
    }
}

impl<T> Visitable<T, Vec<T>> for Vec<Param>
where
    T: Default,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<Vec<T>, Self> {
        let mut res = vec![];
        for node in self {
            match node.visit(visitor)? {
                ControlMut::Produce(t) => res.push(t),
                ControlMut::SkipSiblings(t) => {
                    res.push(t);
                    break;
                }
                ControlMut::Mutation(repl, t) => {
                    *node = repl;
                    res.push(t);
                }
            };
        }
        Ok(ControlMut::Produce(res))
    }
}
