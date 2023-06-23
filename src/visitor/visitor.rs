#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]

use crate::ast::actions::*;
use crate::ast::declarations::*;
use crate::ast::expressions::*;
use crate::ast::logic::*;
use crate::ast::statements::*;
use crate::ast::toplevels::*;
use crate::typechecker::sorts::IvySort;

/// Action performed after visiting a node of type Node.

pub enum Control<T, Node> {
    /// Hand back a value to the caller
    Produce(T),

    /// Skip traversing the nodes' siblings
    SkipSiblings(T),

    Mutation(Node, T),

/* 
    /// Swaps out one expr for another.
    ChangeExpr(Expr, T),

    /// Swaps out one stmt for another.
    ChangeStmt(Stmt, T),

    /// Swaps out one decl for another.
    ChangeDecl(Decl, T),

    /// Swaps out one action for another.
    ChangeAction(Action, T),

    /// Swaps out one param for another.
    ChangeParam(Param, T),
    */
}

/*
impl <T> Control<Vec<T>> {
    pub fn collect<F>(self, mut f: F) -> VisitorResult<T> 
    where
        F: FnMut(Vec<T>) -> VisitorResult<T>
    {
        match self {
            Control::Produce(xs) => f(xs),
            _ => unreachable!()
        }
    }
}
*/

impl <T, Node> Control<T, Node> {
    /// Runs the thunk if we received `Produce` from the Visitor.
    pub fn and_then<F>(self, mut next: F) -> VisitorResult<T, Node>
    where
        // TODO: Thread through the T value??
        F: FnMut(T) -> VisitorResult<T, Node>,
    {
        match self {
            Control::Produce(t) => next(t),
            ctrl => Ok(ctrl),
        }
    }

    pub fn map<F, U>(self, mut f: F) -> VisitorResult<U, Node>
    where
        // TODO: Thread through the T value??
        F: FnMut(T) -> VisitorResult<U, Node>,
    {
        match self {
            Control::Produce(t) => f(t),
            Control::SkipSiblings(t) => f(t),
            Control::Mutation(_, t) => f(t),
        }
    }

    pub fn modifying(self, target: &mut Node) -> VisitorResult<T, Node> {
        Ok(match self {
            Control::Produce(_) => self,
            Control::SkipSiblings(_) => self,
            Control::Mutation(repl, t) => {
                *target = repl;
                Control::Produce(t)
            }
        })
    }

    /*  
    /// Mutates the node if we received `Change` from the Visitor.
    pub fn change_expr(self, target: &mut Expr) -> VisitorResult<T> {
        Ok(match self {
            Control::ChangeExpr(modified, t) => {
                *target = modified;
                Self::Produce(t)
            }
            ctrl => ctrl,
        })
    }

    /// Mutates the node if we received `Change` from the Visitor.
    pub fn change_stmt(self, target: &mut Stmt) -> VisitorResult<T> {
        Ok(match self {
            Control::ChangeStmt(modified, t) => {
                *target = modified;
                Self::Produce(t)
            }
            ctrl => ctrl,
        })
    }

    /// Mutates the node if we received `Change` from the Visitor.
    pub fn change_decl(self, target: &mut Decl) -> VisitorResult<T> {
        Ok(match self {
            Control::ChangeDecl(modified, t) => {
                *target = modified;
                Self::Produce(t)
            }
            ctrl => ctrl,
        })
    }
    /// Delineates when we've finished traversing the children of a node.
    pub fn children_end(self) -> Self {
        match self {
            Control::SkipSiblings(t) => Control::Produce(t),
            ctrl => ctrl,
        }
    }
    */
}

pub type VisitorResult<T, Node> = anyhow::Result<Control<T, Node>>;

/* TODO: deprecate Visitor and rename this. */
pub trait Visitor<T> 
where
T: Default
{
    // Top levels
    fn begin_prog(&mut self, _ast: &mut Prog) -> VisitorResult<T, Prog> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_prog(&mut self, _ast: &mut Prog) -> VisitorResult<T, Prog> {
        Ok(Control::Produce(T::default()))
    }

    // Statements

    fn action_seq(&mut self, ast: &mut Vec<Action>) -> VisitorResult<T, Vec<Action>> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_if(&mut self, _ast: &mut If) -> VisitorResult<T, Stmt> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_if(&mut self, _ast: &mut If) -> VisitorResult<T, Stmt> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_while(&mut self, _ast: &mut While) -> VisitorResult<T, Stmt> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_while(&mut self, _ast: &mut While) -> VisitorResult<T, Stmt> {
        Ok(Control::Produce(T::default()))
    }

    // Actions
    fn begin_assert(&mut self, _ast: &mut AssertAction) -> VisitorResult<T, Action> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_assert(&mut self, _ast: &mut AssertAction) -> VisitorResult<T, Action> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_assign(&mut self, _ast: &mut AssignAction) -> VisitorResult<T, Action> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_assign(&mut self, _ast: &mut AssignAction) -> VisitorResult<T, Action> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_assume(&mut self, _ast: &mut AssumeAction) -> VisitorResult<T, Action> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_assume(&mut self, _ast: &mut AssumeAction) -> VisitorResult<T, Action> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_call(&mut self, _ast: &mut AppExpr) -> VisitorResult<T, Expr> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_call(&mut self, _ast: &mut AppExpr) -> VisitorResult<T, Expr> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_ensure(&mut self, _ast: &mut EnsureAction) -> VisitorResult<T, Action> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_ensure(&mut self, _ast: &mut EnsureAction) -> VisitorResult<T, Action> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_requires(&mut self, _ast: &mut RequiresAction) -> VisitorResult<T, Action> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_requires(&mut self, _ast: &mut RequiresAction) -> VisitorResult<T, Action> {
        Ok(Control::Produce(T::default()))
    }

    // Declarations

    fn begin_action_decl(&mut self, _ast: &mut ActionDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_action_decl(&mut self, _ast: &mut ActionDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_after_decl(&mut self, _ast: &mut AfterDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_after_decl(&mut self, _ast: &mut AfterDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_axiom_decl(&mut self, _ast: &mut Fmla) -> VisitorResult<T, Fmla> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_axiom_decl(&mut self, _ast: &mut Fmla) -> VisitorResult<T, Fmla> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_attribute_decl(&mut self, _ast: &mut Expr) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_attribute_decl(&mut self, _ast: &mut Expr) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_before_decl(&mut self, _ast: &mut BeforeDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_before_decl(&mut self, _ast: &mut BeforeDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_common_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_common_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_export_decl(&mut self, _ast: &mut ExportDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_export_decl(&mut self, _ast: &mut ExportDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_function_decl(&mut self, _ast: &mut FunctionDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_function_decl(&mut self, _ast: &mut FunctionDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_global_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_global_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_implement_decl(&mut self, _ast: &mut ImplementDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_implement_decl(&mut self, _ast: &mut ImplementDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_implementation_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_implementation_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_import_decl(&mut self, _ast: &mut ImportDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_import_decl(&mut self, _ast: &mut ImportDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_include_decl(&mut self, _ast: &mut Symbol) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_include_decl(&mut self, _ast: &mut Symbol) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_instance_decl(&mut self, _ast: &mut InstanceDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_instance_decl(&mut self, _ast: &mut InstanceDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_invariant_decl(&mut self, _ast: &mut Fmla) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_invariant_decl(&mut self, _ast: &mut Fmla) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_isolate_decl(&mut self, _ast: &mut IsolateDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_isolate_decl(&mut self, _ast: &mut IsolateDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_module_decl(&mut self, _ast: &mut ModuleDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_module_decl(&mut self, _ast: &mut ModuleDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_object_decl(&mut self, _ast: &mut ObjectDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_object_decl(&mut self, _ast: &mut ObjectDecl) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_relation(&mut self, _ast: &mut Relation) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_relation(&mut self, _ast: &mut Relation) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_specification(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_specification(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_typedecl(&mut self, _ast: &mut Type) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_typedecl(&mut self, _ast: &mut Type) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_vardecl(&mut self, _ast: &mut Term) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_vardecl(&mut self, _ast: &mut Term) -> VisitorResult<T, Decl> {
        Ok(Control::Produce(T::default()))
    }

    // Quantified formulas

    fn begin_exists(&mut self, _ast: &mut Exists) -> VisitorResult<T, Fmla> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_exists(&mut self, _ast: &mut Exists) -> VisitorResult<T, Fmla> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_forall(&mut self, _ast: &mut Forall) -> VisitorResult<T, Fmla> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_forall(&mut self, _ast: &mut Forall) -> VisitorResult<T, Fmla> {
        Ok(Control::Produce(T::default()))
    }

    // Expressions

    fn begin_app(&mut self, _ast: &mut AppExpr) -> VisitorResult<T, Expr> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_app(&mut self, _ast: &mut AppExpr) -> VisitorResult<T, Expr> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_binop(&mut self, _ast: &mut BinOp) -> VisitorResult<T, Expr> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_binop(&mut self, _ast: &mut BinOp, _lhs_ret: T, _op_ret: T, _rhs_ret: T) -> VisitorResult<T, Expr> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_field_access(&mut self, _lhs: &mut Expr, rhs: &mut Symbol) -> VisitorResult<T, Expr> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_field_access(&mut self, _lhs: &mut Expr, rhs: &mut Symbol) -> VisitorResult<T, Expr> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_index(&mut self, _ast: &mut IndexExpr) -> VisitorResult<T, Expr> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_index(&mut self, _lhs: &mut IndexExpr) -> VisitorResult<T, Expr> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_term(&mut self, _expr: &mut Term) -> VisitorResult<T, Expr> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_term(&mut self, _expr: &mut Term) -> VisitorResult<T, Expr> {
        Ok(Control::Produce(T::default()))
    }

    fn begin_unary_op(&mut self, _op: &mut Verb, _rhs: &mut Expr) -> VisitorResult<T, Expr> {
        Ok(Control::Produce(T::default()))
    }
    fn finish_unary_op(&mut self, _op: &mut Verb, _rhs: &mut Expr) -> VisitorResult<T, Expr> {
        Ok(Control::Produce(T::default()))
    }

    // Terminals

    fn boolean(&mut self, b: &mut bool) -> VisitorResult<T, bool> {
        Ok(Control::Produce(T::default()))
    }

    fn identifier(&mut self, _i: &mut Ident) -> VisitorResult<T, Ident> {
        Ok(Control::Produce(T::default()))
    }

    fn number(&mut self, _n: &mut i64) -> VisitorResult<T, i64> {
        Ok(Control::Produce(T::default()))
    }

    fn param(&mut self, _p: &mut Param) -> VisitorResult<T, Param> {
        Ok(Control::Produce(T::default()))
    }

    fn sort(&mut self, _s: &mut IvySort) -> VisitorResult<T, IvySort> {
        Ok(Control::Produce(T::default()))
    }

    fn symbol(&mut self, _s: &mut Symbol) -> VisitorResult<T, Symbol> {
        Ok(Control::Produce(T::default()))
    }

    fn verb(&mut self, v: &mut Verb) -> VisitorResult<T, Verb> {
        Ok(Control::Produce(T::default()))
    }
}

/// Something that can be visited by a Visitor.
pub trait Visitable<T> where Self: Sized {
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self>;
}

impl <T> Visitable<T> for Prog 
where T: Default
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        let foo = visitor
            .begin_prog(self)?
            /* Note: don't visit the name or args for this special decl */
            .map(|_| self.top.body.visit(visitor))
            
        fo
    }
}

impl <T> Visitable<T> for Action 
where T: Default
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        match self {
            Action::Assert(action) => visitor
                .begin_assert(action)?
                .and_then(|_| action.pred.visit(visitor))?
                .and_then(|_| visitor.finish_assert(action)),
            Action::Assign(action) => visitor
                .begin_assign(action)?
                .and_then(|_| action.lhs.visit(visitor))?
                .and_then(|_| action.rhs.visit(visitor))?
                .and_then(|_| visitor.finish_assign(action)),
            Action::Assume(action) => visitor
                .begin_assume(action)?
                .and_then(|_| action.pred.visit(visitor))?
                .and_then(|_| visitor.finish_assume(action)),
            Action::Call(action) => visitor
                .begin_call(action)?
                .and_then(|_| action.func.visit(visitor))?
                .and_then(|f| action.args.visit(visitor))?
                .and_then(|a| visitor.finish_call(action)),
            Action::Ensure(action) => visitor
                .begin_ensure(action)?
                .and_then(|_| action.pred.visit(visitor))?
                .and_then(|_| visitor.finish_ensure(action)),
            Action::Requires(action) => visitor
                .begin_requires(action)?
                .and_then(|_| action.pred.visit(visitor))?
                .and_then(|_| visitor.finish_requires(action)),
        }
    }
}

impl <T> Visitable<T> for Expr 
where T: Default
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        match self {
            Expr::App(expr) => visitor
                .begin_app(expr)?
                .and_then(|_| expr.func.visit(visitor))?
                .and_then(|_| expr.args.visit(visitor))?
                .and_then(|_| visitor.finish_app(expr)),
            Expr::BinOp(expr) => visitor.begin_binop(expr)
                .and_then(|_| expr.lhs.visit(visitor)?.modifying(&mut expr.lhs)?
                .and_then(|o| expr.rhs.visit(visitor)?.modifying(&mut expr.rhs)?
                .and_then(|r| visitor.finish_binop(expr, l, o, r)))),
            Expr::Boolean(b) => visitor.boolean(b),
            Expr::FieldAccess { record, field } => visitor
                .begin_field_access(record, field)?
                .and_then(|_| record.visit(visitor))?
                .and_then(|_| field.visit(visitor))?
                .and_then(|_| visitor.finish_field_access(record, field)),
            Expr::Index(expr) => visitor
                .begin_index(expr)?
                .and_then(|_| expr.lhs.visit(visitor))?
                .and_then(|_| expr.idx.visit(visitor))?
                .and_then(|_| visitor.finish_index(expr)),
            Expr::Number(n) => visitor.number(n),
            Expr::Symbol(i) => visitor.symbol(i),
            Expr::UnaryOp { op, expr } => visitor
                .begin_unary_op(op, expr)?
                .and_then(|_| visitor.verb(op))?
                .and_then(|_| expr.visit(visitor))?
                .and_then(|_| visitor.finish_unary_op(op, expr)),
            Expr::Term(t) => visitor
                .begin_term(t)?
                .and_then(|_| t.id.visit(visitor))?
                .and_then(|_| t.sort.visit(visitor))?
                .and_then(|_| visitor.finish_term(t)),
            Expr::This => todo!(),
        }?
        .change_expr(self)
    }
}

impl <T> Visitable<T> for Fmla 
where T: Default
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        match self {
            Fmla::Forall(fmla) => visitor
                .begin_forall(fmla)?
                .and_then(|_| fmla.vars.visit(visitor))?
                .and_then(|_| fmla.fmla.visit(visitor))?
                .and_then(|_| visitor.finish_forall(fmla)),
            Fmla::Exists(fmla) => visitor
                .begin_exists(fmla)?
                .and_then(|_| fmla.vars.visit(visitor))?
                .and_then(|_| fmla.fmla.visit(visitor))?
                .and_then(|_| visitor.finish_exists(fmla)),
            Fmla::Pred(expr) => expr.visit(visitor),
        }
    }
}

impl <T> Visitable<T> for Stmt 
where T: Default
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        match self {
            Stmt::ActionSequence(seq) => visitor.action_seq(seq),
            Stmt::If(stmt) => visitor
                .begin_if(stmt)?
                .and_then(|_| stmt.tst.visit(visitor))?
                .and_then(|_| stmt.thn.visit(visitor))?
                .and_then(|_| stmt.els.visit(visitor))?
                .children_end()
                .and_then(|_| visitor.finish_if(stmt)),
            Stmt::While(stmt) => visitor
                .begin_while(stmt)?
                .and_then(|_| stmt.test.visit(visitor))?
                .and_then(|_| stmt.doit.visit(visitor))?
                .children_end()
                .and_then(|_| visitor.finish_while(stmt)),
        }?
        .change_stmt(self)
    }
}

impl <T> Visitable<T> for Decl 
where T: Default
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        match self {
            Decl::Action(decl) => visitor
                .begin_action_decl(decl)?
                .and_then(|_| visitor.symbol(&mut decl.name))?
                .and_then(|_| decl.params.visit(visitor))?
                .and_then(|_| visitor.finish_action_decl(decl)),
            Decl::AfterAction(decl) => visitor
                .begin_after_decl(decl)?
                .and_then(|_| decl.name.visit(visitor))?
                .and_then(|_| decl.params.visit(visitor))?
                .and_then(|_| decl.body.visit(visitor))?
                .and_then(|_| visitor.finish_after_decl(decl)),
            Decl::Alias(_, _) => todo!(),
            Decl::Attribute(decl) => visitor
                .begin_attribute_decl(decl)?
                .and_then(|_| decl.visit(visitor))?
                .and_then(|_| visitor.finish_attribute_decl(decl)),
            Decl::Axiom(fmla) => visitor
                .begin_axiom_decl(fmla)?
                .and_then(|_| fmla.visit(visitor))?
                .and_then(|_| visitor.finish_axiom_decl(fmla)),
            Decl::BeforeAction(decl) => visitor
                .begin_before_decl(decl)?
                .and_then(|_| decl.name.visit(visitor))?
                .and_then(|_| decl.params.visit(visitor))?
                .and_then(|_| decl.body.visit(visitor))?
                .and_then(|_| visitor.finish_before_decl(decl)),
            Decl::Common(decl) => visitor
                .begin_common_decl(decl)?
                .and_then(|_| decl.visit(visitor))?
                .and_then(|_| visitor.finish_common_decl(decl)),
            Decl::Export(decl) => visitor
                .begin_export_decl(decl)?
                .and_then(|_| match decl {
                    ExportDecl::Action(decl) => visitor
                        .begin_action_decl(decl)?
                        .and_then(|_| visitor.symbol(&mut decl.name))?
                        .and_then(|_| decl.params.visit(visitor))?
                        .and_then(|_| visitor.finish_action_decl(decl)),
                    ExportDecl::ForwardRef(sym) => sym.visit(visitor),
                })?
                .and_then(|_| visitor.finish_export_decl(decl)),
            Decl::Function(_) => todo!(),
            Decl::Globals(decl) => visitor
                .begin_global_decl(decl)?
                .and_then(|_| decl.visit(visitor))?
                .and_then(|_| visitor.finish_global_decl(decl)),
            Decl::Implement(_) => todo!(),
            Decl::Implementation(decl) => visitor
                .begin_implementation_decl(decl)?
                .and_then(|_| decl.visit(visitor))?
                .and_then(|_| visitor.finish_implementation_decl(decl)),
            Decl::Import(decl) => visitor
                .begin_import_decl(decl)?
                .and_then(|_| decl.name.visit(visitor))?
                .and_then(|_| decl.params.visit(visitor))?
                .and_then(|_| visitor.finish_import_decl(decl)),
            Decl::Isolate(decl) => visitor
                .begin_isolate_decl(decl)?
                .and_then(|_| decl.name.visit(visitor))?
                .and_then(|_| decl.params.visit(visitor))?
                .and_then(|_| decl.body.visit(visitor))?
                .and_then(|_| visitor.finish_isolate_decl(decl)),
            Decl::Include(decl) => visitor
                .begin_include_decl(decl)?
                .and_then(|_| visitor.finish_include_decl(decl)),
            Decl::Instance(decl) => visitor
                .begin_instance_decl(decl)?
                .and_then(|_| decl.name.visit(visitor))?
                .and_then(|_| decl.sort.visit(visitor))?
                .and_then(|_| decl.args.visit(visitor))?
                .and_then(|_| visitor.finish_instance_decl(decl)),
            Decl::Instantiate { name, prms } => todo!(),
            Decl::Interpretation { itype, ctype } => todo!(),
            Decl::Invariant(decl) => visitor
                .begin_invariant_decl(decl)?
                .and_then(|_| decl.visit(visitor))?
                .and_then(|_| visitor.finish_invariant_decl(decl)),
            Decl::Module(decl) => visitor
                .begin_module_decl(decl)?
                .and_then(|_| decl.name.visit(visitor))?
                .and_then(|_| decl.params.visit(visitor))?
                .and_then(|_| decl.body.visit(visitor))?
                .and_then(|_| visitor.finish_module_decl(decl)),
            Decl::Object(decl) => visitor
                .begin_object_decl(decl)?
                .and_then(|_| decl.name.visit(visitor))?
                .and_then(|_| decl.params.visit(visitor))?
                .and_then(|_| decl.body.visit(visitor))?
                .and_then(|_| visitor.finish_object_decl(decl)),
            Decl::Relation(decl) => visitor
                .begin_relation(decl)?
                .and_then(|_| decl.name.visit(visitor))?
                .and_then(|_| decl.params.visit(visitor))?
                .and_then(|_| visitor.finish_relation(decl)),
            Decl::Specification(decl) => visitor
                .begin_specification(decl)?
                .and_then(|_| decl.visit(visitor))?
                .and_then(|_| visitor.finish_specification(decl)),
            Decl::Stmts(stmts) => stmts.visit(visitor),
            Decl::Var(decl) => visitor
                .begin_vardecl(decl)?
                .and_then(|_| decl.id.visit(visitor))?
                .and_then(|_| decl.sort.visit(visitor))?
                .and_then(|_| visitor.finish_vardecl(decl)),
            Decl::Type(decl) => visitor
                .begin_typedecl(decl)?
                .and_then(|_| match &mut decl.ident {
                    TypeName::Name(n) => n.visit(visitor),
                    TypeName::This => todo!(),
                })?
                .and_then(|_| visitor.sort(&mut decl.sort))?
                .and_then(|_| visitor.finish_typedecl(decl)),
        }?
        .change_decl(self)
    }
}

impl <T> Visitable<T> for Symbol
where T: Default
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        visitor.symbol(self)
    }
}

impl <T> Visitable<T> for Ident 
where T: Default
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        visitor.identifier(self)
    }
}

impl <T> Visitable<T> for Param 
where T: Default
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        visitor.param(self)
    }
}

// Implementations for compound nodes

impl <T> Visitable<T> for Vec<Expr> 
where T: Default
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        let mut res = vec!();
        for node in self {
            match node.visit(visitor)? {
                Control::Produce(t) => res.push(t),
                Control::SkipSiblings(t) => {
                    res.push(t);
                    break;
                }
                Control::Mutation(repl, t) => {
                    *node = repl;
                    res.push(t);
                }
            };
        }
        Ok(Control::Produce(res))
    }
}

impl <T> Visitable<T> for Vec<Stmt> 
where T: Default
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        let mut res = vec!();
        for node in self {
            match node.visit(visitor)? {
                Control::Produce(t) => res.push(t),
                Control::SkipSiblings(t) => {
                    res.push(t);
                    break;
                }
                Control::Mutation(repl, t) => {
                    *node = repl;
                    res.push(t);
                }
            };
        }
        Ok(Control::Produce(res))
    }
}

impl <T> Visitable<T> for Vec<Decl> 
where T: Default
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        let mut res = vec!();
        for node in self {
            match node.visit(visitor)? {
                Control::Produce(t) => res.push(t),
                Control::SkipSiblings(t) => {
                    res.push(t);
                    break;
                }
                Control::Mutation(repl, t) => {
                    *node = repl;
                    res.push(t);
                }
            };
        }
        Ok(Control::Produce(res))
    }
}

impl<V,T> Visitable<T> for Option<V> 
where
    V: Visitable<T>,
    T: Default
    {
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, Self> {
        match self {
            Some(v) => v.visit(visitor),
            None => Ok(Control::Produce(T::default())),
        }
    }
}
