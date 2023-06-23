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

pub enum ControlV2 {
    /// Continue traversal
    Continue,

    /// Skip traversing the nodes' children
    SkipChildren,

    /// Swaps out one expr for another.
    ChangeExpr(Expr),

    /// Swaps out one stmt for another.
    ChangeStmt(Stmt),

    /// Swaps out one decl for another.
    ChangeDecl(Decl),

    /// Swaps out one action for another.
    ChangeAction(Action),

    /// Swaps out one param for another.
    ChangeParam(Param),
}

impl ControlV2 {
    /// Runs the thunk if we received `Continue` from the Visitor.
    pub fn and_then<F>(self, mut next: F) -> VisitorV2Result
    where
        F: FnMut() -> VisitorV2Result,
    {
        match self {
            ControlV2::Continue => next(),
            ctrl => Ok(ctrl),
        }
    }

    pub fn map<F>(self, mut f: F) -> VisitorV2Result
    where
        F: FnMut(ControlV2) -> ControlV2,
    {
        Ok(f(self))
    }

    /// Mutates the node if we received `Change` from the Visitor.
    pub fn change_expr(self, target: &mut Expr) -> VisitorV2Result {
        Ok(match self {
            ControlV2::ChangeExpr(modified) => {
                *target = modified;
                Self::Continue
            }
            ctrl => ctrl,
        })
    }

    /// Mutates the node if we received `Change` from the Visitor.
    pub fn change_stmt(self, target: &mut Stmt) -> VisitorV2Result {
        Ok(match self {
            ControlV2::ChangeStmt(modified) => {
                *target = modified;
                Self::Continue
            }
            ctrl => ctrl,
        })
    }

    /// Mutates the node if we received `Change` from the Visitor.
    pub fn change_decl(self, target: &mut Decl) -> VisitorV2Result {
        Ok(match self {
            ControlV2::ChangeDecl(modified) => {
                *target = modified;
                Self::Continue
            }
            ctrl => ctrl,
        })
    }
    /// Delineates when we've finished traversing the children of a node.
    pub fn children_end(self) -> Self {
        match self {
            ControlV2::SkipChildren => ControlV2::Continue,
            ctrl => ctrl,
        }
    }
}

pub type VisitorV2Result = anyhow::Result<ControlV2>;

/* TODO: deprecate Visitor and rename this. */
pub trait VisitorV2 {
    // Top levels
    fn begin_prog(&mut self, _ast: &mut Prog) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_prog(&mut self, _ast: &mut Prog) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    // Statements

    fn action_seq(&mut self, ast: &mut Vec<Action>) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_if(&mut self, _ast: &mut If) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_if(&mut self, _ast: &mut If) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_while(&mut self, _ast: &mut While) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_while(&mut self, _ast: &mut While) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    // Actions
    fn begin_assert(&mut self, _ast: &mut AssertAction) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_assert(&mut self, _ast: &mut AssertAction) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_assign(&mut self, _ast: &mut AssignAction) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_assign(&mut self, _ast: &mut AssignAction) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_assume(&mut self, _ast: &mut AssumeAction) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_assume(&mut self, _ast: &mut AssumeAction) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_call(&mut self, _ast: &mut AppExpr) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_call(&mut self, _ast: &mut AppExpr) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_ensure(&mut self, _ast: &mut EnsureAction) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_ensure(&mut self, _ast: &mut EnsureAction) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_requires(&mut self, _ast: &mut RequiresAction) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_requires(&mut self, _ast: &mut RequiresAction) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    // Declarations

    fn begin_action_decl(&mut self, _ast: &mut ActionDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_action_decl(&mut self, _ast: &mut ActionDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_after_decl(&mut self, _ast: &mut AfterDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_after_decl(&mut self, _ast: &mut AfterDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_axiom_decl(&mut self, _ast: &mut Fmla) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_axiom_decl(&mut self, _ast: &mut Fmla) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_attribute_decl(&mut self, _ast: &mut Expr) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_attribute_decl(&mut self, _ast: &mut Expr) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_before_decl(&mut self, _ast: &mut BeforeDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_before_decl(&mut self, _ast: &mut BeforeDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_common_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_common_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_export_decl(&mut self, _ast: &mut ExportDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_export_decl(&mut self, _ast: &mut ExportDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_function_decl(&mut self, _ast: &mut FunctionDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_function_decl(&mut self, _ast: &mut FunctionDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_global_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_global_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_implement_decl(&mut self, _ast: &mut ImplementDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_implement_decl(&mut self, _ast: &mut ImplementDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_implementation_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_implementation_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_import_decl(&mut self, _ast: &mut ImportDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_import_decl(&mut self, _ast: &mut ImportDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_include_decl(&mut self, _ast: &mut Symbol) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_include_decl(&mut self, _ast: &mut Symbol) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_instance_decl(&mut self, _ast: &mut InstanceDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_instance_decl(&mut self, _ast: &mut InstanceDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_invariant_decl(&mut self, _ast: &mut Fmla) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_invariant_decl(&mut self, _ast: &mut Fmla) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_isolate_decl(&mut self, _ast: &mut IsolateDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_isolate_decl(&mut self, _ast: &mut IsolateDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_module_decl(&mut self, _ast: &mut ModuleDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_module_decl(&mut self, _ast: &mut ModuleDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_object_decl(&mut self, _ast: &mut ObjectDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_object_decl(&mut self, _ast: &mut ObjectDecl) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_relation(&mut self, _ast: &mut Relation) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_relation(&mut self, _ast: &mut Relation) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_specification(&mut self, _ast: &mut Vec<Decl>) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_specification(&mut self, _ast: &mut Vec<Decl>) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_typedecl(&mut self, _ast: &mut Type) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_typedecl(&mut self, _ast: &mut Type) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_vardecl(&mut self, _ast: &mut Term) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_vardecl(&mut self, _ast: &mut Term) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    // Quantified formulas

    fn begin_exists(&mut self, _ast: &mut Exists) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_exists(&mut self, _ast: &mut Exists) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_forall(&mut self, _ast: &mut Forall) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_forall(&mut self, _ast: &mut Forall) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    // Expressions

    fn begin_app(&mut self, _ast: &mut AppExpr) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_app(&mut self, _ast: &mut AppExpr) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_binop(&mut self, _ast: &mut BinOp) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_binop(&mut self, _ast: &mut BinOp) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_field_access(&mut self, _lhs: &mut Expr, rhs: &mut Symbol) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_field_access(&mut self, _lhs: &mut Expr, rhs: &mut Symbol) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_index(&mut self, _ast: &mut IndexExpr) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_index(&mut self, _lhs: &mut IndexExpr) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_term(&mut self, _expr: &mut Term) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_term(&mut self, _expr: &mut Term) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn begin_unary_op(&mut self, _op: &mut Verb, _rhs: &mut Expr) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
    fn finish_unary_op(&mut self, _op: &mut Verb, _rhs: &mut Expr) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    // Terminals

    fn boolean(&mut self, b: &mut bool) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn identifier(&mut self, _i: &mut Ident) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn number(&mut self, _n: &mut i64) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn param(&mut self, _p: &mut Param) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn sort(&mut self, _s: &mut IvySort) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn symbol(&mut self, _s: &mut Symbol) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }

    fn verb(&mut self, v: &mut Verb) -> VisitorV2Result {
        Ok(ControlV2::Continue)
    }
}

/// Something that can be visited by a Visitor.
pub trait Visitable {
    fn visit(&mut self, visitor: &mut dyn VisitorV2) -> VisitorV2Result;
}

impl Visitable for Prog {
    fn visit(&mut self, visitor: &mut dyn VisitorV2) -> VisitorV2Result {
        visitor
            .begin_prog(self)?
            /* Note: don't visit the name or args for this special decl */
            .and_then(|| self.top.body.visit(visitor))?
            .and_then(|| visitor.finish_prog(self))
    }
}

impl Visitable for Action {
    fn visit(&mut self, visitor: &mut dyn VisitorV2) -> VisitorV2Result {
        match self {
            Action::Assert(action) => visitor
                .begin_assert(action)?
                .and_then(|| action.pred.visit(visitor))?
                .and_then(|| visitor.finish_assert(action)),
            Action::Assign(action) => visitor
                .begin_assign(action)?
                .and_then(|| action.lhs.visit(visitor))?
                .and_then(|| action.rhs.visit(visitor))?
                .and_then(|| visitor.finish_assign(action)),
            Action::Assume(action) => visitor
                .begin_assume(action)?
                .and_then(|| action.pred.visit(visitor))?
                .and_then(|| visitor.finish_assume(action)),
            Action::Call(action) => visitor
                .begin_call(action)?
                .and_then(|| action.func.visit(visitor))?
                .and_then(|| action.args.visit(visitor))?
                .and_then(|| visitor.finish_call(action)),
            Action::Ensure(action) => visitor
                .begin_ensure(action)?
                .and_then(|| action.pred.visit(visitor))?
                .and_then(|| visitor.finish_ensure(action)),
            Action::Requires(action) => visitor
                .begin_requires(action)?
                .and_then(|| action.pred.visit(visitor))?
                .and_then(|| visitor.finish_requires(action)),
        }
    }
}

impl Visitable for Expr {
    fn visit(&mut self, visitor: &mut dyn VisitorV2) -> VisitorV2Result {
        match self {
            Expr::App(expr) => visitor
                .begin_app(expr)?
                .and_then(|| expr.func.visit(visitor))?
                .and_then(|| expr.args.visit(visitor))?
                .children_end()
                .and_then(|| visitor.finish_app(expr)),
            Expr::BinOp(expr) => visitor
                .begin_binop(expr)?
                .and_then(|| expr.lhs.visit(visitor))?
                .and_then(|| visitor.verb(&mut expr.op))?
                .and_then(|| expr.rhs.visit(visitor))?
                .and_then(|| visitor.finish_binop(expr)),
            Expr::Boolean(b) => visitor.boolean(b),
            Expr::FieldAccess { record, field } => visitor
                .begin_field_access(record, field)?
                .and_then(|| record.visit(visitor))?
                .and_then(|| field.visit(visitor))?
                .and_then(|| visitor.finish_field_access(record, field)),
            Expr::Index(expr) => visitor
                .begin_index(expr)?
                .and_then(|| expr.lhs.visit(visitor))?
                .and_then(|| expr.idx.visit(visitor))?
                .and_then(|| visitor.finish_index(expr)),
            Expr::Number(n) => visitor.number(n),
            Expr::Symbol(i) => visitor.symbol(i),
            Expr::UnaryOp { op, expr } => visitor
                .begin_unary_op(op, expr)?
                .and_then(|| visitor.verb(op))?
                .and_then(|| expr.visit(visitor))?
                .and_then(|| visitor.finish_unary_op(op, expr)),
            Expr::Term(t) => visitor
                .begin_term(t)?
                .and_then(|| t.id.visit(visitor))?
                .and_then(|| t.sort.visit(visitor))?
                .and_then(|| visitor.finish_term(t)),
            Expr::This => todo!(),
        }?
        .change_expr(self)
    }
}

impl Visitable for Fmla {
    fn visit(&mut self, visitor: &mut dyn VisitorV2) -> VisitorV2Result {
        match self {
            Fmla::Forall(fmla) => visitor
                .begin_forall(fmla)?
                .and_then(|| fmla.vars.visit(visitor))?
                .and_then(|| fmla.fmla.visit(visitor))?
                .and_then(|| visitor.finish_forall(fmla)),
            Fmla::Exists(fmla) => visitor
                .begin_exists(fmla)?
                .and_then(|| fmla.vars.visit(visitor))?
                .and_then(|| fmla.fmla.visit(visitor))?
                .and_then(|| visitor.finish_exists(fmla)),
            Fmla::Pred(expr) => expr.visit(visitor),
        }
    }
}

impl Visitable for Stmt {
    fn visit(&mut self, visitor: &mut dyn VisitorV2) -> VisitorV2Result {
        match self {
            Stmt::ActionSequence(seq) => visitor.action_seq(seq),
            Stmt::If(stmt) => visitor
                .begin_if(stmt)?
                .and_then(|| stmt.tst.visit(visitor))?
                .and_then(|| stmt.thn.visit(visitor))?
                .and_then(|| stmt.els.visit(visitor))?
                .children_end()
                .and_then(|| visitor.finish_if(stmt)),
            Stmt::While(stmt) => visitor
                .begin_while(stmt)?
                .and_then(|| stmt.test.visit(visitor))?
                .and_then(|| stmt.doit.visit(visitor))?
                .children_end()
                .and_then(|| visitor.finish_while(stmt)),
        }?
        .change_stmt(self)
    }
}

impl Visitable for Decl {
    fn visit(&mut self, visitor: &mut dyn VisitorV2) -> VisitorV2Result {
        match self {
            Decl::Action(decl) => visitor
                .begin_action_decl(decl)?
                .and_then(|| visitor.symbol(&mut decl.name))?
                .and_then(|| decl.params.visit(visitor))?
                .and_then(|| visitor.finish_action_decl(decl)),
            Decl::AfterAction(decl) => visitor
                .begin_after_decl(decl)?
                .and_then(|| decl.name.visit(visitor))?
                .and_then(|| decl.params.visit(visitor))?
                .and_then(|| decl.body.visit(visitor))?
                .and_then(|| visitor.finish_after_decl(decl)),
            Decl::Alias(_, _) => todo!(),
            Decl::Attribute(decl) => visitor
                .begin_attribute_decl(decl)?
                .and_then(|| decl.visit(visitor))?
                .and_then(|| visitor.finish_attribute_decl(decl)),
            Decl::Axiom(fmla) => visitor
                .begin_axiom_decl(fmla)?
                .and_then(|| fmla.visit(visitor))?
                .and_then(|| visitor.finish_axiom_decl(fmla)),
            Decl::BeforeAction(decl) => visitor
                .begin_before_decl(decl)?
                .and_then(|| decl.name.visit(visitor))?
                .and_then(|| decl.params.visit(visitor))?
                .and_then(|| decl.body.visit(visitor))?
                .and_then(|| visitor.finish_before_decl(decl)),
            Decl::Common(decl) => visitor
                .begin_common_decl(decl)?
                .and_then(|| decl.visit(visitor))?
                .and_then(|| visitor.finish_common_decl(decl)),
            Decl::Export(decl) => visitor
                .begin_export_decl(decl)?
                .and_then(|| match decl {
                    ExportDecl::Action(decl) => visitor
                        .begin_action_decl(decl)?
                        .and_then(|| visitor.symbol(&mut decl.name))?
                        .and_then(|| decl.params.visit(visitor))?
                        .and_then(|| visitor.finish_action_decl(decl)),
                    ExportDecl::ForwardRef(sym) => sym.visit(visitor),
                })?
                .and_then(|| visitor.finish_export_decl(decl)),
            Decl::Function(_) => todo!(),
            Decl::Globals(decl) => visitor
                .begin_global_decl(decl)?
                .and_then(|| decl.visit(visitor))?
                .and_then(|| visitor.finish_global_decl(decl)),
            Decl::Implement(_) => todo!(),
            Decl::Implementation(decl) => visitor
                .begin_implementation_decl(decl)?
                .and_then(|| decl.visit(visitor))?
                .and_then(|| visitor.finish_implementation_decl(decl)),
            Decl::Import(decl) => visitor
                .begin_import_decl(decl)?
                .and_then(|| decl.name.visit(visitor))?
                .and_then(|| decl.params.visit(visitor))?
                .and_then(|| visitor.finish_import_decl(decl)),
            Decl::Isolate(decl) => visitor
                .begin_isolate_decl(decl)?
                .and_then(|| decl.name.visit(visitor))?
                .and_then(|| decl.params.visit(visitor))?
                .and_then(|| decl.body.visit(visitor))?
                .and_then(|| visitor.finish_isolate_decl(decl)),
            Decl::Include(decl) => visitor
                .begin_include_decl(decl)?
                .and_then(|| visitor.finish_include_decl(decl)),
            Decl::Instance(decl) => visitor
                .begin_instance_decl(decl)?
                .and_then(|| decl.name.visit(visitor))?
                .and_then(|| decl.sort.visit(visitor))?
                .and_then(|| decl.args.visit(visitor))?
                .and_then(|| visitor.finish_instance_decl(decl)),
            Decl::Instantiate { name, prms } => todo!(),
            Decl::Interpretation { itype, ctype } => todo!(),
            Decl::Invariant(decl) => visitor
                .begin_invariant_decl(decl)?
                .and_then(|| decl.visit(visitor))?
                .and_then(|| visitor.finish_invariant_decl(decl)),
            Decl::Module(decl) => visitor
                .begin_module_decl(decl)?
                .and_then(|| decl.name.visit(visitor))?
                .and_then(|| decl.params.visit(visitor))?
                .and_then(|| decl.body.visit(visitor))?
                .and_then(|| visitor.finish_module_decl(decl)),
            Decl::Object(decl) => visitor
                .begin_object_decl(decl)?
                .and_then(|| decl.name.visit(visitor))?
                .and_then(|| decl.params.visit(visitor))?
                .and_then(|| decl.body.visit(visitor))?
                .and_then(|| visitor.finish_object_decl(decl)),
            Decl::Relation(decl) => visitor
                .begin_relation(decl)?
                .and_then(|| decl.name.visit(visitor))?
                .and_then(|| decl.params.visit(visitor))?
                .and_then(|| visitor.finish_relation(decl)),
            Decl::Specification(decl) => visitor
                .begin_specification(decl)?
                .and_then(|| decl.visit(visitor))?
                .and_then(|| visitor.finish_specification(decl)),
            Decl::Stmts(stmts) => stmts.visit(visitor),
            Decl::Var(decl) => visitor
                .begin_vardecl(decl)?
                .and_then(|| decl.id.visit(visitor))?
                .and_then(|| decl.sort.visit(visitor))?
                .and_then(|| visitor.finish_vardecl(decl)),
            Decl::Type(decl) => visitor
                .begin_typedecl(decl)?
                .and_then(|| match &mut decl.ident {
                    TypeName::Name(n) => n.visit(visitor),
                    TypeName::This => todo!(),
                })?
                .and_then(|| visitor.sort(&mut decl.sort))?
                .and_then(|| visitor.finish_typedecl(decl)),
        }?
        .change_decl(self)
    }
}

impl Visitable for Symbol {
    fn visit(&mut self, visitor: &mut dyn VisitorV2) -> VisitorV2Result {
        visitor.symbol(self)
    }
}

impl Visitable for Ident {
    fn visit(&mut self, visitor: &mut dyn VisitorV2) -> VisitorV2Result {
        visitor.identifier(self)
    }
}

// XXX: Param is neither an expr, nor statement, nor a decl.  It's weird
// that we have to special-case it like this :/
impl Visitable for Param {
    fn visit(&mut self, visitor: &mut dyn VisitorV2) -> VisitorV2Result {
        visitor.param(self)
    }
}

// Implementations for compound nodes

impl Visitable for Vec<Expr> {
    fn visit(&mut self, visitor: &mut dyn VisitorV2) -> VisitorV2Result {
        for node in self {
            match node.visit(visitor)? {
                ControlV2::Continue => continue,
                ControlV2::ChangeExpr(replacement) => *node = replacement,
                _ => continue,
            };
        }
        Ok(ControlV2::Continue)
    }
}

impl Visitable for Vec<Stmt> {
    fn visit(&mut self, visitor: &mut dyn VisitorV2) -> VisitorV2Result {
        for node in self {
            match node.visit(visitor)? {
                ControlV2::Continue => continue,
                ControlV2::ChangeStmt(replacement) => *node = replacement,
                _ => continue,
            };
        }
        Ok(ControlV2::Continue)
    }
}

impl Visitable for Vec<Decl> {
    fn visit(&mut self, visitor: &mut dyn VisitorV2) -> VisitorV2Result {
        for node in self {
            match node.visit(visitor)? {
                ControlV2::Continue => continue,
                ControlV2::ChangeDecl(replacement) => *node = replacement,
                _ => continue,
            };
        }
        Ok(ControlV2::Continue)
    }
}

impl Visitable for ParamList {
    fn visit(&mut self, visitor: &mut dyn VisitorV2) -> VisitorV2Result {
        for node in self {
            match node.visit(visitor)? {
                ControlV2::Continue => continue,
                ControlV2::ChangeParam(replacement) => *node = replacement,
                _ => continue,
            };
        }
        Ok(ControlV2::Continue)
    }
}

impl<V: Visitable> Visitable for Option<V> {
    fn visit(&mut self, visitor: &mut dyn VisitorV2) -> VisitorV2Result {
        match self {
            Some(v) => v.visit(visitor),
            None => Ok(ControlV2::Continue),
        }
    }
}
