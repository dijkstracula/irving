#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]

use crate::ast::actions::*;
use crate::ast::declarations::*;
use crate::ast::expressions::*;
use crate::ast::logic::*;
use crate::ast::statements::*;
use crate::ast::toplevels::*;

/// Action performed after visiting a node of type Node.

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ControlV2<Node> {
    /// Continue traversal
    Continue,

    /// Skip traversing the nodes' children
    SkipChildren,

    /// Swap the node out for another.
    Change(Node),
}

impl<Node> ControlV2<Node> {
    /// Runs the thunk if we received `Continue` from the Visitor.
    pub fn and_then<E, F>(self, mut next: F) -> VisitorV2Result<Node, E>
    where
        F: FnMut() -> VisitorV2Result<Node, E>,
    {
        match self {
            ControlV2::Continue => next(),
            ctrl => Ok(ctrl),
        }
    }

    pub fn map<E, F, N2>(self, mut f: F) -> VisitorV2Result<N2, E>
    where
        F: FnMut(ControlV2<Node>) -> ControlV2<N2>,
    {
        Ok(f(self))
    }

    pub fn ok<E, N2>(self) -> VisitorV2Result<N2, E> {
        self.map(|c| match c {
            ControlV2::Continue => ControlV2::Continue,
            ControlV2::SkipChildren => ControlV2::SkipChildren,
            // TODO: this last case is not well-defined because Node != N2.
            // When we improve error handling, this should return an Err
            // (which will require an interface change; so it goes)
            ControlV2::Change(_) => todo!(),
        })
    }

    /// Mutates the node if we received `Change` from the Visitor.
    pub fn change<E>(self, target: &mut Node) -> VisitorV2Result<Node, E> {
        Ok(match self {
            ControlV2::Change(modified) => {
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

pub type VisitorV2Result<N, E> = std::result::Result<ControlV2<N>, E>;

/* TODO: deprecate Visitor and rename this. */
pub trait VisitorV2<E> {
    // Top levels
    fn start_prog(&mut self, _ast: &mut Prog) -> VisitorV2Result<Prog, E> {
        Ok(ControlV2::Continue)
    }
    fn finish_prog(&mut self, _ast: &mut Prog) -> VisitorV2Result<Prog, E> {
        Ok(ControlV2::Continue)
    }

    // Statements
    fn begin_if(&mut self, _ast: &mut If) -> VisitorV2Result<Stmt, E> {
        Ok(ControlV2::Continue)
    }
    fn finish_if(&mut self, _ast: &mut If) -> VisitorV2Result<Stmt, E> {
        Ok(ControlV2::Continue)
    }

    fn begin_while(&mut self, _ast: &mut While) -> VisitorV2Result<Stmt, E> {
        Ok(ControlV2::Continue)
    }
    fn finish_while(&mut self, _ast: &mut While) -> VisitorV2Result<Stmt, E> {
        Ok(ControlV2::Continue)
    }

    // Actions
    fn begin_assert(&mut self, _ast: &mut AssertAction) -> VisitorV2Result<Action, E> {
        Ok(ControlV2::Continue)
    }
    fn finish_assert(&mut self, _ast: &mut AssertAction) -> VisitorV2Result<Action, E> {
        Ok(ControlV2::Continue)
    }

    fn begin_assign(&mut self, _ast: &mut AssignAction) -> VisitorV2Result<Action, E> {
        Ok(ControlV2::Continue)
    }
    fn finish_assign(&mut self, _ast: &mut AssignAction) -> VisitorV2Result<Action, E> {
        Ok(ControlV2::Continue)
    }

    fn begin_assume(&mut self, _ast: &mut AssumeAction) -> VisitorV2Result<Action, E> {
        Ok(ControlV2::Continue)
    }
    fn finish_assume(&mut self, _ast: &mut AssumeAction) -> VisitorV2Result<Action, E> {
        Ok(ControlV2::Continue)
    }

    fn begin_call(&mut self, _ast: &mut AppExpr) -> VisitorV2Result<Action, E> {
        Ok(ControlV2::Continue)
    }
    fn finish_call(&mut self, _ast: &mut AppExpr) -> VisitorV2Result<Action, E> {
        Ok(ControlV2::Continue)
    }

    fn begin_ensure(&mut self, _ast: &mut EnsureAction) -> VisitorV2Result<Action, E> {
        Ok(ControlV2::Continue)
    }
    fn finish_ensure(&mut self, _ast: &mut EnsureAction) -> VisitorV2Result<Action, E> {
        Ok(ControlV2::Continue)
    }

    fn begin_requires(&mut self, _ast: &mut RequiresAction) -> VisitorV2Result<Action, E> {
        Ok(ControlV2::Continue)
    }
    fn finish_requires(&mut self, _ast: &mut RequiresAction) -> VisitorV2Result<Action, E> {
        Ok(ControlV2::Continue)
    }

    // Declarations

    fn begin_action_decl(&mut self, _ast: &mut ActionDecl) -> VisitorV2Result<Decl, E> {
        Ok(ControlV2::Continue)
    }
    fn finish_action_decl(&mut self, _ast: &mut ActionDecl) -> VisitorV2Result<Decl, E> {
        Ok(ControlV2::Continue)
    }

    fn begin_after_decl(&mut self, _ast: &mut AfterDecl) -> VisitorV2Result<Decl, E> {
        Ok(ControlV2::Continue)
    }
    fn finish_after_decl(&mut self, _ast: &mut AfterDecl) -> VisitorV2Result<Decl, E> {
        Ok(ControlV2::Continue)
    }

    // TODO: alias

    fn begin_attribute(&mut self, _ast: &mut Expr) -> VisitorV2Result<Decl, E> {
        Ok(ControlV2::Continue)
    }
    fn finish_attribute(&mut self, _ast: &mut Expr) -> VisitorV2Result<Decl, E> {
        Ok(ControlV2::Continue)
    }

    // TODO: the rest of the declarations

    // Expressions

    fn begin_app(&mut self, _ast: &mut AppExpr) -> VisitorV2Result<Expr, E> {
        Ok(ControlV2::Continue)
    }
    fn finish_app(&mut self, _ast: &mut AppExpr) -> VisitorV2Result<Expr, E> {
        Ok(ControlV2::Continue)
    }

    fn begin_binop(&mut self, _ast: &mut BinOp) -> VisitorV2Result<Expr, E> {
        Ok(ControlV2::Continue)
    }
    fn finish_binop(&mut self, _ast: &mut BinOp) -> VisitorV2Result<Expr, E> {
        Ok(ControlV2::Continue)
    }

    // Terminals

    fn boolean(&mut self, b: &mut bool) -> VisitorV2Result<Expr, E> {
        Ok(ControlV2::Continue)
    }

    fn number(&mut self, n: &mut i64) -> VisitorV2Result<Expr, E> {
        Ok(ControlV2::Continue)
    }

    fn symbol(&mut self, s: &mut Symbol) -> VisitorV2Result<Expr, E> {
        Ok(ControlV2::Continue)
    }

    fn verb(&mut self, v: &mut Verb) -> VisitorV2Result<Verb, E> {
        Ok(ControlV2::Continue)
    }
}

/// Something that can be visited by a Visitor.
pub trait Visitable<E>
where
    Self: Sized,
{
    fn visit(&mut self, visitor: &mut dyn VisitorV2<E>) -> VisitorV2Result<Self, E>;
}

impl<E> Visitable<E> for Action {
    fn visit(&mut self, visitor: &mut dyn VisitorV2<E>) -> VisitorV2Result<Self, E> {
        todo!()
    }
}

impl<E> Visitable<E> for Expr {
    fn visit(&mut self, visitor: &mut dyn VisitorV2<E>) -> VisitorV2Result<Self, E> {
        match self {
            Expr::App(expr) => visitor
                .begin_app(expr)?
                .and_then(|| expr.func.visit(visitor)?.ok())?
                .and_then(|| expr.args.visit(visitor)?.ok())?
                .children_end()
                .and_then(|| visitor.finish_app(expr)),
            Expr::BinOp(expr) => visitor
                .begin_binop(expr)?
                .and_then(|| expr.lhs.visit(visitor))?
                .and_then(|| visitor.verb(&mut expr.op)?.ok())?
                .and_then(|| expr.rhs.visit(visitor)?.ok())?
                .and_then(|| visitor.finish_binop(expr)),
            Expr::Boolean(b) => visitor.boolean(b),
            Expr::FieldAccess { record, field } => todo!(),
            Expr::Index(_) => todo!(),
            Expr::Number(n) => visitor.number(n),
            Expr::Symbol(i) => visitor.symbol(i),
            Expr::UnaryOp { op, expr } => todo!(),
            Expr::Term(_) => todo!(),
            Expr::This => todo!(),
        }?
        .change(self)
    }
}

impl<E> Visitable<E> for Stmt {
    fn visit(&mut self, visitor: &mut dyn VisitorV2<E>) -> VisitorV2Result<Self, E> {
        match self {
            Stmt::ActionSequence(seq) => seq.visit(visitor)?.ok(),
            Stmt::If(stmt) => visitor
                .begin_if(stmt)?
                .and_then(|| stmt.tst.visit(visitor)?.ok())?
                .and_then(|| stmt.thn.visit(visitor)?.ok())?
                .and_then(|| stmt.els.visit(visitor)?.ok())?
                .children_end()
                .and_then(|| visitor.finish_if(stmt)),
            Stmt::While(stmt) => visitor
                .begin_while(stmt)?
                .and_then(|| stmt.test.visit(visitor)?.ok())?
                .and_then(|| stmt.doit.visit(visitor)?.ok())?
                .children_end()
                .and_then(|| visitor.finish_while(stmt)),
        }?
        .change(self)
    }
}

impl<E, V: Visitable<E>> Visitable<E> for Vec<V> {
    fn visit(&mut self, visitor: &mut dyn VisitorV2<E>) -> VisitorV2Result<Self, E> {
        for node in self {
            match node.visit(visitor)? {
                ControlV2::Continue => continue,
                ControlV2::SkipChildren => continue,
                ControlV2::Change(replacement) => *node = replacement,
            };
        }
        Ok(ControlV2::Continue)
    }
}

impl<E, V: Visitable<E>> Visitable<E> for Option<V> {
    fn visit(&mut self, visitor: &mut dyn VisitorV2<E>) -> VisitorV2Result<Self, E> {
        match self {
            Some(v) => v.visit(visitor)?.ok(),
            None => Ok(ControlV2::Continue),
        }
    }
}
