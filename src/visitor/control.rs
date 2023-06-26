use super::VisitorResult;

/// Action performed after visiting a node of type Node.

#[derive(Debug, Eq, PartialEq)]
pub enum ControlMut<T, Node> {
    /// Hand back a value to the caller
    Produce(T),

    /// Skip traversing the nodes' siblings
    SkipSiblings(T),

    Mutation(Node, T),
}

impl<T, Node> ControlMut<T, Node> {
    /// Runs the thunk if we received `Produce` from the Visitor.
    pub fn and_then<F>(self, mut next: F) -> VisitorResult<T, Node>
    where
        // TODO: Thread through the T value??
        F: FnMut(T) -> VisitorResult<T, Node>,
    {
        match self {
            ControlMut::Produce(t) => next(t),
            ctrl => Ok(ctrl),
        }
    }

    pub fn map<F, U, N2>(self, mut f: F) -> VisitorResult<U, N2>
    where
        // TODO: Thread through the T value??
        F: FnMut(T) -> anyhow::Result<U>,
    {
        match self {
            ControlMut::Produce(t) => Ok(ControlMut::Produce(f(t)?)),
            ControlMut::SkipSiblings(t) => Ok(ControlMut::SkipSiblings(f(t)?)),
            ControlMut::Mutation(_, _) => todo!(),
        }
    }

    /// XXX: "unwrap"?
    pub fn modifying(self, target: &mut Node) -> anyhow::Result<T> {
        Ok(match self {
            ControlMut::Produce(t) => t,
            ControlMut::SkipSiblings(t) => t,
            ControlMut::Mutation(repl, t) => {
                *target = repl;
                t
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
