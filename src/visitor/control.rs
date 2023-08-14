use std::fmt::Debug;

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

impl<T, Node> ControlMut<T, Node>
where
    Node: Debug,
{
    /// Runs the thunk if we received `Produce` from the Visitor.
    pub fn and_then<E, F>(self, mut next: F) -> VisitorResult<T, E, Node>
    where
        // TODO: Thread through the T value??
        F: FnMut(T) -> VisitorResult<T, E, Node>,
    {
        match self {
            ControlMut::Produce(t) => next(t),
            ctrl => Ok(ctrl),
        }
    }

    pub fn map<E, F, U, N2>(self, mut f: F) -> VisitorResult<U, E, N2>
    where
        // TODO: Thread through the T value??
        F: FnMut(T) -> Result<U, E>,
    {
        match self {
            ControlMut::Produce(t) => Ok(ControlMut::Produce(f(t)?)),
            ControlMut::SkipSiblings(t) => Ok(ControlMut::SkipSiblings(f(t)?)),
            ControlMut::Mutation(_, _) => todo!(),
        }
    }

    /// XXX: "unwrap"?
    pub fn modifying(self, target: &mut Node) -> T {
        match self {
            ControlMut::Produce(t) => t,
            ControlMut::SkipSiblings(t) => t,
            ControlMut::Mutation(repl, t) => {
                log::trace!(target: "visitor", "Mutation: {:?} -> {:?}", target, repl);
                *target = repl;
                t
            }
        }
    }
}
