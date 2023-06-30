pub mod control;
pub mod visitor;
pub(crate) use crate::visitor::visitor::Visitor;
pub(crate) use control::ControlMut;

pub type VisitorResult<T, Node> = anyhow::Result<ControlMut<T, Node>>;
