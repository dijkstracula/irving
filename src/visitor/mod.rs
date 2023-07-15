pub mod ast;
pub mod control;

pub(crate) use control::ControlMut;

pub type VisitorResult<T, Node> = anyhow::Result<ControlMut<T, Node>>;
