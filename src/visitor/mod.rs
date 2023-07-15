pub mod ast;
pub mod control;
pub mod sort;

pub(crate) use control::ControlMut;

pub type VisitorResult<T, Node> = anyhow::Result<ControlMut<T, Node>>;
