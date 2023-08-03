pub mod ast;
pub mod control;
pub mod sort;

pub(crate) use control::ControlMut;

pub type VisitorResult<T, E, Node> = std::result::Result<ControlMut<T, Node>, E>;
