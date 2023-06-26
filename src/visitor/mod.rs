use self::control::ControlMut;

pub mod constant_fold;
pub mod control;
pub(crate) mod global_lowerer;
pub(crate) mod pprint;
pub(crate) mod visitor;

pub type VisitorResult<T, Node> = anyhow::Result<ControlMut<T, Node>>;
