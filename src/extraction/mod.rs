use crate::visitor::VisitorResult;

pub mod ivy;
pub mod java;
pub mod pprint;

type ExtractResult<Node> = VisitorResult<(), std::fmt::Error, Node>;
