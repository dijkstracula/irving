use crate::visitor::VisitorResult;

pub mod ivy;
pub mod java;
pub mod pprint;
pub mod vmt;

type ExtractResult<Node> = VisitorResult<(), std::fmt::Error, Node>;
