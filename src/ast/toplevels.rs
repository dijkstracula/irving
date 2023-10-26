use super::declarations::{IncludeDecl, ObjectDecl};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Prog {
    pub major_version: u8,
    pub minor_version: u8,

    pub includes: Vec<IncludeDecl>,
    pub top: ObjectDecl
}
