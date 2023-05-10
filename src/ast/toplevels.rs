use super::declarations::IsolateDecl;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Prog {
    pub major_version: u8,
    pub minor_version: u8,

    pub top: IsolateDecl,
}