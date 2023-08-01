use super::{expressions::*, logic::Fmla};

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub struct AssignAction {
    pub lhs: ExprKind,
    pub rhs: ExprKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub struct AssertAction {
    pub pred: ExprKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssumeAction {
    pub pred: ExprKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnsureAction {
    pub pred: Fmla,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RequiresAction {
    pub pred: Fmla,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    Assert(AssertAction),
    Assign(AssignAction),
    Assume(AssumeAction),
    Call(AppExpr),
    Ensure(EnsureAction),
    Requires(RequiresAction),
}
