use super::expressions::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub struct AssignAction {
    pub lhs: Expr,
    pub rhs: Expr
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub struct AssertAction{
    pub pred: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssumeAction{
    pub pred: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnsureAction{
    pub pred: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RequiresAction{
    pub pred: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Action {
    Assert(AssertAction),
    Assign(AssignAction),
    Assume(AssumeAction),
    Call(AppExpr),
    Ensure(EnsureAction),
    Requires(RequiresAction),
}
