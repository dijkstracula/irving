use super::{expressions::*, logic::Fmla, span::Span};

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub struct AssignAction {
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub struct AssertAction {
    pub pred: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssumeAction {
    pub pred: Expr,
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
    Assert { span: Span, action: AssertAction },

    Assign { span: Span, action: AssignAction },

    Assume { span: Span, action: AssumeAction },

    Call { span: Span, action: AppExpr },

    Ensure { span: Span, action: EnsureAction },

    Requires { span: Span, action: RequiresAction },
}
