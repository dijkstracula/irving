use super::{expressions::*, logic::Fmla, span::Span};

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub struct AssignAction {
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub struct AssignLogicalAction {
    pub lhs: Fmla,
    pub rhs: Fmla,
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

    AssignLogical { span: Span, action: AssignLogicalAction },

    Assume { span: Span, action: AssumeAction },

    Call { span: Span, action: AppExpr },

    Ensure { span: Span, action: EnsureAction },


    Requires { span: Span, action: RequiresAction },
}

impl Action {
    pub fn span(&self) -> &Span {
        match self {
            Action::Assert { span, .. } => span,
            Action::Assign { span, .. } => span,
            Action::Assume { span, .. } => span,
            Action::Call { span, .. } => span,
            Action::Ensure { span, .. } => span,
            Action::AssignLogical { span, .. } => span,
            Action::Requires { span, .. } => span,
        }
    }
}
