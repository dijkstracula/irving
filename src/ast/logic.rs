use super::{
    expressions::{Expr, ParamList, Symbol},
    span::Span,
};

/*
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LogicVar {
    pub name: String,
    pub sort: Option<String>
}
*/

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Fmla {
    Forall { span: Span, fmla: Forall },
    Exists { span: Span, fmla: Exists },
    Pred(Expr),

    LogicSymbol { span: Span, sym: Symbol },
}

impl Fmla {
    pub fn span(&self) -> &Span {
        match self {
            Fmla::Forall { span, .. } => span,
            Fmla::Exists { span, .. } => span,
            Fmla::Pred(expr) => expr.span(),
            Fmla::LogicSymbol { span, .. } => span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Exists {
    pub vars: ParamList,
    pub fmla: Box<Fmla>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Forall {
    pub vars: ParamList,
    pub fmla: Box<Fmla>,
}
