use super::{
    expressions::{self, Expr, ParamList, Symbol},
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
    Forall {
        span: Span,
        fmla: Forall,
    },
    Exists {
        span: Span,
        fmla: Exists,
    },
    Pred(Expr),

    App {
        span: Span,
        fmla: LogicApp,
    },

    BinOp {
        span: Span,
        op: LogicBinOp,
    },

    LogicSymbol {
        span: Span,
        sym: Symbol,
    },

    UnaryOp {
        span: Span,
        op: expressions::Verb,
        fmla: Box<Fmla>,
    },
}

impl Fmla {
    pub fn span(&self) -> &Span {
        match self {
            Fmla::Forall { span, .. } => span,
            Fmla::Exists { span, .. } => span,
            Fmla::Pred(expr) => expr.span(),
            Fmla::App { span, .. } => span,
            Fmla::BinOp { span, .. } => span,
            Fmla::LogicSymbol { span, .. } => span,
            Fmla::UnaryOp { span, .. } => span,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LogicBinOp {
    pub lhs: Box<Fmla>,
    pub op: expressions::Verb,
    pub rhs: Box<Fmla>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LogicApp {
    pub func: Box<Expr>,
    pub args: Vec<Fmla>,
}
