use super::{
    expressions::{Expr, ParamList},
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
    Forall(Forall),
    Exists(Exists),
    Pred(Expr),
}

impl Fmla {
    pub fn span(&self) -> &Span {
        match self {
            Fmla::Forall(forall) => forall.fmla.span(),
            Fmla::Exists(exists) => exists.fmla.span(),
            Fmla::Pred(expr) => expr.span(),
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
