use super::expressions::{Expr, Param};

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Exists {
    pub vars: Vec<Param>,
    pub fmla: Box<Fmla>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Forall {
    pub vars: Vec<Param>,
    pub fmla: Box<Fmla>,
}
