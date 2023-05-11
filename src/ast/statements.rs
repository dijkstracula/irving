#![allow(dead_code)]

use super::expressions::*;
use super::actions::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub struct If {
    pub tst: Expr,
    pub thn: Vec<Stmt>,
    pub els: Option<Vec<Stmt>>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub struct While {
    pub test: Expr,
    pub doit: Vec<Stmt>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub enum Stmt {
    ActionSequence(Vec<Action>),
    If(If),
    While(While),
    Expr(Expr),
}
