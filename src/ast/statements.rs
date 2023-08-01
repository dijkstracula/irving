#![allow(dead_code)]

use super::actions::*;
use super::declarations::Binding;
use super::expressions::*;

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub struct If {
    pub tst: ExprKind,
    pub thn: Vec<Stmt>,
    pub els: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub struct While {
    pub test: ExprKind,
    pub doit: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub enum Stmt {
    ActionSequence(Vec<Action>),
    If(If),
    While(While),
    VarDecl(Binding<Sort>),
}
