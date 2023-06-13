#![allow(dead_code)]

use crate::ast::expressions::{Expr, Symbol};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Process {
    args: Vec<(String, IvySort)>,
    impl_fields: Vec<(String, IvySort)>,
    spec_fields: Vec<(String, IvySort)>,
    commonspec_fields: Vec<(String, IvySort)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IvySort {
    Uninterpreted,
    Unit,
    Top,
    Bool,
    Number,
    Range(Box<Expr>, Box<Expr>),
    Enum(Vec<Symbol>),
    Function(Vec<IvySort>, Box<IvySort>),
    Relation(Vec<IvySort>),
    Subclass(Symbol),
    Process(Process),

    // A SortVar contains the index of its referrent into the typing context.
    SortVar(usize),
}

impl Default for IvySort {
    fn default() -> Self {
        IvySort::Unit
    }
}
