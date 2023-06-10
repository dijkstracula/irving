#![allow(dead_code)]

use crate::ast::expressions::{Expr, Symbol};

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

    // A SortVar contains the index of its referrent into the typing context.
    SortVar(usize),
}

impl Default for IvySort {
    fn default() -> Self {
        IvySort::Unit
    }
}
