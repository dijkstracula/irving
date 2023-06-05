#![allow(dead_code)]

use crate::ast::expressions::{Expr, Symbol};

use super::inference::SortVar;

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

    SortVar(SortVar),
}

impl Default for IvySort {
    fn default() -> Self {
        IvySort::Unit
    }
}
