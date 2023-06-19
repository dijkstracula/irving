#![allow(dead_code)]

use std::collections::HashMap;

use crate::ast::expressions::{Expr, Symbol};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub args: Vec<(Symbol, IvySort)>,
    pub impl_fields: HashMap<Symbol, IvySort>,
    pub spec_fields: HashMap<Symbol, IvySort>,
    pub commonspec_fields: HashMap<Symbol, IvySort>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    Module(Module),

    // A SortVar contains the index of its referrent into the typing context.
    SortVar(usize),
}

impl Default for IvySort {
    fn default() -> Self {
        IvySort::Unit
    }
}
