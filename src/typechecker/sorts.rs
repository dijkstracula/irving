#![allow(dead_code)]

use std::collections::HashMap;

use crate::ast::expressions::{Expr, Symbol};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Process {
    pub args: Vec<(Symbol, IvySort)>,
    pub impl_fields: HashMap<Symbol, IvySort>,
    pub spec_fields: HashMap<Symbol, IvySort>,
    pub common_impl_fields: HashMap<Symbol, IvySort>,
    pub common_spec_fields: HashMap<Symbol, IvySort>,
}

// TODO: this module is non-monomorphized (e.g. module type parameters are
// still in the argument list).  We're good with this??
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub args: Vec<(Symbol, IvySort)>, // Each of these will be SortVars
    pub fields: HashMap<Symbol, IvySort>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Fargs {
    Unknown, /* Still to be unified. */
    List(Vec<IvySort>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IvySort {
    Uninterpreted,
    Unit,
    Top,
    Bool,
    Number,
    BitVec(u8),
    Vector(Box<IvySort>),
    Range(Box<Expr>, Box<Expr>),
    Enum(Vec<Symbol>),
    Function(Fargs, Box<IvySort>),
    Relation(Vec<IvySort>),
    Subclass(Symbol),
    Module(Module),
    Process(Process),

    // A SortVar contains the index of its referrent into the typing context.
    SortVar(usize),
}

impl Default for IvySort {
    fn default() -> Self {
        IvySort::Unit
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FArgs {
    SortVar(usize),
    ArgList(Vec<IvySort>),
}
