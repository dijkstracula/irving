#![allow(dead_code)]

use std::collections::HashMap;

use crate::ast::expressions::{Expr, Symbol};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Process {
    pub args: HashMap<Symbol, IvySort>,
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

impl Module {
    pub fn init_action_sort() -> IvySort {
        IvySort::function_sort(vec![], IvySort::Unit)
    }
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

impl IvySort {
    pub fn function_sort(args: Vec<IvySort>, ret: IvySort) -> IvySort {
        IvySort::Function(Fargs::List(args), Box::new(ret))
    }

    pub fn range_sort(lo: Expr, hi: Expr) -> IvySort {
        IvySort::Range(Box::new(lo), Box::new(hi))
    }
}

impl Default for IvySort {
    fn default() -> Self {
        IvySort::Unit
    }
}
