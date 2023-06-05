#![allow(dead_code)]

use crate::ast::expressions::Symbol;

use self::sorts::IvySort;

pub (crate) mod inference;
pub (crate) mod sorts;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Error {
    ConstraintFailure,
    SortMismatch { expected: IvySort, actual: IvySort}, 
    UnboundVariable(Symbol),
}