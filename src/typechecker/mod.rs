#![allow(dead_code)]

use crate::ast::expressions::Symbol;

use self::sorts::IvySort;

pub(crate) mod inference;
pub(crate) mod sorts;
pub(crate) mod unifier;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    InvalidApplication(IvySort),
    SortMismatch { expected: IvySort, actual: IvySort },
    NotARecord(IvySort),
    UnboundVariable(Symbol),
    UnificationError(IvySort, IvySort),
}
