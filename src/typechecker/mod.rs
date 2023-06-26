#![allow(dead_code)]

use thiserror::Error;

use crate::ast::expressions::Symbol;

use self::sorts::IvySort;

pub(crate) mod inference;
pub(crate) mod sorts;
pub(crate) mod unifier;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum TypeError {
    #[error("Sort {0:?} can't be called")]
    InvalidApplication(IvySort),

    #[error("Expected {expected:?}, got {actual:?}")]
    SortMismatch { expected: IvySort, actual: IvySort },

    #[error("{0:?} is not a field-haver")]
    NotARecord(IvySort),

    #[error("Unbound variable {0}")]
    UnboundVariable(Symbol),

    #[error("Sort {0:?} mismatches {1:?}")]
    UnificationError(IvySort, IvySort),
}
