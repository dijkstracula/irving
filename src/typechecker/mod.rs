#![allow(dead_code)]

use thiserror::Error;

use crate::ast::{
    declarations::{Decl, IsolateDecl},
    expressions::Symbol,
};

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

    #[error("Declaration {0:?} doesn't bind a name")]
    NonBindingDecl(Decl),

    #[error("{0:?} is not an action or function")]
    NotAFunction(IvySort),

    #[error("{0:?} cannot be instantiated (did you mean to use `var`?)")]
    NotInstanceable(IvySort),

    #[error("{0:?} cannot be indexed into with `.`")]
    NotARecord(IvySort),

    #[error("{0:?} has no field {1} ")]
    MissingRecordField(IvySort, Symbol),

    #[error("Unbound variable {0}")]
    UnboundVariable(Symbol),

    #[error("Sort {0:?} mismatches {1:?}")]
    UnificationError(IvySort, IvySort),

    #[error("Got isolate declaration {0:?}; did the normalization pass run?")]
    UnnormalizedIsolate(IsolateDecl),

    #[error("Sort sequence {0:?} mismatches {1:?}")]
    LenMismatch(Vec<IvySort>, Vec<IvySort>),

    #[error("Symbol {expected:?} redefined as {actual:?}")]
    FargMismatch { expected: Symbol, actual: Symbol },

    #[error("Symbol {sym:?} defined as {prev:?} and rebound as {new:?}")]
    ReboundVariable {
        sym: Symbol,
        prev: IvySort,
        new: IvySort,
    },
}
