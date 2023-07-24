#![allow(dead_code)]

use thiserror::Error;

use crate::ast::{declarations::Decl, expressions::Token};

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
    MissingRecordField(IvySort, Token),

    #[error("Unbound variable {0}")]
    UnboundVariable(Token),

    #[error("Sort {0:?} mismatches {1:?}")]
    UnificationError(IvySort, IvySort),

    #[error("Sort sequence {0:?} mismatches {1:?}")]
    LenMismatch(Vec<IvySort>, Vec<IvySort>),

    #[error("Token {expected:?} redefined as {actual:?}")]
    FargMismatch { expected: Token, actual: Token },

    #[error("Token {sym:?} defined as {prev:?} and rebound as {new:?}")]
    ReboundVariable {
        sym: Token,
        prev: IvySort,
        new: IvySort,
    },
}
