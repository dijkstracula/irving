use thiserror::Error;

use crate::{ast::expressions::Token, visitor::VisitorResult};

use self::sorts::IvySort;

pub(crate) mod inference;
pub(crate) mod programs;
pub(crate) mod sorts;
pub(crate) mod subst;
pub(crate) mod unifier;

pub type InferenceResult<N> = VisitorResult<IvySort, TypeError, N>;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum TypeError {
    #[error("Value can't be called")]
    InvalidApplication,

    #[error("Expected {expected:?}, got {actual:?}")]
    SortMismatch { expected: String, actual: String },

    #[error("Declaration doesn't bind a name")]
    NonBindingDecl,

    #[error("{0:?} cannot be instantiated (did you mean to use `var`?)")]
    NotInstanceable(&'static str),

    #[error("{0:?} cannot be indexed into with `.`")]
    NotARecord(&'static str),

    #[error("Undefined field {0} ")]
    MissingRecordField(Token),

    #[error("Unbound variable {0}")]
    UnboundVariable(Token),

    #[error("Sort {0:?} mismatches {1:?}")]
    UnificationError(String, String),

    #[error("Sequence of length {expected:?} mismatches {actual:?}")]
    LenMismatch { expected: usize, actual: usize },

    #[error("Token {expected:?} redefined as {actual:?}")]
    TokenMismatch { expected: Token, actual: Token },

    #[error("Token {sym:?} defined as {prev:?} and rebound as {new:?}")]
    ReboundVariable {
        sym: Token,
        prev: String,
        new: String,
    },
}

impl TypeError {
    pub fn unification_error(lhs: &IvySort, rhs: &IvySort) -> Self {
        Self::UnificationError(format!("{}", lhs), format!("{}", rhs))
    }
}
