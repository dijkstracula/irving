use thiserror::Error;

use crate::{
    ast::{expressions::Token, span::Span},
    visitor::VisitorResult,
};

use self::sorts::IvySort;

pub(crate) mod inference;
pub(crate) mod programs;
pub(crate) mod sorts;
pub(crate) mod subst;
pub(crate) mod unifier;

pub type InferenceResult<N> = VisitorResult<IvySort, TypeError, N>;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum TypeError {
    #[error("Invalid kind of action kind???")]
    ActionKindMismatch,

    #[error("Value can't be called")]
    InvalidApplication,

    #[error("Value can't be called in a logical context")]
    InvalidLogicApp,

    #[error("Expected {expected:?}, got {actual:?}")]
    SortMismatch { expected: String, actual: String },

    #[error("Declaration doesn't bind a name")]
    NonBindingDecl,

    #[error("{0:?} cannot be instantiated (did you mean to use `var`?)")]
    NotInstanceable(String),

    #[error("{0:?} cannot be indexed into with `.`")]
    NotARecord(String),

    #[error("Undefined field {0} ")]
    MissingRecordField(Token),

    #[error("{0:?} cannot be subclassed")]
    NonClassInheritance(IvySort),

    #[error("Unbound variable {0}")]
    UnboundVariable(Token),

    #[error("Sort {0} mismatches {1}")]
    UnificationError(String, String),

    #[error("Sequence of length {actual} received; expected {expected}")]
    LenMismatch { expected: usize, actual: usize },

    #[error("Token {expected:?} redefined as {actual:?}")]
    TokenMismatch { expected: Token, actual: Token },

    #[error("Token {sym:?} defined as {prev:?} and rebound as {new:?}")]
    ReboundVariable {
        sym: Token,
        prev: String,
        new: String,
    },

    #[error("{inner} at:\n{span}")]
    Spanned { span: Span, inner: Box<TypeError> },
}

impl TypeError {
    pub fn unification_error(lhs: &IvySort, rhs: &IvySort) -> Self {
        Self::UnificationError(format!("{}", lhs), format!("{}", rhs))
    }

    pub fn wrap(self, span: &Span) -> Self {
        match self {
            TypeError::Spanned { span, inner } => match span {
                Span::Todo => TypeError::Spanned {
                    span: span.clone(),
                    inner,
                },
                _ => TypeError::Spanned { span, inner },
            },
            e => TypeError::Spanned {
                span: span.clone(),
                inner: Box::new(e),
            },
        }
    }
}
