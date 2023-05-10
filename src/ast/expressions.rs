#![allow(dead_code)]

use super::declarations::{Sort, Param};

/// Corresponds to a file/line pairing, and possibly additionally docstrings to
/// be reconstructed in the extracted code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Annotation {
    docstring: Vec<String>,
    file: String,
    line: u32,
}

pub type Ident = Vec<Symbol>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Verb {
    Iff, Or, And, Lt, Le, Gt, Ge, Equals, Notequals, Not, Arrow,
    Plus, Minus, Times, Div,
    Empty, True, False, 
    Colon, Comma, Dot
}

pub type Symbol = String;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AppExpr {
    pub func: Box<Expr>,
    pub args: Vec<Expr>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IndexExpr {
    pub lhs: Box<Expr>,
    pub idx: Box<Expr>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub enum Formula {
    Forall {
        params: Vec<Param>,
        expr: Box<Expr>
    },
    Exists {
        params: Vec<Param>,
        expr: Box<Expr>
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub name: Symbol,
    pub sort: Sort
    /* spec: TypeSpec */
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Term {
    pub id: Ident,
    pub sort: Option<Ident>,
    //is_destructor: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub enum Expr {
    App(AppExpr),

    BinOp {
        lhs: Box<Expr>,
        op: Verb,
        rhs: Box<Expr>
    },

    Formula(Formula),

    Identifier(Ident),

    Index(IndexExpr),

    Number(i64),

    Pi {
        terms: Vec<Expr>,
        body: Box<Expr>,
    },

    Subscript {
        val: Box<Expr>,
        subscripts: Vec<Expr>
    },

    UnaryOp{op: Verb, expr: Box<Expr>},

    Term(Term)
}