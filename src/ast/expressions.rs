#![allow(dead_code)]

use crate::typechecker::sorts::IvySort;

/// Corresponds to a file/line pairing, and possibly additionally docstrings to
/// be reconstructed in the extracted code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Annotation {
    docstring: Vec<String>,
    file: String,
    line: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Verb {
    Iff,
    Or,
    And,
    Lt,
    Le,
    Gt,
    Ge,
    Equals,
    Notequals,
    Not,
    Arrow,
    Plus,
    Minus,
    Times,
    Div,
    Dot, /* TODO: Mod????? */
}

pub type Symbol = String;
pub type Ident = Vec<Symbol>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AppExpr {
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinOp {
    pub lhs: Box<Expr>,
    pub op: Verb,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IndexExpr {
    pub lhs: Box<Expr>,
    pub idx: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub id: Symbol,
    pub sort: Option<Ident>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeName {
    Name(Symbol),
    This,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub ident: TypeName,
    pub sort: IvySort, /* spec: TypeSpec */
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Term {
    pub id: Symbol,
    pub sort: Option<Ident>,
    //is_destructor: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub enum Expr {
    App(AppExpr),

    BinOp(BinOp),

    Boolean(bool),

    FieldAccess {
        record: Box<Expr>,
        field: Symbol,
    },

    Index(IndexExpr),

    Number(i64),

    Symbol(Symbol),

    UnaryOp {
        op: Verb,
        expr: Box<Expr>,
    },

    Term(Term),

    This,
}
