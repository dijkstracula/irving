#![allow(dead_code)]

use crate::typechecker::sorts::IvySort;

/// Corresponds to a file/line pairing, and possibly additionally docstrings to
/// be reconstructed in the extracted code.
#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct Annotation {
    docstring: Vec<String>,
    file: String,
    line: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
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

pub type Token = String;
pub type Ident = Vec<Token>;

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct Symbol {
    pub id: Token,
    pub sort: Sort,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct AppExpr {
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct BinOp {
    pub lhs: Box<Expr>,
    pub op: Verb,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct FieldAccess {
    pub record: Box<Expr>,
    pub field: Symbol,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct IndexExpr {
    pub lhs: Box<Expr>,
    pub idx: Box<Expr>,
}

pub type ParamList = Vec<Symbol>;

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub enum Sort {
    ToBeInferred,
    Annotated(Ident),
    Resolved(IvySort),
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct Type {
    pub ident: TypeName,
    pub sort: IvySort, /* spec: TypeSpec */
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub enum TypeName {
    Name(Token),
    This,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
#[allow(clippy::large_enum_variant)]
pub enum Expr {
    App(AppExpr),

    BinOp(BinOp),

    Boolean(bool),

    FieldAccess(FieldAccess),

    Index(IndexExpr),

    Number(i64),

    UnaryOp { op: Verb, expr: Box<Expr> },

    Symbol(Symbol),

    This,
}

impl Expr {
    pub fn inferred_symbol(s: String) -> Self {
        Self::Symbol(Symbol {
            id: s,
            sort: Sort::ToBeInferred,
        })
    }
    pub fn annotated_symbol(s: String, id: Ident) -> Self {
        Self::Symbol(Symbol {
            id: s,
            sort: Sort::Annotated(id),
        })
    }
}
