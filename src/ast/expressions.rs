#![allow(dead_code)]

use crate::typechecker::sorts::IvySort;

/// Corresponds to a file/line pairing, and possibly additionally docstrings to
/// be reconstructed in the extracted code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annotation {
    docstring: Vec<String>,
    file: String,
    line: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnnotatedSymbol {
    // TODO: this should become a Symbol.
    pub id: Symbol,
    pub sort: Sort,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AppExpr {
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinOp {
    pub lhs: Box<Expr>,
    pub op: Verb,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldAccess {
    pub record: Box<Expr>,
    pub field: AnnotatedSymbol,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexExpr {
    pub lhs: Box<Expr>,
    pub idx: Box<Expr>,
}

pub type ParamList = Vec<AnnotatedSymbol>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Sort {
    ToBeInferred,
    Annotated(Ident),
    Resolved(IvySort),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub ident: TypeName,
    pub sort: IvySort, /* spec: TypeSpec */
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeName {
    Name(Symbol),
    This,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub enum Expr {
    // TODO: this should become a Symbol.
    AnnotatedSym(AnnotatedSymbol),

    App(AppExpr),

    BinOp(BinOp),

    Boolean(bool),

    FieldAccess(FieldAccess),

    Index(IndexExpr),

    Number(i64),

    UnaryOp { op: Verb, expr: Box<Expr> },

    This,
}

impl Expr {
    pub fn inferred_symbol(s: String) -> Self {
        Self::AnnotatedSym(AnnotatedSymbol {
            id: s,
            sort: Sort::ToBeInferred,
        })
    }
    pub fn annotated_symbol(s: String, id: Ident) -> Self {
        Self::AnnotatedSym(AnnotatedSymbol {
            id: s,
            sort: Sort::Annotated(id),
        })
    }
}
