#![allow(dead_code)]

use crate::typechecker::sorts::IvySort;

use super::{declarations::Binding, span::Span};

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

pub type Symbol = Binding<Sort>;

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

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
#[allow(clippy::large_enum_variant)]
pub enum Expr {
    App {
        span: Span,
        expr: AppExpr,
    },

    BinOp {
        span: Span,
        expr: BinOp,
    },

    Boolean {
        span: Span,
        val: bool,
    },

    FieldAccess {
        span: Span,
        expr: FieldAccess,
    },

    Index {
        span: Span,
        expr: IndexExpr,
    },

    LogicSymbol {
        span: Span,
        sym: Symbol,
    },

    Number {
        span: Span,
        val: i64,
    },

    UnaryOp {
        span: Span,
        op: Verb,
        expr: Box<Expr>,
    },

    ProgramSymbol {
        span: Span,
        sym: Symbol,
    },

    This(Span),
}

impl Expr {
    pub fn span(&self) -> &Span {
        match self {
            Expr::App { span, .. } => span,
            Expr::BinOp { span, .. } => span,
            Expr::Boolean { span, .. } => span,
            Expr::FieldAccess { span, .. } => span,
            Expr::Index { span, .. } => span,
            Expr::LogicSymbol { span, .. } => span,
            Expr::Number { span, .. } => span,
            Expr::UnaryOp { span, .. } => span,
            Expr::ProgramSymbol { span, .. } => span,
            Expr::This(span) => span,
        }
    }
}
