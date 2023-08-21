#![allow(dead_code)]

use crate::typechecker::sorts::IvySort;

use super::{declarations::Binding, span::Span};

#[derive(Copy, Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
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

impl Verb {
    pub fn negate(&self) -> Self {
        match self {
            Verb::Arrow => Verb::Arrow,
            Verb::Iff => Verb::Iff,
            Verb::Or => Verb::And,
            Verb::And => Verb::Or,
            Verb::Lt => Verb::Ge,
            Verb::Le => Verb::Gt,
            Verb::Gt => Verb::Le,
            Verb::Ge => Verb::Lt,
            Verb::Equals => Verb::Notequals,
            Verb::Notequals => Verb::Equals,
            _ => unreachable!(),
        }
    }

    pub fn is_logical(&self) -> bool {
        match self {
            Verb::Arrow
            | Verb::Iff
            | Verb::Or
            | Verb::And
            | Verb::Lt
            | Verb::Le
            | Verb::Gt
            | Verb::Ge
            | Verb::Equals
            | Verb::Notequals
            | Verb::Not => true,
            _ => false,
        }
    }

    pub fn is_numeric_cmp(&self) -> bool {
        match self {
            Verb::Lt | Verb::Le | Verb::Gt | Verb::Ge => true,
            _ => false,
        }
    }

    pub fn is_arith(&self) -> bool {
        match self {
            Verb::Plus | Verb::Minus | Verb::Times | Verb::Div => true,
            _ => false,
        }
    }
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

    // TODO: Deprecate - the logic parser should handle these.
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

    pub fn depth(&self) -> usize {
        1 + match self {
            Expr::App {
                expr: AppExpr { func, args },
                ..
            } => {
                let func_depth = func.depth();
                let args_depth = args.iter().map(|a| a.depth()).max().unwrap_or(0);
                usize::max(func_depth, args_depth) + 1
            }
            Expr::BinOp {
                expr: BinOp { lhs, rhs, .. },
                ..
            } => usize::max(lhs.depth(), rhs.depth()) + 1,
            Expr::Boolean { .. } => 1,
            Expr::FieldAccess {
                expr: FieldAccess { record, .. },
                ..
            } => record.depth(),
            Expr::Index {
                expr: IndexExpr { lhs, .. },
                ..
            } => lhs.depth(),
            Expr::LogicSymbol { .. } => 1,
            Expr::Number { .. } => 1,
            Expr::UnaryOp { expr, .. } => expr.depth(),
            Expr::ProgramSymbol { .. } => 1,
            Expr::This(_) => 1,
        }
    }

    pub fn n_nodes(&self) -> usize {
        1 + match self {
            Expr::App {
                expr: AppExpr { func, args },
                ..
            } => {
                let func_depth = func.n_nodes();
                let args_depth: usize = args.iter().map(|a| a.n_nodes()).sum();
                func_depth + args_depth
            }
            Expr::BinOp {
                expr: BinOp { lhs, rhs, .. },
                ..
            } => lhs.depth() + rhs.depth(),
            Expr::Boolean { .. } => 1,
            Expr::FieldAccess {
                expr: FieldAccess { record, .. },
                ..
            } => record.n_nodes(),
            Expr::Index {
                expr: IndexExpr { lhs, .. },
                ..
            } => lhs.n_nodes(),
            Expr::LogicSymbol { .. } => 1,
            Expr::Number { .. } => 1,
            Expr::UnaryOp { expr, .. } => expr.n_nodes(),
            Expr::ProgramSymbol { .. } => 1,
            Expr::This(_) => 1,
        }
    }
}
