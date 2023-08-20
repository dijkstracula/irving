use super::{
    expressions::{self, Expr, ParamList, Symbol},
    span::Span,
};

/*
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LogicVar {
    pub name: String,
    pub sort: Option<String>
}
*/

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Fmla {
    Forall {
        span: Span,
        fmla: Forall,
    },
    Exists {
        span: Span,
        fmla: Exists,
    },
    Pred(Expr),

    App {
        span: Span,
        app: LogicApp,
    },

    BinOp {
        span: Span,
        binop: LogicBinOp,
    },

    Boolean {
        span: Span,
        val: bool,
    },

    FieldAccess {
        span: Span,
        fmla: FieldAccess,
    },

    Number {
        span: Span,
        val: i64,
    },

    LogicSymbol {
        span: Span,
        sym: Symbol,
    },

    ProgramSymbol {
        span: Span,
        sym: Symbol,
    },

    UnaryOp {
        span: Span,
        op: expressions::Verb,
        fmla: Box<Fmla>,
    },
}

impl Fmla {
    pub fn span(&self) -> &Span {
        match self {
            Fmla::Forall { span, .. } => span,
            Fmla::Exists { span, .. } => span,
            Fmla::Pred(expr) => expr.span(),
            Fmla::App { span, .. } => span,
            Fmla::BinOp { span, .. } => span,
            Fmla::Boolean { span, .. } => span,
            Fmla::FieldAccess { span, .. } => span,
            Fmla::LogicSymbol { span, .. } => span,
            Fmla::Number { span, .. } => span,
            Fmla::ProgramSymbol { span, .. } => span,
            Fmla::UnaryOp { span, .. } => span,
        }
    }

    pub fn contains_logicvar(&self) -> bool {
        match self {
            Fmla::Forall { .. } => true,
            Fmla::Exists { .. } => true,
            Fmla::Pred(_) => false,
            Fmla::App { app, .. } => app.args.iter().any(|arg| arg.is_quantified()),
            Fmla::BinOp { binop, .. } => binop.lhs.is_quantified() || binop.rhs.is_quantified(),
            Fmla::Boolean { .. } => false,
            Fmla::FieldAccess { fmla, .. } => fmla.record.is_quantified(),
            Fmla::Number { .. } => false,
            Fmla::LogicSymbol { .. } => true,
            Fmla::ProgramSymbol { .. } => false,
            Fmla::UnaryOp { fmla, .. } => fmla.is_quantified()
        }
    }

    pub fn is_quantified(&self) -> bool {
        match self {
            Fmla::Forall { .. } => true,
            Fmla::Exists { .. } => true,
            _ => false,
        }
    }

}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Exists {
    pub vars: ParamList,
    pub fmla: Box<Fmla>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Forall {
    pub vars: ParamList,
    pub fmla: Box<Fmla>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LogicBinOp {
    pub lhs: Box<Fmla>,
    pub op: expressions::Verb,
    pub rhs: Box<Fmla>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LogicApp {
    pub func: Box<Fmla>,
    pub args: Vec<Fmla>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldAccess {
    pub record: Box<Fmla>,
    pub field: Symbol,
}