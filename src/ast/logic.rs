use super::{
    expressions::{self, ParamList, Symbol},
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
            Fmla::App { app, .. } => app.args.iter().any(|arg| arg.is_quantified()),
            Fmla::BinOp { binop, .. } => binop.lhs.is_quantified() || binop.rhs.is_quantified(),
            Fmla::Boolean { .. } => false,
            Fmla::FieldAccess { fmla, .. } => fmla.record.is_quantified(),
            Fmla::Number { .. } => false,
            Fmla::LogicSymbol { .. } => true,
            Fmla::ProgramSymbol { .. } => false,
            Fmla::UnaryOp { fmla, .. } => fmla.is_quantified(),
        }
    }

    pub fn is_quantified(&self) -> bool {
        match self {
            Fmla::Forall { .. } => true,
            Fmla::Exists { .. } => true,
            _ => false,
        }
    }

    pub fn depth(&self) -> usize {
        match self {
            Fmla::Forall {
                fmla: Forall { fmla, .. },
                ..
            } => fmla.depth() + 1,
            Fmla::Exists {
                fmla: Exists { fmla, .. },
                ..
            } => fmla.depth() + 1,

            Fmla::App {
                app: LogicApp { func, args },
                ..
            } => {
                let func_depth = func.depth();
                let args_depth = args.iter().map(|arg| arg.depth()).max().unwrap_or(0);
                usize::max(func_depth, args_depth) + 1
            }
            Fmla::BinOp {
                binop: LogicBinOp { lhs, rhs, .. },
                ..
            } => usize::max(lhs.depth(), rhs.depth()) + 1,
            Fmla::Boolean { .. } => 1,
            Fmla::FieldAccess {
                fmla: FieldAccess { record, .. },
                ..
            } => record.depth() + 1,
            Fmla::Number { .. } => 1,
            Fmla::LogicSymbol { .. } => 1,
            Fmla::ProgramSymbol { .. } => 1,
            Fmla::UnaryOp { fmla, .. } => fmla.depth() + 1,
        }
    }
}

impl From<&Box<expressions::Expr>> for Box<Fmla> {
    fn from(value: &Box<expressions::Expr>) -> Self {
        Box::new(value.as_ref().into())
    }
}

impl From<&expressions::Expr> for Fmla {
    fn from(value: &expressions::Expr) -> Self {
        match value {
            expressions::Expr::App { span, expr } => Fmla::App {
                span: span.clone(),
                app: expr.into(),
            },
            expressions::Expr::BinOp { span, expr } => Fmla::BinOp {
                span: span.clone(),
                binop: expr.into(),
            },
            expressions::Expr::Boolean { span, val } => Fmla::Boolean {
                span: span.clone(),
                val: *val,
            },
            expressions::Expr::FieldAccess { span, expr } => Fmla::FieldAccess {
                span: span.clone(),
                fmla: expr.into(),
            },
            expressions::Expr::Index { span: _, expr: _ } => todo!(),
            expressions::Expr::Number { span, val } => Fmla::Number {
                span: span.clone(),
                val: *val,
            },
            expressions::Expr::UnaryOp { span, op, expr } => Fmla::UnaryOp {
                span: span.clone(),
                op: *op,
                fmla: expr.into(),
            },
            expressions::Expr::ProgramSymbol { sym } => Fmla::ProgramSymbol {
                span: sym.span.clone(),
                sym: sym.clone(),
            },
            expressions::Expr::This(_) => todo!(),
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

impl From<&expressions::BinOp> for LogicBinOp {
    fn from(value: &expressions::BinOp) -> Self {
        LogicBinOp {
            lhs: Box::new(value.lhs.as_ref().into()),
            op: value.op,
            rhs: Box::new(value.rhs.as_ref().into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LogicApp {
    pub func: Box<Fmla>,
    pub args: Vec<Fmla>,
}

impl From<&expressions::AppExpr> for LogicApp {
    fn from(value: &expressions::AppExpr) -> Self {
        LogicApp {
            func: (&value.func).into(),
            args: value.args.iter().map(|expr| expr.into()).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldAccess {
    pub record: Box<Fmla>,
    pub field: Symbol,
}

impl From<&expressions::FieldAccess> for FieldAccess {
    fn from(value: &expressions::FieldAccess) -> Self {
        FieldAccess {
            record: (&value.record).into(),
            field: value.field.clone(),
        }
    }
}
