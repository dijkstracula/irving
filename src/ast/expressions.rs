#![allow(dead_code)]

use std::rc::Rc;

use pest::{error::ErrorVariant, iterators::Pair};
use pest_consume::Error;

use crate::{
    parser::{expressions::parse_rval, ivy::Rule},
    typechecker::sorts::IvySort,
};

use super::span::Span;

// Corresponds to a file/line pairing, and possibly additionally docstrings to
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
    pub func: Box<ExprKind>,
    pub args: Vec<ExprKind>,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct BinOp {
    pub lhs: Box<ExprKind>,
    pub op: Verb,
    pub rhs: Box<ExprKind>,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct FieldAccess {
    pub record: Box<ExprKind>,
    pub field: Symbol,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct IndexExpr {
    pub lhs: Box<ExprKind>,
    pub idx: Box<ExprKind>,
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
pub enum ExprKind {
    App(AppExpr),

    BinOp(BinOp),

    Boolean(bool),

    FieldAccess(FieldAccess),

    Index(IndexExpr),

    LogicSymbol(Symbol),

    Number(i64),

    UnaryOp { op: Verb, expr: Box<Expr> },

    ProgramSymbol(Symbol),

    This,
}

impl ExprKind {
    pub fn inferred_progsym(s: String) -> Self {
        Self::ProgramSymbol(Symbol {
            id: s,
            sort: Sort::ToBeInferred,
        })
    }
    pub fn annotated_progsym(s: String, id: Ident) -> Self {
        Self::ProgramSymbol(Symbol {
            id: s,
            sort: Sort::Annotated(id),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct Expr {
    span: Span,
    expr: ExprKind,
}

impl Expr {
    pub fn from_pest_span(input: Rc<str>, span: pest::Span, expr: ExprKind) -> Self {
        let span = Span::from_pest(input, span);
        Self { span, expr }
    }

    pub fn boolean_with_span(input: Rc<str>, span: pest::Span, b: bool) -> Self {
        Expr::from_pest_span(input, span, ExprKind::Boolean(b))
    }

    fn number_with_span(input: Rc<str>, span: pest::Span, n: i64) -> Self {
        Expr::from_pest_span(input, span, ExprKind::Number(n))
    }

    pub fn program_symbol_with_span<S>(input: Rc<str>, span: pest::Span, id: S, sort: Sort) -> Self
    where
        S: Into<String>,
    {
        Expr::from_pest_span(
            input,
            span,
            ExprKind::LogicSymbol(Symbol {
                id: id.into(),
                sort,
            }),
        )
    }

    pub fn logic_symbol_with_span<S>(input: Rc<str>, span: pest::Span, id: S, sort: Sort) -> Self
    where
        S: Into<String>,
    {
        Expr::from_pest_span(
            input,
            span,
            ExprKind::LogicSymbol(Symbol {
                id: id.into(),
                sort,
            }),
        )
    }

    pub fn this_with_span(input: Rc<str>, span: pest::Span) -> Self {
        Expr::from_pest_span(input, span, ExprKind::This)
    }

    pub fn expr_from_pair<'a>(
        input: Rc<str>,
        pair: Pair<'a, Rule>,
    ) -> Result<Self, pest::error::Error<Rule>> {
        let span = pair.as_span();
        match pair.as_rule() {
            Rule::THIS => Ok(Expr::this_with_span(input, span)),
            Rule::LOGICTOK => Ok(Expr::logic_symbol_with_span(
                input,
                span,
                pair.as_str(),
                Sort::ToBeInferred,
            )),
            Rule::PROGTOK => Ok(Expr::program_symbol_with_span(
                input,
                span,
                pair.as_str(),
                Sort::ToBeInferred,
            )),
            Rule::boollit => {
                let val = match pair.as_str() {
                    "true" => true,
                    "false" => false,
                    _ => unreachable!(),
                };
                Ok(Expr::boolean_with_span(input, span, val))
            }
            Rule::number => {
                let val: i64 = pair.as_str().parse().unwrap();
                Ok(Expr::number_with_span(input, span, val))
            }
            Rule::rval => parse_rval(input, pair.into_inner()),
            _ => Err(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: "Expected primary expression".into(),
                },
                span,
            )),
        }
    }
}
