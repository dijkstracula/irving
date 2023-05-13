#![allow(dead_code)]

use super::expressions::*;
use super::statements::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeclSig {
    pub name: Vec<Symbol>,
    pub params: Vec<Param>,
}

pub type DeclRet = Option<Param>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ActionDecl {
    pub name: Vec<String>,
    pub params: Vec<Param>,
    pub ret: Option<Param>,
    pub body: Option<Vec<Decl>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AfterDecl {
    pub name: Vec<String>,
    pub params: Option<Vec<Param>>,
    pub ret:  Option<Param>,
    pub body: Vec<Decl>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BeforeDecl {
    pub name: Vec<String>,
    pub params: Option<Vec<Param>>,
    pub body: Vec<Decl>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionDecl {
    pub name: Vec<String>,
    pub params: Vec<Param>,
    pub ret: Symbol
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExportDecl {
    Action(ActionDecl),
    ForwardRef(Vec<Symbol>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImportDecl {
    pub name: Symbol,
    pub params: Vec<Param>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstanceDecl {
    pub name: Vec<Symbol>,
    pub sort: Vec<Symbol>,
    pub args: Vec<Param>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IsolateDecl {
    pub name: Symbol,
    pub params: Vec<Param>,
    pub body: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleDecl {
    pub name: Vec<String>,
    pub params: Vec<Param>,
    pub body: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjectDecl {
    pub name: Vec<String>,
    pub params: Vec<Param>,
    pub body: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Relation {
    pub name: Vec<String>,
    pub params: Vec<Param>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Sort {
    Range(Box<Expr>, Box<Expr>),
    Enum(Vec<Symbol>),
    Subclass(Symbol),
    Uninterpreted,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(clippy::large_enum_variant)]
pub enum Decl {

    Action(ActionDecl),

    AfterAction(AfterDecl),

    Alias(Symbol, Expr),

    Axiom(Expr),

    BeforeAction(BeforeDecl),

    Export(ExportDecl),

    Function(FunctionDecl),

    Globals(Vec<Decl>),

    Import(ImportDecl),

    Isolate(IsolateDecl),

    Include(Symbol),

    Instance(InstanceDecl),

    Instantiate {
        name: Expr,
        prms: Vec<Expr>,
    },

    Interpretation {
        itype: Expr,
        ctype: Expr,
    },
    
    Invariant(Expr),

    Module(ModuleDecl),

    Object(ObjectDecl),

    Relation(Relation),

    Stmts(Vec<Stmt>),

    Var(Term),

    Type(Type),
}
