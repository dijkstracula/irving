#![allow(dead_code)]

use super::expressions::*;
use super::logic::Fmla;
use super::statements::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclSig {
    pub name: Symbol,
    pub params: ParamList,
}

pub type DeclRet = Option<Param>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MixinSig {
    pub name: Ident,
    pub params: ParamList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActionDecl {
    pub name: Symbol,
    pub params: ParamList,
    pub ret: Option<Param>,
    pub body: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AfterDecl {
    pub name: Ident,
    pub params: Option<ParamList>,
    pub ret: Option<Param>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BeforeDecl {
    pub name: Ident,
    pub params: Option<ParamList>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDecl {
    pub name: Symbol,
    pub params: ParamList,
    pub ret: Symbol, // Am I an idiot? Where's the bee^W body
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExportDecl {
    Action(ActionDecl),
    ForwardRef(Symbol),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportDecl {
    pub name: Symbol,
    pub params: ParamList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplementDecl {
    pub name: Ident,
    pub params: ParamList,
    pub ret: Option<Param>,
    pub body: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstanceDecl {
    pub name: Symbol,
    pub sort: Ident,
    pub args: ParamList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IsolateDecl {
    pub name: Symbol,
    pub params: ParamList,
    pub body: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleDecl {
    pub name: Symbol,
    pub params: ParamList,
    pub body: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ObjectDecl {
    pub name: Symbol,
    pub params: ParamList,
    pub body: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Relation {
    pub name: Symbol,
    pub params: ParamList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub enum Decl {
    Action(ActionDecl),

    AfterAction(AfterDecl),

    Alias(Symbol, Expr),

    Attribute(Expr),

    Axiom(Fmla),

    BeforeAction(BeforeDecl),

    Common(Vec<Decl>),

    Export(ExportDecl),

    Function(FunctionDecl),

    Globals(Vec<Decl>),

    Implement(ImplementDecl),

    Implementation(Vec<Decl>),

    Import(ImportDecl),

    Isolate(IsolateDecl),

    Include(Symbol),

    Instance(InstanceDecl),

    Instantiate { name: Expr, prms: Vec<Expr> },

    Interpretation { itype: Expr, ctype: Expr },

    Invariant(Fmla),

    Module(ModuleDecl),

    Object(ObjectDecl),

    Relation(Relation),

    Specification(Vec<Decl>),

    Stmts(Vec<Stmt>),

    Var(Term),

    Type(Type),
}
