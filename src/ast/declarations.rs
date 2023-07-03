#![allow(dead_code)]

use super::expressions::*;
use super::logic::Fmla;
use super::statements::*;

// Syntactic AST nodes

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclSig {
    pub name: Symbol,
    pub params: ParamList,
}

pub type DeclRet = Option<Param>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModSig {
    pub name: Symbol,
    pub params: Vec<Symbol>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MixinSig {
    pub name: Ident,
    pub params: Option<ParamList>,
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
    pub params: Option<ParamList>,
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
    pub params: Vec<Symbol>,
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

// Transformed AST nodes

/// Created by the ModuleNormalizer pass.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NormalizedIsolateDecl {
    pub name: Symbol,
    pub params: ParamList,
    pub impl_decls: Vec<Decl>,
    pub spec_decls: Vec<Decl>,
    pub common_spec_decls: Vec<Decl>,
    pub common_impl_decls: Vec<Decl>,
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

    NormalizedIsolate(NormalizedIsolateDecl),

    Noop,

    Object(ObjectDecl),

    Relation(Relation),

    Specification(Vec<Decl>),

    Stmts(Vec<Stmt>),

    Var(Term),

    Type(Type),
}

impl Decl {
    /// For declarations that bind a new name, produce that name.
    /// TODO: https://github.com/dijkstracula/irving/issues/19
    pub fn name_for_binding(&self) -> Option<&str> {
        match self {
            Decl::Action(a) => Some(&a.name),
            Decl::AfterAction(_) => None,
            Decl::Alias(name, _) => Some(&name),
            Decl::Attribute(_) => None,
            Decl::Axiom(_) => None,
            Decl::BeforeAction(_) => None,
            Decl::Common(_) => None,
            Decl::Export(_) => None,
            Decl::Function(f) => Some(&f.name),
            Decl::Globals(_) => None,
            Decl::Implement(_) => None,
            Decl::Implementation(_) => None,
            Decl::Import(_) => None,
            Decl::Isolate(i) => Some(&i.name),
            Decl::Include(_) => None,
            Decl::Instance(i) => Some(&i.name),
            Decl::Instantiate { .. } => None,
            Decl::Interpretation { .. } => None,
            Decl::Invariant(_) => None,
            Decl::Module(m) => Some(&m.name),
            Decl::NormalizedIsolate(n) => Some(&n.name),
            Decl::Noop => None,
            Decl::Object(o) => Some(&o.name),
            Decl::Relation(r) => Some(&r.name),
            Decl::Specification(_) => None,
            Decl::Stmts(_) => None,
            Decl::Var(v) => Some(&v.id),
            Decl::Type(Type {
                ident: TypeName::Name(n),
                ..
            }) => Some(&n),
            Decl::Type(Type {
                ident: TypeName::This,
                ..
            }) => Some("this".into()),
        }
    }
}
