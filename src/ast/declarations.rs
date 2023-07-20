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

pub type DeclRet = Option<AnnotatedSymbol>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModSig {
    pub name: Symbol,
    pub sortsyms: Vec<Symbol>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MixinSig {
    pub name: Ident,
    pub params: Option<ParamList>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActionDecl {
    pub params: ParamList,
    pub ret: Option<AnnotatedSymbol>,
    pub body: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActionMixinDecl {
    pub name: Ident,
    pub params: Option<ParamList>,
    pub ret: Option<AnnotatedSymbol>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDecl {
    pub params: ParamList,
    pub ret: Symbol, // Am I an idiot? Where's the bee^W body
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExportDecl {
    Action(Binding<ActionDecl>),
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
    pub ret: Option<AnnotatedSymbol>,
    pub body: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstanceDecl {
    pub sort: Ident,
    pub args: ParamList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleDecl {
    pub sortsyms: Vec<Symbol>,
    pub body: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ObjectDecl {
    pub params: ParamList,
    pub fields: Vec<Decl>,

    // The non-action and non-field declarations go here, stuff like
    // axioms, attributes, invariants, etc...
    pub body: Vec<Decl>,
}

impl ObjectDecl {
    pub fn actions<'a>(&'a self) -> Vec<&'a Binding<ActionDecl>> {
        self.body
            .iter()
            .filter_map(|d| match d {
                Decl::Export(ExportDecl::Action(binding)) | Decl::Action(binding) => Some(binding),
                _ => None,
            })
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Relation {
    pub params: ParamList,
}

// Transformed AST nodes

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding<T> {
    pub name: String,
    pub decl: T,
}

impl<T> Binding<T> {
    pub fn from(name: String, decl: T) -> Self {
        Self { name, decl }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub enum Decl {
    Action(Binding<ActionDecl>),

    AfterAction(ActionMixinDecl),

    Alias(Binding<Sort>),

    Attribute(Expr),

    Axiom(Fmla),

    BeforeAction(ActionMixinDecl),

    Common(Vec<Decl>),

    Export(ExportDecl),

    Function(Binding<FunctionDecl>),

    Globals(Vec<Decl>),

    Implement(ImplementDecl),

    Import(ImportDecl),

    Include(Symbol),

    Instance(Binding<InstanceDecl>),

    Instantiate { name: Expr, prms: Vec<Expr> },

    Interpretation { itype: Expr, ctype: Expr },

    Invariant(Fmla),

    Module(Binding<ModuleDecl>),

    Noop,

    Object(Binding<ObjectDecl>),

    Relation(Binding<Relation>),

    Stmts(Vec<Stmt>),

    Var(Binding<Sort>),

    Type(Binding<Sort>),
}

impl Decl {
    /// For declarations that bind a new name, produce that name.
    /// TODO: https://github.com/dijkstracula/irving/issues/19
    pub fn name_for_binding(&self) -> Option<&str> {
        match self {
            Decl::Action(Binding { name, .. }) => Some(&name),
            Decl::AfterAction(_) => None,
            Decl::Alias(Binding { name, .. }) => Some(&name),
            Decl::Attribute(_) => None,
            Decl::Axiom(_) => None,
            Decl::BeforeAction(_) => None,
            Decl::Common(_) => None,
            Decl::Export(_) => None,
            Decl::Function(Binding { name, .. }) => Some(&name),
            Decl::Globals(_) => None,
            Decl::Implement(_) => None,
            Decl::Import(_) => None,
            Decl::Include(_) => None,
            Decl::Instance(i) => Some(&i.name),
            Decl::Instantiate { .. } => None,
            Decl::Interpretation { .. } => None,
            Decl::Invariant(_) => None,
            Decl::Module(Binding { name, .. }) => Some(&name),
            Decl::Noop => None,
            Decl::Object(Binding { name, .. }) => Some(&name),
            Decl::Relation(Binding { name, .. }) => Some(&name),
            Decl::Stmts(_) => None,
            Decl::Var(Binding { name, .. }) => Some(&name),
            Decl::Type(Binding { name, .. }) => Some(&name),
        }
    }

    pub fn as_action(self) -> Option<Binding<ActionDecl>> {
        match self {
            Decl::Action(binding) => Some(binding),
            _ => None,
        }
    }
}
