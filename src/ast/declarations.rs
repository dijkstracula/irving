#![allow(dead_code)]

use super::expressions::*;
use super::logic::Fmla;
use super::statements::*;

// Syntactic AST nodes

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclSig {
    pub name: Token,
    pub params: ParamList,
}

pub type DeclRet = Option<Symbol>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModSig {
    pub name: Token,
    pub sortsyms: Vec<Token>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MixinSig {
    pub name: Ident,
    pub params: Option<ParamList>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActionDecl {
    pub params: ParamList,
    pub ret: Option<Symbol>,
    pub body: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActionMixinDecl {
    pub name: Ident,
    pub params: Option<ParamList>,
    pub ret: Option<Symbol>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDecl {
    pub params: ParamList,
    pub ret: Token, // Am I an idiot? Where's the bee^W body
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExportDecl {
    Action(Binding<ActionDecl>),
    ForwardRef(Token),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportDecl {
    pub name: Token,
    pub params: ParamList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstanceDecl {
    pub sort: Ident,
    pub args: ParamList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterpretDecl {
    pub name: Token,
    pub sort: Sort,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleDecl {
    pub sortsyms: Vec<Token>,
    pub body: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ObjectDecl {
    pub params: ParamList,

    // The non-action and non-field declarations go here, stuff like
    // axioms, attributes, invariants, etc...
    pub body: Vec<Decl>,
}

impl ObjectDecl {
    pub fn actions(&mut self) -> Vec<&mut Decl> {
        self.body
            .iter_mut()
            .filter_map(|d| match d {
                Decl::Export(ExportDecl::Action(_)) | Decl::Action(_) => Some(d),
                _ => None,
            })
            .collect()
    }

    pub fn vars(&mut self) -> Vec<&mut Decl> {
        self.body
            .iter_mut()
            .filter_map(|d| match d {
                Decl::Var(_) => Some(d),
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Binding<T> {
    pub name: String,
    pub decl: T,
}

impl<T> Binding<T> {
    pub fn from<S>(name: S, decl: T) -> Self
    where
        S: Into<String>,
    {
        Self {
            name: name.into(),
            decl,
        }
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

    Implement(ActionMixinDecl),

    Import(ImportDecl),

    Include(Token),

    Instance(Binding<InstanceDecl>),

    Instantiate { name: Expr, prms: Vec<Expr> },

    Interpret(InterpretDecl),

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
    pub fn name_for_binding(&self) -> Option<&str> {
        match self {
            Decl::Action(Binding { name, .. }) => Some(name),
            Decl::AfterAction(_) => None,
            Decl::Alias(Binding { name, .. }) => Some(name),
            Decl::Attribute(_) => None,
            Decl::Axiom(_) => None,
            Decl::BeforeAction(_) => None,
            Decl::Common(_) => None,
            Decl::Export(ExportDecl::Action(Binding { name, .. })) => Some(name),
            Decl::Export(ExportDecl::ForwardRef(name)) => Some(name),
            Decl::Function(Binding { name, .. }) => Some(name),
            Decl::Globals(_) => None,
            Decl::Implement(_) => None,
            Decl::Import(_) => None,
            Decl::Include(_) => None,
            Decl::Instance(i) => Some(&i.name),
            Decl::Instantiate { .. } => None,
            Decl::Interpret { .. } => None,
            Decl::Invariant(_) => None,
            Decl::Module(Binding { name, .. }) => Some(name),
            Decl::Noop => None,
            Decl::Object(Binding { name, .. }) => Some(name),
            Decl::Relation(Binding { name, .. }) => Some(name),
            Decl::Stmts(_) => None,
            Decl::Var(Binding { name, .. }) => Some(name),
            Decl::Type(Binding { name, .. }) => Some(name),
        }
    }

    pub fn as_action(self) -> Option<Binding<ActionDecl>> {
        match self {
            Decl::Action(binding) => Some(binding),
            _ => None,
        }
    }
}
