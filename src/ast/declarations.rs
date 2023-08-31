#![allow(dead_code)]

use super::expressions::{self, *};
use super::logic::Fmla;
use super::span::Span;
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
                Decl::Export {
                    decl: ExportDecl::Action { .. },
                    ..
                }
                | Decl::Import { .. }
                | Decl::Action { .. } => Some(d),
                _ => None,
            })
            .collect()
    }

    pub fn params(&mut self) -> &mut expressions::ParamList {
        &mut self.params
    }

    pub fn vars(&mut self) -> Vec<&mut Decl> {
        self.body
            .iter_mut()
            .filter_map(|d| match d {
                Decl::Var { .. } => Some(d),
                _ => None,
            })
            .collect()
    }

    pub fn subobjects(&mut self) -> Vec<&mut Decl> {
        self.body
            .iter_mut()
            .filter_map(|d| match d {
                Decl::Module { .. } | Decl::Object { .. } => Some(d),
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
    Action {
        span: Span,
        decl: Binding<ActionDecl>,
    },

    AfterAction {
        span: Span,
        decl: ActionMixinDecl,
    },

    Alias {
        span: Span,
        decl: Binding<Sort>,
    },

    Attribute {
        span: Span,
        decl: Expr,
    },

    Axiom {
        span: Span,
        decl: Fmla,
    },

    BeforeAction {
        span: Span,
        decl: ActionMixinDecl,
    },

    Common {
        span: Span,
        decl: Vec<Decl>,
    },

    Export {
        span: Span,
        decl: ExportDecl,
    },

    Function {
        span: Span,
        decl: Binding<FunctionDecl>,
    },

    Globals(Vec<Decl>),

    Implement {
        span: Span,
        decl: ActionMixinDecl,
    },

    Import {
        span: Span,
        decl: ImportDecl,
    },

    Include {
        span: Span,
        decl: Token,
    },

    Instance {
        span: Span,
        decl: Binding<InstanceDecl>,
    },

    Instantiate {
        name: Expr,
        prms: Vec<Expr>,
    },

    Interpret {
        span: Span,
        decl: InterpretDecl,
    },

    Invariant {
        span: Span,
        decl: Fmla,
    },

    Module {
        span: Span,
        decl: Binding<ModuleDecl>,
    },

    Noop,

    Object {
        span: Span,
        decl: Binding<ObjectDecl>,
    },

    Relation {
        span: Span,
        decl: Binding<Relation>,
    },

    Stmts(Vec<Stmt>),

    Var {
        span: Span,
        decl: Binding<Sort>,
    },

    Type {
        span: Span,
        decl: Binding<Sort>,
    },
}

impl Decl {
    /// For declarations that bind a new name, produce that name.
    pub fn name_for_binding(&self) -> Option<&str> {
        match self {
            Decl::Action {
                decl: Binding { name, .. },
                ..
            } => Some(name),
            Decl::AfterAction { .. } => None,
            Decl::Alias {
                decl: Binding { name, .. },
                ..
            } => Some(name),
            Decl::Attribute { .. } => None,
            Decl::Axiom { .. } => None,
            Decl::BeforeAction { .. } => None,
            Decl::Common { .. } => None,
            Decl::Export {
                decl: ExportDecl::Action(Binding { name, .. }),
                ..
            } => Some(name),
            Decl::Export {
                decl: ExportDecl::ForwardRef(name),
                ..
            } => Some(name),
            Decl::Function {
                decl: Binding { name, .. },
                ..
            } => Some(name),
            Decl::Globals(_) => None,
            Decl::Implement { .. } => None,
            Decl::Import { decl, .. } => Some(&decl.name),
            Decl::Include { .. } => None,
            Decl::Instance { decl, .. } => Some(&decl.name),
            Decl::Instantiate { .. } => None,
            Decl::Interpret { .. } => None,
            Decl::Invariant { .. } => None,
            Decl::Module {
                decl: Binding { name, .. },
                ..
            } => Some(name),
            Decl::Noop => None,
            Decl::Object {
                decl: Binding { name, .. },
                ..
            } => Some(name),
            Decl::Relation {
                decl: Binding { name, .. },
                ..
            } => Some(name),
            Decl::Stmts(_) => None,
            Decl::Var {
                decl: Binding { name, .. },
                ..
            } => Some(name),
            Decl::Type {
                decl: Binding { name, .. },
                ..
            } => Some(name),
        }
    }

    pub fn as_action(self) -> Option<Binding<ActionDecl>> {
        match self {
            Decl::Action { decl, .. } => Some(decl),
            _ => None,
        }
    }

    pub fn span(&self) -> &Span {
        match self {
            Decl::Action { span, .. } => span,
            Decl::AfterAction { span, .. } => span,
            Decl::Alias { span, .. } => span,
            Decl::Attribute { span, .. } => span,
            Decl::Axiom { span, .. } => span,
            Decl::BeforeAction { span, .. } => span,
            Decl::Common { span, .. } => span,
            Decl::Export { span, .. } => span,
            Decl::Function { span, .. } => span,
            Decl::Globals(_) => DEFAULT_SPAN,
            Decl::Implement { span, .. } => span,
            Decl::Import { span, .. } => span,
            Decl::Include { span, .. } => span,
            Decl::Instance { span, .. } => span,
            Decl::Instantiate { .. } => DEFAULT_SPAN,
            Decl::Interpret { span, .. } => span,
            Decl::Invariant { span, .. } => span,
            Decl::Module { span, .. } => span,
            Decl::Noop => DEFAULT_SPAN,
            Decl::Object { span, .. } => span,
            Decl::Relation { span, .. } => span,
            Decl::Stmts(_) => DEFAULT_SPAN,
            Decl::Var { span, .. } => span,
            Decl::Type { span, .. } => span,
        }
    }
}

const DEFAULT_SPAN: &Span = &Span::IgnoredForTesting;
