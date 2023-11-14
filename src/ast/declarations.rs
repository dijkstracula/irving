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
pub struct ClassDecl {
    pub parent: Option<Token>,

    pub fields: Vec<Binding<Sort>>,
    pub actions: Vec<Binding<ActionDecl>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassSlot {
    Field(Binding<Sort>),
    Action(Binding<ActionDecl>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDecl {
    pub params: ParamList,
    pub ret: Option<Ident>,
    pub body: Option<Fmla>,
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
pub struct IncludeDecl {
    pub span: Span,
    pub name: Token,
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

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct MapDecl {
    pub domain: ParamList,
    pub range: Sort,
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Binding<T> {
    pub name: String,
    pub decl: T,
    pub span: Span,
}

impl<T> Binding<T> {
    pub fn from<S>(name: S, decl: T, span: Span) -> Self
    where
        S: Into<String>,
    {
        Self {
            name: name.into(),
            decl,
            span,
        }
    }
}

#[cfg(test)]
impl Binding<Sort> {
    pub fn inferred<S>(name: S) -> Self
    where
        S: Into<String>,
    {
        Self::from(name, Sort::ToBeInferred, Span::IgnoredForTesting)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub enum Decl {
    Action {
        decl: Binding<ActionDecl>,
    },

    AfterAction {
        span: Span,
        decl: ActionMixinDecl,
    },

    Alias {
        decl: Binding<Sort>,
    },

    Attribute {
        span: Span,
        lhs: Expr,
        rhs: Option<Expr>,
    },

    Axiom {
        span: Span,
        decl: Fmla,
    },

    BeforeAction {
        span: Span,
        decl: ActionMixinDecl,
    },

    Class {
        decl: Binding<ClassDecl>,
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

    Instance {
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
        decl: Binding<Fmla>,
    },

    Map {
        decl: Binding<MapDecl>,
    },

    Module {
        decl: Binding<ModuleDecl>,
    },

    Noop,

    Object {
        decl: Binding<ObjectDecl>,
    },

    Stmts(Vec<Stmt>),

    Subclass {
        decl: Binding<ClassDecl>,
    },

    Var {
        decl: Binding<Sort>,
    },

    Type {
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
            Decl::Class {
                decl: Binding { name, .. },
                ..
            } => Some(name),
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
            Decl::Instance { decl, .. } => Some(&decl.name),
            Decl::Instantiate { .. } => None,
            Decl::Interpret { decl, .. } => Some(&decl.name),
            Decl::Invariant { .. } => None,
            Decl::Map {
                decl: Binding { name, .. },
                ..
            } => Some(name),
            Decl::Module {
                decl: Binding { name, .. },
                ..
            } => Some(name),
            Decl::Noop => None,
            Decl::Object {
                decl: Binding { name, .. },
                ..
            } => Some(name),
            Decl::Stmts(_) => None,
            Decl::Subclass {
                decl: Binding { name, .. },
                ..
            } => Some(name),
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
            Decl::Action {
                decl: Binding { span, .. },
                ..
            } => span,
            Decl::AfterAction { span, .. } => span,
            Decl::Alias {
                decl: Binding { span, .. },
            } => span,
            Decl::Attribute { span, .. } => span,
            Decl::Axiom { span, .. } => span,
            Decl::BeforeAction { span, .. } => span,
            Decl::Common { span, .. } => span,
            Decl::Class {
                decl: Binding { span, .. },
            } => span,
            Decl::Export { span, .. } => span,
            Decl::Function {
                decl: Binding { span, .. },
            } => span,
            Decl::Globals(_) => DEFAULT_SPAN,
            Decl::Implement { span, .. } => span,
            Decl::Import { span, .. } => span,
            Decl::Instance {
                decl: Binding { span, .. },
            } => span,
            Decl::Instantiate { .. } => DEFAULT_SPAN,
            Decl::Interpret { span, .. } => span,
            Decl::Invariant {
                decl: Binding { span, .. },
            } => span,
            Decl::Map {
                decl: Binding { span, .. },
            } => span,
            Decl::Module {
                decl: Binding { span, .. },
            } => span,
            Decl::Noop => DEFAULT_SPAN,
            Decl::Object {
                decl: Binding { span, .. },
            } => span,
            Decl::Stmts(_) => DEFAULT_SPAN,
            Decl::Subclass {
                decl: Binding { span, .. },
            } => span,
            Decl::Var {
                decl: Binding { span, .. },
            } => span,
            Decl::Type {
                decl: Binding { span, .. },
            } => span,
        }
    }
}

const DEFAULT_SPAN: &Span = &Span::IgnoredForTesting;
