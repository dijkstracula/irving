#![allow(dead_code)]

/* Structures that we need to extract */

use std::{collections::HashMap};

use crate::ast::declarations::*;
use crate::ast::expressions::*;
use crate::ast::statements::Stmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleError {
    DuplicateDecl(Decl),
    MixinMismatch,
    MissingSubmodule(String)
}

type Result<T> = std::result::Result<T, ModuleError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mixin {
    pub name: Symbol,
    pub params: Option<Vec<Param>>,
    pub ret: Option<Param>,

    pub pre: Option<Vec<Stmt>>,
    pub body: Option<Vec<Stmt>>,
    pub post: Option<Vec<Stmt>>,
}

impl Mixin {
    pub fn new(name: String) -> Self {
        Mixin {
            name: name,
            params: None,
            ret: None,

            pre: None,
            body: None,
            post: None,
        }
    }

    pub fn from_decl(decl: Decl) -> Self {
        match decl {
            Decl::Action(actiondecl) => Self::from_action(actiondecl),
            Decl::BeforeAction(pre) => Self::from_before(pre),
            Decl::AfterAction(post) => Self::from_after(post),
            _ => unreachable!()
        }
    }

    pub fn from_action(action: ActionDecl) -> Self {
        Mixin {
            name: action.name.last().unwrap().to_owned(),
            params: Some(action.params),
            ret: action.ret,

            pre: None,
            body: action.body,
            post: None,
        }
    }

    pub fn from_after(action: AfterDecl) -> Self {
        Mixin {
            name: action.name.last().unwrap().to_owned(),
            params: action.params,
            ret: action.ret,

            pre: None,
            body: None,
            post: Some(action.body),
        }
    }

    pub fn from_before(action: BeforeDecl) -> Self {
        Mixin {
            name: action.name.last().unwrap().to_owned(),
            params: action.params,
            ret: None,

            pre: Some(action.body),
            body: None,
            post: None,
        }
    }

    pub fn check_name(&self, n: &str) -> Result<()> {
        if self.name == n {
            Ok(())
        } else {
            Err(ModuleError::MixinMismatch)
        }
    }

    pub fn mix_action(&mut self, action: ActionDecl) -> Result<()> {
        self.params = Self::mix(std::mem::take(&mut self.params), Some(action.params))?;
        self.ret    = Self::mix(std::mem::take(&mut self.ret), action.ret)?;
        self.body   = Self::mix(std::mem::take(&mut self.body), action.body)?;
        Ok(())
    }

    pub fn mix_after(&mut self, action: AfterDecl) -> Result<()> {
        self.params = Self::mix(std::mem::take(&mut self.params), action.params)?;
        self.ret    = Self::mix(std::mem::take(&mut self.ret), action.ret)?;
        self.post   = Self::mix(std::mem::take(&mut self.post), Some(action.body))?;
        Ok(())
    }

    pub fn mix_before(&mut self, action: BeforeDecl) -> Result<()> {
        self.params = Self::mix(std::mem::take(&mut self.params), action.params)?;
        self.pre    = Self::mix(std::mem::take(&mut self.pre), Some(action.body))?;
        Ok(())
    }

    fn mix<'a, T>(o1: Option<T>, o2: Option<T>) -> Result<Option<T>> 
    where
        T: Clone + Eq,
    {
        match (o1, o2) {
            (None, None)     => Ok(None),
            (Some(t1), None) => Ok(Some(t1)),
            (None, Some(t2)) => Ok(Some(t2)),
            (Some(t1), Some(t2)) if t1 == t2 => Ok(Some(t2)),
            _ => Err(ModuleError::MixinMismatch)
        } 
    }

}

/// A module in the sense of the top level ivy_module.Module object.
/// It feels like this is not the same thing as a `module` declaration
/// so we should come up with a different name here.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    name: String,
    decls: Vec<Decl>,
    actions: HashMap<String, Mixin>,

    submodules: HashMap<String, Module>,
}

impl Module {
    pub fn new(name: String) -> Self {
        Module {
            name: name,
            decls: Vec::new(),
            actions: HashMap::new(),
            submodules: HashMap::new(),
        }
    } 


/* 
    pub fn handle_action_decl(&mut self, action: ActionDecl) -> Result<()> {
        match action.name.as_slice() {
            [] => panic!("Malformed AST: mod={:?}, act={:?}", self, action),
            [name] => {
                match self.actions.get_mut(name) {
                    None => { self.actions.insert(name.to_owned(), Mixin::from_action(action)); },
                    Some(mixin) => { mixin.mix_action(action)?; },
                }
                Ok(()) // TODO
            }
            // If the action name is qualified, then we'll need to hand it off 
            // to the appropriate submodule.
            [qualifier, ..] => {
                match self.submodules.get_mut(qualifier) {
                    None      => Err(ModuleError::MissingSubmodule(qualifier.to_owned())),
                    Some(sub) => sub.handle_action_decl(action),
                }
            }
        }
    }

    pub fn handle_decl(&mut self, decl: Decl) -> Result<()> {
        match decl {
            Decl::Action(decl) => self.handle_action_decl(decl),
            _ => unimplemented!()
        }
    }

    pub fn handle_prog(&mut self, prog: Prog) -> Result<()> {
        prog.tops.into_iter()
            .map(|decl| self.handle_decl(decl))
            .collect::<Result<_>>()
    }
    */

}