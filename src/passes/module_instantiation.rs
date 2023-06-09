use std::collections::HashMap;

use anyhow::bail;
use thiserror::Error;

use crate::{
    ast::expressions::{Ident, Symbol},
    visitor::*,
};

/// Walks a module definition, replacing ununified SortVars with a
/// concrete IvySort.
pub struct ModuleInstantiation {
    mapping: HashMap<Ident, Ident>,
}

impl ModuleInstantiation {
    #[allow(dead_code)]
    pub fn new(mapping: HashMap<Symbol, Ident>) -> Self {
        let mapping: HashMap<Ident, Ident> = mapping
            .into_iter()
            .map(|(k, v)| (vec![k], v))
            .collect::<_>();

        Self { mapping: mapping }
    }
}

impl Visitor<()> for ModuleInstantiation {
    fn identifier(&mut self, i: &mut Ident) -> VisitorResult<(), Ident> {
        match self.mapping.get(i) {
            None => Ok(ControlMut::Produce(())),
            Some(s2) => Ok(ControlMut::Mutation(s2.clone(), ())),
        }
    }

    fn begin_alias_decl(
        &mut self,
        sym: &mut Symbol,
        _e: &mut crate::ast::expressions::Expr,
    ) -> VisitorResult<(), crate::ast::declarations::Decl> {
        match self.mapping.get(&vec![sym.clone()]) {
            None => Ok(ControlMut::Produce(())),
            Some(_) => bail!(ModuleInstantiationError::ModuleArgumentRebinding(
                sym.clone()
            )),
        }
    }

    fn finish_module_decl(
        &mut self,
        _node: &mut Symbol,
        ast: &mut crate::ast::declarations::ModuleDecl,
        _n: (),
        _p: Vec<()>,
        _b: Vec<()>,
    ) -> VisitorResult<(), crate::ast::declarations::Decl> {
        ast.sortsyms = vec![];
        Ok(ControlMut::Produce(()))
    }
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum ModuleInstantiationError {
    #[error("Symbol {0:?} is a free variable in the module argument list")]
    ModuleArgumentRebinding(Symbol),
}
