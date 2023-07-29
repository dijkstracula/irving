use std::collections::BTreeMap;

use anyhow::Result;

use thiserror::Error;

use crate::{
    ast::expressions::Token,
    typechecker::sorts::{IvySort, Module, SortSubstituter},
    visitor::sort::Visitable,
};

/// Walks a ModuleSort definition, replacing ununified SortVars with a
/// concrete IvySort.
pub fn instantiate(mut m: Module, args: Vec<IvySort>) -> Result<IvySort> {
    let curried = m.args.drain(0..args.len());
    let substs = curried
        .zip(args.into_iter())
        .map(|((_name, s1), s2)| {
            //let unified = ctx.unify(&s1, &s2)?;
            Ok((s1, s2))
        })
        .collect::<Result<BTreeMap<_, _>>>()?;

    let mut ss = SortSubstituter::new(substs);

    let mut as_ivysort = IvySort::Module(m);

    as_ivysort.visit(&mut ss)?.modifying(&mut as_ivysort)
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum ModuleInstantiationError {
    #[error("Symbol {0:?} is already bound to {1:?}, which is not a free sort variable")]
    ModuleArgumentRebinding(Token, IvySort),
}
