use std::collections::BTreeMap;

use anyhow::{bail, Result};

use thiserror::Error;

use crate::{
    ast::expressions::Symbol,
    typechecker::sorts::{IvySort, Module, SortSubstituter},
    visitor::sort::Visitable,
};

/// Walks a ModuleSort definition, replacing ununified SortVars with a
/// concrete IvySort.
pub fn instantiate(mut m: Module, args: Vec<IvySort>) -> Result<IvySort> {
    let curried = m.args.drain(0..args.len());
    let substs = curried
        .zip(args.into_iter())
        .map(|((name, s1), s2)| {
            let IvySort::SortVar(_) = s1 else {
                bail!(ModuleInstantiationError::ModuleArgumentRebinding(name.clone(), s1));
            };
            Ok((s1, s2))
        })
        .collect::<Result<BTreeMap<_, _>>>()?;

    let mut ss = SortSubstituter::new(substs);

    let mut as_ivysort = IvySort::Module(m);

    Ok(as_ivysort.visit(&mut ss)?.modifying(&mut as_ivysort)?)
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum ModuleInstantiationError {
    #[error("Symbol {0:?} is already bound to {1:?}, which is not a free sort variable")]
    ModuleArgumentRebinding(Symbol, IvySort),
}
