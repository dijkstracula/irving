use std::collections::BTreeMap;

use crate::{
    typechecker::{
        sorts::{IvySort, Module, SortSubstituter},
        TypeError,
    },
    visitor::sort::Visitable,
};

/// Walks a ModuleSort definition, replacing ununified SortVars with a
/// concrete IvySort.
pub fn instantiate(mut m: Module, args: Vec<IvySort>) -> Result<IvySort, TypeError> {
    let curried = m.args.drain(0..args.len());
    let substs = curried
        .zip(args.into_iter())
        .map(|(binding, s2)| {
            //let unified = ctx.unify(&s1, &s2)?;
            (binding.decl, s2)
        })
        .collect::<BTreeMap<_, _>>();

    let mut ss = SortSubstituter::new(substs);

    let mut as_ivysort = IvySort::Module(m);

    Ok(as_ivysort.visit(&mut ss)?.modifying(&mut as_ivysort))
}
