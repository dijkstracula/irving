use crate::{
    ast::toplevels::{self},
    typechecker::{inference::SortInferer, subst::SortSubstituter, TypeError},
    visitor::ast::Visitable,
};

// XXX: Not a great place for this to live.
pub fn typecheck(prog: &mut toplevels::Prog) -> Result<(), TypeError> {
    log::info!(target: "typechecking", "sort-inference");
    let mut inferer = SortInferer::new();
    prog.visit(&mut inferer)?.modifying(prog);

    log::info!(target: "typechecking", "sort-substitution");
    let mut subst = SortSubstituter::from_inferer(inferer);
    prog.visit(&mut subst)?.modifying(prog);
    Ok(())
}
