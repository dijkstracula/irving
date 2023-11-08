pub mod constant_fold;
pub mod global_lowerer;
pub mod include_expander;
pub mod module_instantiation;
pub mod quantifier_bounds;

use std::{env, path::PathBuf};

use crate::{
    ast::toplevels::Prog,
    error::IrvingError,
    passes::{global_lowerer::GlobalLowerer, include_expander::IncludeExpander},
    stdlib,
    visitor::ast::Visitable,
};

pub fn all_passes(cwd: PathBuf, prog: &mut Prog) -> Result<(), IrvingError> {
    log::info!(target: "pass", "lowering globals");
    let mut gl = GlobalLowerer::new();
    prog.visit(&mut gl)?.modifying(prog);

    log::info!(target: "pass", "expanding includes");
    let mut ie = IncludeExpander::new(&cwd);
    prog.visit(&mut ie)?.modifying(prog);
    println!("{:?}", prog);

    log::info!(target: "pass", "typechecking");
    stdlib::typecheck(prog)?;

    Ok(())
}
