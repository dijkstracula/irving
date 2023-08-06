use crate::{
    ast::toplevels::{self, Prog},
    error::IrvingError,
    passes::global_lowerer::GlobalLowerer,
    typechecker::{inference::SortInferer, subst::SortSubstituter, TypeError},
    visitor::ast::Visitable,
};

// TODO: This should really be a prog_from_decl, but we have an irritating issue
// that's popped up in a few different places: `Prog` contains a top level
// Ioslate called, well, 'top' - and, typechecking multiple files means we will
// have, morally, multiple different 'top' isolates.  Either the visitor needs
// to go (back?) to just treating the top isolate as a decl block, or, the
// typechecker whilst visiting needs to just treat the top isolate as special.
//
// See https://github.com/dijkstracula/irving/issues/5 .
fn prog_from_filename(path: &str) -> std::result::Result<Prog, IrvingError> {
    log::info!(target: "stdlib", "loading {path}");

    let text = std::fs::read_to_string(path).unwrap();
    let mut prog = crate::parser::prog_from_str(&text)?;

    let mut gl = GlobalLowerer::new();
    prog.visit(&mut gl)?.modifying(&mut prog);
    Ok(prog)
}

pub fn load_stdlib() -> Result<SortInferer, IrvingError> {
    let mut tc = SortInferer::new();

    let mut decl = prog_from_filename("src/stdlib/ivy/network.ivy")?;
    decl.visit(&mut tc)?.modifying(&mut decl);

    let mut decl = prog_from_filename("src/stdlib/ivy/collections.ivy")?;
    decl.visit(&mut tc)?.modifying(&mut decl);

    Ok(tc)
}

// XXX: Not a great place for this to live.
pub fn typecheck(prog: &mut toplevels::Prog) -> Result<(), TypeError> {
    let mut inferer = load_stdlib().unwrap();
    prog.visit(&mut inferer)?.modifying(prog);

    println!("{:?}", inferer.bindings.ctx.get(14));
    let mut subst = SortSubstituter::from_inferer(inferer);
    prog.visit(&mut subst)?.modifying(prog);

    Ok(())
}
