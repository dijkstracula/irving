use crate::{
    ast::toplevels::{self, Prog},
    parser::ivy::{IvyParser, Rule},
    passes::global_lowerer::GlobalLowerer,
    typechecker::{inference::SortInferer, subst::SortSubstituter},
    visitor::ast::Visitable,
};
use anyhow::Result;
use pest_consume::Parser;

// TODO: This should really be a prog_from_decl, but we have an irritating issue
// that's popped up in a few different places: `Prog` contains a top level
// Ioslate called, well, 'top' - and, typechecking multiple files means we will
// have, morally, multiple different 'top' isolates.  Either the visitor needs
// to go (back?) to just treating the top isolate as a decl block, or, the
// typechecker whilst visiting needs to just treat the top isolate as special.
//
// See https://github.com/dijkstracula/irving/issues/5 .
fn prog_from_filename(path: &str) -> Result<Prog> {
    log::info!(target: "stdlib", "loading {path}");

    let text = std::fs::read_to_string(path).unwrap();
    let res = IvyParser::parse(Rule::prog, &text)?.single().unwrap();
    let mut prog = IvyParser::prog(res)?;

    let mut gl = GlobalLowerer::new();
    prog.visit(&mut gl)?.modifying(&mut prog)?;
    Ok(prog)
}

pub fn load_stdlib() -> Result<SortInferer> {
    let mut tc = SortInferer::new();

    let mut decl = prog_from_filename("src/stdlib/ivy/network.ivy")?;
    decl.visit(&mut tc)?.modifying(&mut decl)?;

    let mut decl = prog_from_filename("src/stdlib/ivy/collections.ivy")?;
    decl.visit(&mut tc)?.modifying(&mut decl)?;

    Ok(tc)
}

// XXX: Not a great place for this to live.
pub fn typecheck(prog: &mut toplevels::Prog) -> anyhow::Result<()> {
    let mut inferer = load_stdlib().unwrap();
    prog.visit(&mut inferer)?.modifying(prog)?;

    println!("{:?}", inferer.bindings.ctx.get(14));
    let mut subst = SortSubstituter::from_inferer(inferer);
    prog.visit(&mut subst)?.modifying(prog)?;

    Ok(())
}
