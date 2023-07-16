use crate::{
    ast::declarations::Decl,
    parser::ivy::{IvyParser, Rule},
    passes::{global_lowerer::GlobalLowerer, isolate_normalizer::IsolateNormalizer},
    typechecker::inference::TypeChecker,
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
fn decl_from_filename(path: &str) -> Result<Decl> {
    let text = std::fs::read_to_string(path).unwrap();
    let res = IvyParser::parse(Rule::decl, &text)?.single().unwrap();
    let mut prog = IvyParser::decl(res)?;

    let mut gl = GlobalLowerer::new();
    prog.visit(&mut gl)?.modifying(&mut prog)?;
    let mut nm = IsolateNormalizer::new();
    prog.visit(&mut nm)?.modifying(&mut prog)?;
    Ok(prog)
}

pub fn load_stdlib() -> Result<TypeChecker> {
    let mut tc = TypeChecker::new();

    let mut decl = decl_from_filename("src/stdlib/ivy/network.ivy")?;
    decl.visit(&mut tc)?.modifying(&mut decl)?;

    let mut decl = decl_from_filename("src/stdlib/ivy/collections.ivy")?;
    decl.visit(&mut tc)?.modifying(&mut decl)?;

    Ok(tc)
}
