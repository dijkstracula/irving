use clap::Parser;
use irving::cli::{Cli, Commands, ExtractTarget};
use irving::extraction;
use irving::passes::global_lowerer::GlobalLowerer;
use irving::visitor::ast::Visitable;
use std::io::Write;

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let ivy_file = cli.read_ivy_file()?;

    let mut prog = irving::parser::prog_from_str(&ivy_file)?;

    // TODO: Might be good to wrap these all up in one meta-pass.
    let mut gl = GlobalLowerer::new();
    prog.visit(&mut gl)?.modifying(&mut prog)?;
    let mut tc = irving::stdlib::load_stdlib()?;
    prog.visit(&mut tc)?.modifying(&mut prog)?;

    match cli.cmd {
        Commands::Extract(ExtractTarget::Ivy) => {
            let mut e = extraction::ivy::Extractor::<String>::new();
            prog.visit(&mut e)?;

            let mut stdout = std::io::stdout().lock();
            stdout.write(e.pp.out.as_bytes())?;
        }
        Commands::Extract(ExtractTarget::Java) => {
            let mut e = extraction::java::extraction::Extractor::<String>::new();
            prog.visit(&mut e)?;

            let mut stdout = std::io::stdout().lock();
            stdout.write(e.pp.out.as_bytes())?;
        }
    }
    Ok(())
}
