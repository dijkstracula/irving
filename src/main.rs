use clap::Parser;
use irving::visitor::ast::Visitable;
use irving::cli::{Cli, Commands, ExtractTarget};
use irving::extraction;
use std::io::Write;

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let ivy_file = cli.read_ivy_file()?;

    let mut prog = irving::parser::prog_from_str(&ivy_file)?;

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
