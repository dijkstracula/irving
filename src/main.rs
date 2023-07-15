use clap::Parser;
use irving::cli::{Cli, Commands};
use irving::extraction::ivy::Extractor;
use irving::visitor::ast::Visitable;
use std::io::Write;

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let ivy_file = cli.read_ivy_file()?;

    let mut prog = irving::parser::prog_from_str(&ivy_file)?;

    match cli.cmd {
        Commands::PPrint => {
            let mut e = Extractor::<String>::new();
            prog.visit(&mut e)?;

            let mut stdout = std::io::stdout().lock();
            stdout.write(e.pp.out.as_bytes())?;
        }
    }
    Ok(())
}
