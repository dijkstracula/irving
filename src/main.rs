use std::path::Path;

use clap::Parser;
use irving::visitor::visitor::Visitable;
use irving::{
    cli::{Cli, Commands},
    passes::pprint::PrettyPrinter,
};
use std::io::{self, Write};

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let ivy_file = cli.read_ivy_file()?;

    let mut prog = irving::parser::prog_from_str(&ivy_file)?;

    match cli.cmd {
        Commands::PPrint => {
            let mut pp = PrettyPrinter::<String>::new();
            prog.visit(&mut pp)?;

            let mut stdout = std::io::stdout().lock();
            stdout.write(pp.out.as_bytes())?;
        }
    }
    Ok(())
}
