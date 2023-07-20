use clap::Parser;
use irving::cli::{Cli, Commands, ExtractTarget};
use irving::extraction;
use irving::passes::global_lowerer::GlobalLowerer;
use irving::visitor::ast::Visitable;

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let cli = Cli::parse();
    let ivy_file = cli.read_ivy_file()?;

    let mut prog = irving::parser::prog_from_str(&ivy_file)?;

    // TODO: Might be good to wrap these all up in one meta-pass.
    let mut gl = GlobalLowerer::new();
    log::info!(target: "pass", "lowering globals");
    prog.visit(&mut gl)?.modifying(&mut prog)?;
    let mut tc = irving::stdlib::load_stdlib()?;

    log::info!("[pass] typechecking");
    prog.visit(&mut tc)?.modifying(&mut prog)?;

    match cli.cmd {
        Commands::Extract(ExtractTarget::Ivy) => {
            let mut e = extraction::ivy::Extractor::<String>::new();
            prog.visit(&mut e)?;
            println!("{}", e.pp.out);
        }
        Commands::Extract(ExtractTarget::Java) => {
            let mut e = extraction::java::extraction::Extractor::<String>::new();
            prog.visit(&mut e)?;
            println!("{}", e.pp.out);
        }
    }
    Ok(())
}
