use clap::Parser;
use env_logger::Env;
use irving::cli::{Cli, Commands, ExtractTarget};
use irving::error::IrvingError;
use irving::extraction;
use irving::passes::global_lowerer::GlobalLowerer;
use irving::visitor::ast::Visitable;

fn main() {
    // This is dreadful!  But, I want to explicitly emit the IvyError's Display,
    // not Debug.  I also can't use anyhow::Result<T> because the Rc that backs
    // the program text is of course not Sync.  Maybe I'll come up with a better
    // solution someday, or maybe not.
    match main_impl() {
        Ok(()) => (),
        Err(e) => eprintln!("{}", e),
    }
}

fn main_impl() -> std::result::Result<(), IrvingError> {
    env_logger::Builder::from_env(Env::default().default_filter_or("info")).init();

    let cli = Cli::parse();
    let ivy_file = cli.read_ivy_file()?;

    let mut prog = irving::parser::prog_from_str(&ivy_file)?;

    // TODO: Might be good to wrap these all up in one meta-pass.
    let mut gl = GlobalLowerer::new();
    log::info!(target: "pass", "lowering globals");
    prog.visit(&mut gl)?.modifying(&mut prog);
    irving::stdlib::typecheck(&mut prog)?;

    match cli.cmd {
        Commands::Extract(ExtractTarget::Ivy) => {
            let mut e = extraction::ivy::Extractor::<String>::new();
            log::info!(target: "cli", "Extracting Ivy");
            prog.visit(&mut e)?;
            println!("{}", e.pp.out);
        }
        Commands::Extract(ExtractTarget::Java) => {
            let mut e = extraction::java::extraction::Extractor::<String>::new();
            log::info!(target: "cli", "Extracting Java");
            prog.visit(&mut e)?;
            println!("{}", e.pp.out);
        }
        Commands::Extract(ExtractTarget::Vmt) => {
            let mut e = extraction::vmt::Extractor::<String>::new();
            log::info!(target: "cli", "Extracting VMT");
            prog.visit(&mut e)?;
            println!("{}", e.pp.out);
        }
    }
    Ok(())
}
