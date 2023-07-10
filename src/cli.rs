use clap::{CommandFactory, Parser, Subcommand};

#[derive(Parser)]
#[command(name = "Irving")]
#[command(author = "Nathan Taylor <ntaylor@cs.utexas.edu>")]
#[command(version, long_about = None)]
pub struct Cli {
    /// Path to the input Ivy file
    #[arg(value_hint = clap::ValueHint::FilePath)]
    ivy_file: std::path::PathBuf,

    /// What do?
    #[command(subcommand)]
    pub cmd: Commands,
}

impl Cli {
    pub fn read_ivy_file(&self) -> Result<String, clap::Error> {
        std::fs::read_to_string(&self.ivy_file).map_err(|e| {
            let mut cmd = Cli::command();
            let path = self.ivy_file.as_os_str();
            cmd.error(
                clap::error::ErrorKind::InvalidValue,
                format!("Error reading {:?}: {}", path, e),
            )
        })
    }
}

#[derive(Subcommand)]
pub enum Commands {
    /// Pretty-prints the input file
    /// TODO: this will become Extract with a subcommand
    /// for each language.
    PPrint,
}
