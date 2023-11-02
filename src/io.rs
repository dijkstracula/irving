use std::path::Path;

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

#[derive(Subcommand)]
pub enum Commands {
    #[command(subcommand)]
    Extract(ExtractTarget),
}

#[derive(Subcommand)]
pub enum ExtractTarget {
    Ivy,
    Java,
    Vmt,
}

impl Cli {
    pub fn read_ivy_file(&self) -> Result<String, clap::Error> {
        read_ivy_file(&self.ivy_file)
    }
}

pub fn read_ivy_file<P>(filename: P) -> Result<String, clap::Error>
where
    P: AsRef<Path>,
{
    std::fs::read_to_string(filename.as_ref()).map_err(|e| {
        let mut cmd = Cli::command();
        let path = filename.as_ref().as_os_str();
        cmd.error(
            clap::error::ErrorKind::InvalidValue,
            format!("Error reading {:?}: {}", path, e),
        )
    })
}
