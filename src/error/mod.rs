use std::{error::Error, fmt::Display};

use crate::{parser::ivy::ParseError, typechecker::TypeError};

#[derive(Debug)]
pub enum IvyError {
    Cli(clap::Error),
    Parse(ParseError),
    IO(std::io::Error),
    Typecheck(TypeError),
    Extraction(std::fmt::Error),
}

impl Error for IvyError {}

impl Display for IvyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IvyError::Cli(e) => f.write_fmt(format_args!("Command-line parse error: {}", e)),
            IvyError::Parse(e) => f.write_fmt(format_args!("Program error: {}", e)),
            IvyError::IO(e) => f.write_fmt(format_args!("IO error: {:?}", e)),
            IvyError::Typecheck(e) => f.write_fmt(format_args!("Typechecking error: {:?}", e)),
            IvyError::Extraction(e) => f.write_fmt(format_args!("Extraction error: {}", e)),
        }
    }
}

impl From<clap::Error> for IvyError {
    fn from(value: clap::Error) -> Self {
        Self::Cli(value)
    }
}

impl From<ParseError> for IvyError {
    fn from(value: ParseError) -> Self {
        Self::Parse(value)
    }
}

impl From<std::io::Error> for IvyError {
    fn from(value: std::io::Error) -> Self {
        Self::IO(value)
    }
}

impl From<TypeError> for IvyError {
    fn from(value: TypeError) -> Self {
        Self::Typecheck(value)
    }
}

impl From<std::fmt::Error> for IvyError {
    fn from(value: std::fmt::Error) -> Self {
        Self::Extraction(value)
    }
}
