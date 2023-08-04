use std::{error::Error, fmt::Display};

use crate::{parser::ivy::ParseError, typechecker::TypeError};

#[derive(Debug)]
pub enum IrvingError {
    Cli(clap::Error),
    Parse(ParseError),
    IO(std::io::Error),
    Typecheck(TypeError),
    Extraction(std::fmt::Error),
}

impl Error for IrvingError {}

impl Display for IrvingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrvingError::Cli(e) => f.write_fmt(format_args!("Command-line parse error: {}", e)),
            IrvingError::Parse(e) => f.write_fmt(format_args!("Program error: {}", e)),
            IrvingError::IO(e) => f.write_fmt(format_args!("IO error: {:?}", e)),
            IrvingError::Typecheck(e) => f.write_fmt(format_args!("Typechecking error: {:?}", e)),
            IrvingError::Extraction(e) => f.write_fmt(format_args!("Extraction error: {}", e)),
        }
    }
}

impl From<clap::Error> for IrvingError {
    fn from(value: clap::Error) -> Self {
        Self::Cli(value)
    }
}

impl From<ParseError> for IrvingError {
    fn from(value: ParseError) -> Self {
        Self::Parse(value)
    }
}

impl From<std::io::Error> for IrvingError {
    fn from(value: std::io::Error) -> Self {
        Self::IO(value)
    }
}

impl From<TypeError> for IrvingError {
    fn from(value: TypeError) -> Self {
        Self::Typecheck(value)
    }
}

impl From<std::fmt::Error> for IrvingError {
    fn from(value: std::fmt::Error) -> Self {
        Self::Extraction(value)
    }
}
