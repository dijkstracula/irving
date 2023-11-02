use std::{
    io::ErrorKind,
    path::{Path, PathBuf},
};

use crate::{
    ast::{expressions, toplevels::Prog},
    error::IrvingError,
    visitor::{ast::Visitor, ControlMut, VisitorResult},
};

const STDLIB_IMPORT_DIR: &'static str = "src/stdlib/ivy";

/// A compiler pass that expands out include directives
pub struct IncludeExpander {
    cwd: PathBuf,
    done: Vec<Prog>,
}

impl IncludeExpander {
    pub fn new<P>(cwd: P) -> Self
    where
        P: AsRef<Path>,
    {
        Self {
            cwd: cwd.as_ref().to_owned(),
            done: vec![],
        }
    }

    fn handle_include(&mut self, file: &mut expressions::Token) -> Result<Prog, IrvingError> {
        let file = file.to_owned() + ".ivy";
        let stdlib_path = PathBuf::from(STDLIB_IMPORT_DIR).join(&file);
        let cwd_path = PathBuf::from(&self.cwd).join(&file);

        let (path, text) = match std::fs::read_to_string(&stdlib_path) {
            Ok(text) => Ok((stdlib_path, text)),
            Err(e) if e.kind() == ErrorKind::NotFound => {
                std::fs::read_to_string(&cwd_path).map(|text| (cwd_path, text))
            }
            Err(e) => Err(e),
        }?;

        log::info!(target: "include_expander", "Including {}", path.display());
        Ok(crate::parser::prog_from_str(&text)?)
    }
}

impl Visitor<(), IrvingError> for IncludeExpander {
    fn begin_prog(&mut self, prog: &mut Prog) -> VisitorResult<(), IrvingError, Prog> {
        for inc in prog.includes.iter_mut() {
            let included = self.handle_include(&mut inc.name)?;
            self.done.push(included);
        }
        Ok(ControlMut::Produce(()))
    }

    fn finish_prog(&mut self, prog: &mut Prog) -> VisitorResult<(), IrvingError, Prog> {
        for included in &mut self.done {
            log::info!(target: "include_expander", "Merging {} declarations", included.top.body.len());
            let merged = std::mem::take(&mut included.top.body)
                .into_iter()
                .chain(std::mem::take(&mut prog.top.body).into_iter());
            prog.top.body = merged.collect();
        }
        Ok(ControlMut::Produce(()))
    }
}
