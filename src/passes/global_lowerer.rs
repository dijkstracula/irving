#![allow(dead_code)]
#![allow(unused_variables)]

use crate::ast::{declarations::*, toplevels::Prog};
use crate::visitor::ast::Visitor;
use crate::visitor::*;

/// A compiler pass that finds global declarations at any scope level and moves them
/// to the top level of the program.
pub struct GlobalLowerer {
    pub globals: Vec<Decl>,
}

impl GlobalLowerer {
    pub fn new() -> Self {
        GlobalLowerer { globals: vec![] }
    }
}

impl Visitor<(), std::fmt::Error> for GlobalLowerer {
    fn finish_prog(&mut self, prog: &mut Prog) -> VisitorResult<(), std::fmt::Error, Prog> {
        self.globals.append(&mut prog.top.body);
        prog.top.body.append(&mut self.globals);
        Ok(ControlMut::Produce(()))
    }

    fn finish_global_decl(
        &mut self,
        ast: &mut Vec<Decl>,
    ) -> VisitorResult<(), std::fmt::Error, Decl> {
        self.globals.append(ast);
        Ok(ControlMut::Mutation(Decl::Noop, ()))
    }
}
