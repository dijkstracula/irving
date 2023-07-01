#![allow(dead_code)]
#![allow(unused_variables)]

use anyhow::Result;

use crate::ast::{declarations::*, toplevels::Prog};

use crate::visitor::visitor::Visitable;
use crate::visitor::*;

pub struct GlobalLowerer {
    pub globals: Vec<Decl>,
}

impl GlobalLowerer {
    fn new() -> Self {
        GlobalLowerer { globals: vec![] }
    }

    pub fn visit(prog: &mut Prog) -> Result<()> {
        let mut g = Self::new();
        prog.visit(&mut g)?;
        prog.top.body.push(Decl::Globals(g.globals));
        Ok(())
    }
}

impl Visitor<()> for GlobalLowerer {
    fn finish_global_decl(&mut self, ast: &mut Vec<Decl>) -> VisitorResult<(), Decl> {
        self.globals.append(ast);
        Ok(ControlMut::Mutation(Decl::Globals(vec![]), ()))
    }
}
