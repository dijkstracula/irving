#![allow(dead_code)]
#![allow(unused_variables)]

use std::fmt::{Error};

use crate::ast::declarations::*;
use crate::ast::toplevels::Prog;
use crate::visitor::visitor::Visitor;

use super::control::Control::Remove;
use super::control::VisitorResult;

pub struct GlobalLowerer {
    pub globals: Vec<Decl>
}

impl GlobalLowerer {
    fn new() -> Self {
        GlobalLowerer { globals: vec!() }
    }

    pub fn visit(prog: &mut Prog) {
        let mut g = Self::new();
        g.visit_prog(prog).unwrap();
        prog.top.body.push(Decl::Globals(g.globals));
    }
}

impl Visitor<Error> for GlobalLowerer {
    fn visit_globals(&mut self, defs: &mut Vec<Decl>) -> VisitorResult<Error> {
        self.globals.append(defs);
        Ok(Remove)
    }
}