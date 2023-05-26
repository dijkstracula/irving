#![allow(dead_code)]
#![allow(unused_variables)]

use std::fmt::{Error};

use crate::ast::declarations::*;
use crate::ast::toplevels::Prog;
use crate::visitor::visitor::Visitor;

use super::control::Control::Remove;
use super::control::VisitorResult;

pub struct GlobalLowerer {
    pub prog: Prog
}

impl Visitor<Error> for GlobalLowerer {
    fn visit_globals(&mut self, defs: &mut Vec<Decl>) -> VisitorResult<Error> {
        self.prog.top.body.append(defs);
        Ok(Remove)
    }
}