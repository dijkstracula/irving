#![allow(dead_code)]
#![allow(unused_variables)]

use crate::ast::{declarations::*, toplevels::Prog};
use crate::visitor::*;

pub struct GlobalLowerer {
    pub globals: Vec<Decl>,
}

impl GlobalLowerer {
    pub fn new() -> Self {
        GlobalLowerer { globals: vec![] }
    }
}

impl ast::Visitor<()> for GlobalLowerer {
    fn finish_prog(&mut self, prog: &mut Prog) -> VisitorResult<(), Prog> {
        match &mut prog.top {
            Decl::Isolate(Binding {
                decl: IsolateDecl { body, .. },
                ..
            }) => {
                self.globals.append(body);
                body.append(&mut self.globals);
            }
            Decl::NormalizedIsolate(_) => {
                panic!("Lower globals prior to running the isolate normalizer.")
            }
            _ => unreachable!(),
        }
        Ok(ControlMut::Produce(()))
    }

    fn finish_global_decl(&mut self, ast: &mut Vec<Decl>) -> VisitorResult<(), Decl> {
        self.globals.append(ast);
        Ok(ControlMut::Mutation(Decl::Noop, ()))
    }
}
