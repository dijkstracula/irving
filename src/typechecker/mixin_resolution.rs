use crate::{
    ast::declarations::*,
    visitor::{control::VisitorResult, visitor::Visitor},
};

use super::{unifier::Resolver, Error};

pub struct MixinResolver {
    pub bindings: Resolver,
    //    curr_isolate: Option<&'a IsolateDecl>,
}

impl MixinResolver {
    pub fn new() -> Self {
        let mut s = Self {
            bindings: Resolver::new(),
            //           curr_isolate: None,
        };
        s.bindings.push_scope();
        s
    }
}

impl Visitor<(), Error> for MixinResolver {
    fn visit_isolate(&mut self, iso: &mut IsolateDecl) -> VisitorResult<(), Error> {
        //        let prev_isolate = self.curr_isolate;
        //self.curr_isolate = Some(iso);

        // recuse...

        //       self.curr_isolate = prev_isolate;
        todo!()
    }
}
