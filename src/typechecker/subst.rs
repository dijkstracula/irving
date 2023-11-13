use crate::{
    ast::{actions, expressions, statements},
    typechecker::sorts::IvySort,
    visitor::{
        ast::{Visitable, Visitor},
        ControlMut, VisitorResult,
    },
};

use super::{inference::SortInferer, unifier::BindingResolver, TypeError};

pub struct SortSubstituter {
    pub bindings: BindingResolver,
}

impl SortSubstituter {
    pub fn from_inferer(inferer: SortInferer) -> Self {
        Self {
            bindings: inferer.bindings,
        }
    }
}

impl Visitor<(), TypeError> for SortSubstituter {
    fn action_seq(
        &mut self,
        ast: &mut Vec<actions::Action>,
    ) -> VisitorResult<(), TypeError, statements::Stmt> {
        //XXX: kinda dumb, honestly.
        // https://github.com/dijkstracula/irving/issues/17
        let _ = ast.visit(self)?.modifying(ast);
        Ok(ControlMut::Produce(()))
    }

    fn sort(
        &mut self,
        s: &mut expressions::Sort,
    ) -> VisitorResult<(), TypeError, expressions::Sort> {
        match s {
            expressions::Sort::ToBeInferred => {
                log::warn!("Assuming ToBeInferred is a nat");
                Ok(ControlMut::Produce(()))
            }
            expressions::Sort::Annotated(_) => {
                panic!("Didn't fully infer sort {:?}", s);
            }
            expressions::Sort::Resolved(is) => {
                if let IvySort::SortVar(_) = is {
                    let resolved = self.bindings.resolve(is);
                    if is != resolved {
                        log::debug!(target: "sort-substituter", "{is:?} -> {resolved:?}");
                        return Ok(ControlMut::Mutation(
                            expressions::Sort::Resolved(resolved.clone()),
                            (),
                        ));
                    }
                }
                Ok(ControlMut::Produce(()))
            }
        }
    }
}
