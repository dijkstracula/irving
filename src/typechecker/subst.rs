use crate::{
    ast::{actions, expressions::Sort, statements},
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
        s: &mut Sort,
    ) -> VisitorResult<(), TypeError, Sort> {
        match s {
            Sort::ToBeInferred => {
                panic!("Didn't infer sort!");
            }
            Sort::Annotated(name) => {
                let name = name.join(".");
                panic!("Didn't resolve sort {:?}", name);
            }
            Sort::Resolved(is) => {
                match is {
                    IvySort::Generic(_, _) |
                    IvySort::SortVar(_) => {
                        let resolved = self.bindings.resolve(is);
                        if is != resolved {
                            log::debug!(target: "sort-substituter", "{is:?} -> {resolved:?}");
                            return Ok(ControlMut::Mutation(
                                Sort::Resolved(resolved.clone()),
                                (),
                            ));
                        }
                    }
                    _ => ()
                }
                Ok(ControlMut::Produce(()))
            }
        }
    }
}
