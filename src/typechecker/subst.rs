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
        log::debug!(target: "sort-substituter", "{s:?}");
        match s {
            expressions::Sort::ToBeInferred | expressions::Sort::Annotated(_) => {
                println!("Uh oh! {:?}", s);
                Ok(ControlMut::Produce(()))
            }
            expressions::Sort::Resolved(is) => {
                if let IvySort::SortVar(_) = is {
                    let resolved = self.bindings.resolve(is);
                    log::debug!(target: "sort-substituter", "{is:?} -> {resolved:?}");
                    Ok(ControlMut::Mutation(
                        expressions::Sort::Resolved(resolved.clone()),
                        (),
                    ))
                } else {
                    Ok(ControlMut::Produce(()))
                }
            }
        }
    }
}
