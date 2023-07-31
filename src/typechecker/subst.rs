use crate::{
    ast::expressions,
    visitor::{ast::Visitor, ControlMut, VisitorResult},
};

use super::{inference::SortInferer, unifier::BindingResolver};

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

impl Visitor<()> for SortSubstituter {
    fn sort(&mut self, s: &mut expressions::Sort) -> VisitorResult<(), expressions::Sort> {
        match s {
            expressions::Sort::ToBeInferred | expressions::Sort::Annotated(_) => {
                println!("Uh oh! {:?}", s);
                Ok(ControlMut::Produce(()))
            }
            expressions::Sort::Resolved(is) => {
                let resolved = self.bindings.resolve(is);
                log::debug!(target: "sort-substituter", "{is:?} -> {resolved:?}");
                Ok(ControlMut::Mutation(
                    expressions::Sort::Resolved(resolved.clone()),
                    (),
                ))
            }
        }
    }
}
