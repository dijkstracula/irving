#![allow(dead_code)]

use std::collections::HashMap;

use crate::{
    ast::{
        expressions::{self, Param, Symbol},
        toplevels::Prog,
    },
    visitor::{control::VisitorResult, visitor::Visitor},
};

use super::{sorts::IvySort, Error};
use crate::visitor::control::Control::Continue;

pub struct Bindings(Vec<HashMap<Symbol, IvySort>>);

impl Bindings {
    pub fn new() -> Self {
        Bindings(vec![])
    }

    pub fn push_scope(&mut self) {
        self.0.push(HashMap::<_, _>::new())
    }

    pub fn pop_scope(&mut self) {
        match self.0.pop() {
            None => panic!("popping an empty scope"),
            Some(_) => (),
        }
    }

    // TODO: I wonder what we need to do in order to support annotations, which are qualified
    // identifiers.  Maybe it's its own kind of constraint?
    pub fn lookup(&self, sym: &Symbol) -> Option<&IvySort> {
        self.0
            .iter()
            .rfind(|scope| scope.contains_key(sym))
            .and_then(|scope| scope.get(sym))
    }

    pub fn append(&mut self, sym: Symbol, sort: IvySort) -> Result<(), Error> {
        let scope = match self.0.last_mut() {
            None => panic!("Appending into an empty scope"),
            Some(scope) => scope,
        };

        // Only check the current scope, shadowing should be fine, right?
        if let Some(existing) = scope.get(&sym) {
            if existing != &sort {
                return Err(Error::SortMismatch {
                    expected: existing.clone(),
                    actual: sort,
                });
            }
        }
        scope.insert(sym, sort);
        Ok(())
    }
}

// TODO: Experiment with this holding references, or maybe RCs?
pub struct Constraint(IvySort, IvySort);

pub struct TypeChecker {
    pub ctx: Vec<IvySort>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker { ctx: vec![] }
    }

    fn resolve(&mut self, sort: &IvySort) -> IvySort {
        if let IvySort::SortVar(original_id) = sort {
            let mut curr_id = *original_id;
            let mut next_sort = &self.ctx[curr_id];

            while let IvySort::SortVar(new_id) = next_sort {
                curr_id = *new_id;
                next_sort = &self.ctx[curr_id];

                //TODO: occurs check?
            }
            next_sort.clone()
        } else {
            sort.clone()
        }
    }

    fn unify(&mut self, lhs: &IvySort, rhs: &IvySort) -> Result<IvySort, Error> {
        let lhs = self.resolve(lhs);
        let rhs = self.resolve(rhs);
        match (&lhs, &rhs) {
            (IvySort::SortVar(i), IvySort::SortVar(j)) => {
                if i < j {
                    self.ctx[*j] = rhs.clone();
                    Ok(lhs)
                } else if i > j {
                    self.ctx[*i] = lhs.clone();
                    Ok(rhs)
                } else {
                    Ok(lhs)
                }
            }
            (IvySort::SortVar(i), _) => {
                self.ctx[*i] = rhs.clone();
                Ok(rhs)
            }
            (_, IvySort::SortVar(j)) => {
                self.ctx[*j] = lhs.clone();
                Ok(lhs)
            }
            (IvySort::Function(lhsargs, lhsret), IvySort::Function(rhsargs, rhsret)) => {
                if lhsargs.len() != rhsargs.len() {
                    Err(Error::UnificationError(lhs.clone(), rhs.clone()))
                } else {
                    for (a1, a2) in lhsargs.iter().zip(rhsargs.iter()) {
                        self.unify(a1, a2)?;
                    }
                    self.unify(&lhsret, &rhsret)
                }
            }
            (t1, t2) => {
                if t1 == t2 {
                    Ok(lhs)
                } else {
                    Err(Error::UnificationError(lhs.clone(), rhs.clone()))
                }
            }
        }
    }

    pub fn visit(prog: &mut Prog) {
        let mut cg = Self::new();
        cg.visit_prog(prog).unwrap();
    }
}

impl Visitor<IvySort, Error> for TypeChecker {
    // Terminals

    fn visit_boolean(&mut self, _: &mut bool) -> VisitorResult<IvySort, Error> {
        Ok(Continue(IvySort::Bool))
    }
    fn visit_param(&mut self, p: &mut Param) -> VisitorResult<IvySort, Error> {
        //if let Some(annotated) = p.
        //match self.ctx.lookup(&p.id) {
        //    None => Err(Error::UnboundVariable(p.id.clone())),
        //    Some(sort) => Ok(Continue((sort.clone(), vec![]))),
        // }
        todo!()
    }
    fn visit_number(&mut self, _: &mut i64) -> VisitorResult<IvySort, Error> {
        Ok(Continue(IvySort::Number))
    }

    // Nonterminals

    fn visit_binop(
        &mut self,
        lhs: &mut expressions::Expr,
        op: &expressions::Verb,
        rhs: &mut expressions::Expr,
    ) -> VisitorResult<IvySort, Error> {
        let lhs_sort = match self.visit_expr(lhs)? {
            Continue(s) => s,
            crate::visitor::control::Control::Remove => unreachable!(),
        };
        let rhs_sort = match self.visit_expr(rhs)? {
            Continue(s) => s,
            crate::visitor::control::Control::Remove => unreachable!(),
        };

        match op {
            // Boolean operators
            expressions::Verb::Iff
            | expressions::Verb::Or
            | expressions::Verb::And
            | expressions::Verb::Not
            | expressions::Verb::Arrow => {
                if self
                    .unify(&lhs_sort, &IvySort::Bool)
                    .and(self.unify(&IvySort::Bool, &rhs_sort))
                    .is_err()
                {
                    Err(Error::UnificationError(lhs_sort, rhs_sort))
                } else {
                    Ok(Continue(IvySort::Bool))
                }
            }

            // Equality and comparison
            expressions::Verb::Lt
            | expressions::Verb::Le
            | expressions::Verb::Gt
            | expressions::Verb::Ge => {
                if self
                    .unify(&lhs_sort, &IvySort::Number)
                    .and(self.unify(&IvySort::Number, &rhs_sort))
                    .is_err()
                {
                    Err(Error::UnificationError(lhs_sort, rhs_sort))
                } else {
                    Ok(Continue(IvySort::Bool))
                }
            }

            expressions::Verb::Equals | expressions::Verb::Notequals => {
                if self.unify(&lhs_sort, &rhs_sort).is_err() {
                    Err(Error::UnificationError(lhs_sort, rhs_sort))
                } else {
                    Ok(Continue(IvySort::Bool))
                }
            }

            // Numeric operators
            expressions::Verb::Plus
            | expressions::Verb::Minus
            | expressions::Verb::Times
            | expressions::Verb::Div => {
                if self
                    .unify(&lhs_sort, &IvySort::Number)
                    .and(self.unify(&IvySort::Number, &rhs_sort))
                    .is_err()
                {
                    Err(Error::UnificationError(lhs_sort, rhs_sort))
                } else {
                    Ok(Continue(IvySort::Number))
                }
            }

            // Field indexing
            expressions::Verb::Dot => {
                unimplemented!()
            }
        }
    }

    fn visit_call(&mut self, e: &mut expressions::AppExpr) -> VisitorResult<IvySort, Error> {
        todo!()
    }
}
