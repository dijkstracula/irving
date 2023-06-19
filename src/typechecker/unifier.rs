use std::{collections::HashMap, vec};

use crate::ast::expressions::*;

use super::{sorts::IvySort, Error};

pub struct Bindings(Vec<HashMap<Symbol, IvySort>>);

impl Bindings {
    pub fn new() -> Self {
        Bindings(vec![])
    }
}

pub struct Resolver {
    pub bindings: Vec<HashMap<String, IvySort>>,
    pub ctx: Vec<IvySort>,
}

impl Resolver {
    pub fn new() -> Self {
        let mut s = Self {
            bindings: vec![],
            ctx: vec![],
        };
        s.push_scope();
        s
    }

    // Name bindings

    pub fn push_scope(&mut self) {
        self.bindings.push(HashMap::<_, _>::new())
    }

    pub fn pop_scope(&mut self) {
        match self.bindings.pop() {
            None => panic!("popping an empty scope"),
            Some(_) => (),
        }
    }

    // TODO: I wonder what we need to do in order to support annotations, which are qualified
    // identifiers.  Maybe it's its own kind of constraint?
    pub fn lookup(&self, sym: &Symbol) -> Option<&IvySort> {
        self.bindings
            .iter()
            .rfind(|scope| scope.contains_key(sym))
            .and_then(|scope| scope.get(sym))
    }

    pub fn append(&mut self, sym: Symbol, sort: IvySort) -> Result<(), Error> {
        let scope = match self.bindings.last_mut() {
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

    // Unification

    pub fn resolve(&mut self, sort: &IvySort) -> IvySort {
        if let IvySort::SortVar(original_id) = sort {
            let mut curr_id = *original_id;
            let mut next_sort = &self.ctx[curr_id];

            while let IvySort::SortVar(new_id) = next_sort {
                if new_id == original_id {
                    //TODO: occurs check?
                    break;
                }
                curr_id = *new_id;
                next_sort = &self.ctx[curr_id];
            }
            next_sort.clone()
        } else {
            sort.clone()
        }
    }

    pub fn unify(&mut self, lhs: &IvySort, rhs: &IvySort) -> Result<IvySort, Error> {
        println!("unify({:?}, {:?})", lhs, rhs);
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
                    let mut args = vec![];
                    for (a1, a2) in lhsargs.iter().zip(rhsargs.iter()) {
                        args.push(self.unify(a1, a2)?);
                    }
                    let ret = self.unify(&lhsret, &rhsret)?;
                    Ok(IvySort::Function(args, Box::new(ret)))
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

    pub fn new_sortvar(&mut self) -> IvySort {
        let s = IvySort::SortVar(self.ctx.len());
        self.ctx.push(s.clone());
        s
    }
}
