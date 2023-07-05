use std::{collections::HashMap, io, vec};

use crate::{
    ast::expressions::*,
    typechecker::sorts::{Fargs, Module, Process},
};

use super::{sorts::IvySort, TypeError};

pub struct Bindings(Vec<HashMap<Symbol, IvySort>>);

impl Bindings {
    pub fn new() -> Self {
        Bindings(vec![])
    }
}

pub struct Resolver {
    pub sorts: Vec<HashMap<String, IvySort>>,
    pub ctx: Vec<IvySort>,
}

impl Resolver {
    pub fn new() -> Self {
        let mut s = Self {
            sorts: vec![],
            ctx: vec![],
        };
        s.push_scope();
        s
    }

    // Name bindings

    pub fn push_scope(&mut self) {
        self.sorts.push(HashMap::<_, _>::new())
    }

    pub fn pop_scope(&mut self) {
        if let None = self.sorts.pop() {
            panic!("popping an empty sort scope");
        }
    }

    pub fn lookup_sym(&self, sym: &str) -> Option<&IvySort> {
        self.sorts
            .iter()
            .rfind(|scope| scope.contains_key(sym))
            .and_then(|scope| scope.get(sym))
            .map(|s| self.resolve(s))
    }

    pub fn lookup_ident(&self, id: &Ident, _include_spec: bool) -> Result<&IvySort, TypeError> {
        let mut idents = id.iter();

        let mut curr_sym = idents.next().unwrap();
        let mut curr_sort = self.lookup_sym(curr_sym);

        for field in idents {
            match curr_sort {
                Some(IvySort::Module(Module { fields, .. })) => {
                    curr_sym = field;
                    curr_sort = fields.get(field);
                }
                Some(IvySort::Process(_)) => todo!(),
                Some(sort) => {
                    return Err(TypeError::NotARecord(sort.clone()));
                }
                None => return Err(TypeError::UnboundVariable(curr_sym.clone())),
            }
        }

        match curr_sort {
            None => Err(TypeError::UnboundVariable(curr_sym.clone())),
            Some(s) => Ok(s),
        }
    }

    pub fn append(&mut self, sym: Symbol, sort: IvySort) -> Result<(), TypeError> {
        let scope = match self.sorts.last_mut() {
            None => panic!("Appending into an empty scope"),
            Some(scope) => scope,
        };

        // Only check the current scope, shadowing should be fine, right?
        if let Some(existing) = scope.get(&sym) {
            if existing != &sort {
                return Err(TypeError::SortMismatch {
                    expected: existing.clone(),
                    actual: sort,
                });
            }
        }
        scope.insert(sym, sort);
        Ok(())
    }

    // Unification

    pub fn resolve<'a>(&'a self, sort: &'a IvySort) -> &'a IvySort {
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
            next_sort
        } else {
            sort
        }
    }

    fn unify_fargs(&mut self, lhs: &Fargs, rhs: &Fargs) -> Result<Fargs, TypeError> {
        match (lhs, rhs) {
            (Fargs::Unknown, Fargs::Unknown) => Ok(Fargs::Unknown),
            (Fargs::Unknown, Fargs::List(rhs)) => Ok(Fargs::List(rhs.clone())),
            (Fargs::List(lhs), Fargs::Unknown) => Ok(Fargs::List(lhs.clone())),
            (Fargs::List(lhs), Fargs::List(rhs)) => {
                if lhs.len() != rhs.len() {
                    Err(TypeError::LenMismatch(lhs.clone(), rhs.clone()))
                } else {
                    let mut args = vec![];
                    for (a1, a2) in lhs.iter().zip(rhs.iter()) {
                        args.push(self.unify(a1, a2)?);
                    }
                    Ok(Fargs::List(args))
                }
            }
        }
    }

    pub fn unify(&mut self, lhs: &IvySort, rhs: &IvySort) -> Result<IvySort, TypeError> {
        let lhs = self.resolve(lhs).clone();
        let rhs = self.resolve(rhs).clone();
        println!("unify({:?}, {:?})", lhs, rhs);
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
                let args = self.unify_fargs(lhsargs, rhsargs)?;
                let ret = self.unify(&lhsret, &rhsret)?;
                Ok(IvySort::Function(args, Box::new(ret)))
            }
            (t1, t2) => {
                if t1 == t2 {
                    Ok(lhs)
                } else {
                    Err(TypeError::UnificationError(lhs.clone(), rhs.clone()))
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
