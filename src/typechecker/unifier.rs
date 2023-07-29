use std::{collections::HashMap, vec};

use crate::{
    ast::expressions::*,
    typechecker::sorts::{ActionArgs, Module, Object},
};

use super::{
    sorts::{ActionRet, IvySort},
    TypeError,
};

pub struct Bindings(Vec<HashMap<Token, IvySort>>);

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

    pub fn lookup_ident(&self, id: &Ident) -> Result<&IvySort, TypeError> {
        let mut idents = id.iter();

        let mut curr_sym = idents.next().unwrap();
        let mut curr_sort = self.lookup_sym(curr_sym);

        for field in idents {
            match curr_sort {
                Some(IvySort::Module(Module { fields, .. })) => {
                    curr_sort = fields.get(field);
                }
                Some(IvySort::Object(Object { args, fields })) => {
                    curr_sort = args.get(field).or(fields.get(field))
                }
                Some(sort) => {
                    return Err(TypeError::NotARecord(sort.clone()));
                }
                None => return Err(TypeError::UnboundVariable(curr_sym.clone())),
            }
            curr_sym = field;
        }

        match curr_sort {
            None => Err(TypeError::UnboundVariable(curr_sym.clone())),
            Some(s) => Ok(s),
        }
    }

    pub fn append(&mut self, sym: Token, sort: IvySort) -> Result<(), TypeError> {
        if self.sorts.last().is_none() {
            panic!("Appending into an empty scope");
        }

        // Only check the current scope, shadowing should be fine, right?
        if let Some(existing) = self.sorts.last().unwrap().get(&sym) {
            if existing.is_sortvar() || sort.is_sortvar() {
                let existing = existing.clone();
                let unified = self.unify(&existing, &sort)?;

                self.sorts.last_mut().unwrap().insert(sym, unified);
                return Ok(());
            } else if existing != &sort {
                return Err(TypeError::ReboundVariable {
                    sym,
                    prev: existing.clone(),
                    new: sort.clone(),
                });
            }
        }
        self.sorts.last_mut().unwrap().insert(sym, sort);

        Ok(())
    }

    // Unification

    pub fn resolve<'a>(&'a self, sort: &'a IvySort) -> &'a IvySort {
        if let IvySort::SortVar(original_id) = sort {
            let mut curr_id = *original_id;
            let mut next_sort = &self.ctx[curr_id];

            while let IvySort::SortVar(new_id) = next_sort {
                //eprintln!("resolve: {:?}", self.ctx);
                if new_id == &curr_id {
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

    fn unify_vec(
        &mut self,
        lhs: &Vec<IvySort>,
        rhs: &Vec<IvySort>,
    ) -> Result<Vec<IvySort>, TypeError> {
        if lhs.len() != rhs.len() {
            Err(TypeError::SortListMismatch(lhs.clone(), rhs.clone()))
        } else {
            let mut args = vec![];
            for (a1, a2) in lhs.iter().zip(rhs.iter()) {
                args.push(self.unify(a1, a2)?);
            }
            Ok(args)
        }
    }

    fn unify_action_args(
        &mut self,
        lhs: &ActionArgs,
        rhs: &ActionArgs,
    ) -> Result<ActionArgs, TypeError> {
        match (lhs, rhs) {
            (ActionArgs::Unknown, ActionArgs::Unknown) => Ok(ActionArgs::Unknown),
            (ActionArgs::Unknown, ActionArgs::List(rhs)) => Ok(ActionArgs::List(rhs.clone())),
            (ActionArgs::List(lhs), ActionArgs::Unknown) => Ok(ActionArgs::List(lhs.clone())),
            (ActionArgs::List(lhs), ActionArgs::List(rhs)) => {
                Ok(ActionArgs::List(self.unify_vec(lhs, rhs)?))
            }
        }
    }

    fn unify_action_rets(
        &mut self,
        lhs: &ActionRet,
        rhs: &ActionRet,
    ) -> Result<ActionRet, TypeError> {
        match (lhs, rhs) {
            (ActionRet::Unknown, ActionRet::Unknown) => Ok(ActionRet::Unknown),

            (ActionRet::Unknown, ActionRet::Unit)
            | (ActionRet::Unit, ActionRet::Unknown)
            | (ActionRet::Unit, ActionRet::Unit) => Ok(ActionRet::Unit),

            (ActionRet::Unknown, n @ ActionRet::Named(_))
            | (n @ ActionRet::Named(_), ActionRet::Unknown) => Ok(n.clone()),

            (ActionRet::Unit, ActionRet::Named(binding))
            | (ActionRet::Named(binding), ActionRet::Unit) => self
                .unify(&binding.decl, &IvySort::Unit)
                .map(|s| ActionRet::named(binding.name.clone(), s)),
            (ActionRet::Named(lhs), ActionRet::Named(rhs)) => {
                // TODO: we can't support alpha-renaming in unification because we assume argument names
                // are bound to consistent names.
                //if lhs.name != rhs.name {
                //    return Err(TypeError::FargMismatch { expected: lhs.name.clone(), actual: rhs.name.clone() });
                // }
                self.unify(&lhs.decl, &rhs.decl)
                    .map(|s| ActionRet::named(lhs.name.clone(), s))
            }
        }
    }

    pub fn unify(&mut self, lhs: &IvySort, rhs: &IvySort) -> Result<IvySort, TypeError> {
        let lhs = self.resolve(lhs).clone();
        let rhs = self.resolve(rhs).clone();
        log::debug!(target: "typechecker", "unify({lhs:?},{rhs:?})");
        match (&lhs, &rhs) {
            (IvySort::SortVar(i), IvySort::SortVar(j)) => {
                if i < j {
                    self.ctx[*j] = lhs.clone();
                    Ok(lhs)
                } else if i > j {
                    self.ctx[*i] = rhs.clone();
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
            (
                IvySort::Action(lhsargnames, lhsargsorts, lhsret),
                IvySort::Action(rhsargnames, rhsargsorts, rhsret),
            ) => {
                if lhsargnames.len() != rhsargnames.len() {
                    return Err(TypeError::LenMismatch {
                        expected: lhsargnames.len(),
                        actual: rhsargnames.len(),
                    });
                }
                let args = self.unify_action_args(lhsargsorts, rhsargsorts)?;
                let ret = self.unify_action_rets(lhsret, rhsret)?;

                // XXX: choosing the LHS' argument list arbitrarily!  Maybe we
                // can't treat the arguments to unify() as symmetric...
                Ok(IvySort::Action(lhsargnames.clone(), args, ret))
            }

            // These subtyping relationship are meant to capture part of the
            // aspect that in Ivy, numerals' types are entirely inferred from
            // context.
            (IvySort::Number, IvySort::Range(lo, hi))
            | (IvySort::Range(lo, hi), IvySort::Number) => {
                // This subtyping relationship is fine, because Ivy's range
                // datatype saturates arithmetic operations.
                Ok(IvySort::Range(lo.clone(), hi.clone()))
            }
            (IvySort::Number, IvySort::Uninterpreted)
            | (IvySort::Uninterpreted, IvySort::Number) => Ok(IvySort::Uninterpreted),
            // TODO: I need to experiment with weird typings like
            // `var x: bool := 42`: if the types are truly inferred from
            // context, then `42` should be treated of course as a bool.  Is
            // this actually what happens???

            // This subtyping relationship is for indexing into an isolate
            // definition by its arguments.
            (
                IvySort::Action(_, ActionArgs::List(fargs), fret),
                p @ IvySort::Object(Object { args, .. }),
            )
            | (
                p @ IvySort::Object(Object { args, .. }),
                IvySort::Action(_, ActionArgs::List(fargs), fret),
            ) => {
                let unified = match fret {
                    ActionRet::Unknown => p.clone(),
                    ActionRet::Unit => IvySort::Unit,
                    ActionRet::Named(binding) => self.unify(&binding.decl, p)?,
                };

                // XXX: args is unordered so we can't unify them in the multiple argument case.
                // see https://github.com/dijkstracula/irving/issues/25 .
                if fargs.len() != args.len() || fargs.len() > 1 {
                    let pargs = args.iter().map(|(_, v)| v.clone()).collect::<Vec<_>>();
                    return Err(TypeError::SortListMismatch(fargs.clone(), pargs));
                }
                Ok(unified)
            }

            // This subtyping relation says that "calling" a Relation with well-typed
            // arguments of the correct arity produces a Bool.
            (
                IvySort::Action(_, ActionArgs::List(aargsorts), ActionRet::Unknown),
                IvySort::Relation(rargsorts),
            )
            | (
                IvySort::Relation(rargsorts),
                IvySort::Action(_, ActionArgs::List(aargsorts), ActionRet::Unknown),
            ) => {
                let unified_sorts = self.unify_vec(aargsorts, rargsorts)?;
                Ok(IvySort::Relation(unified_sorts))
            }

            // This subtyping relationship says that `this` shoudl only
            // unify with processes or modules.
            (IvySort::This, s @ IvySort::Object(_))
            | (IvySort::This, s @ IvySort::Module(_))
            | (s @ IvySort::Module(_), IvySort::This)
            | (s @ IvySort::Object(_), IvySort::This) => Ok(s.clone()),

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
