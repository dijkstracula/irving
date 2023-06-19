#![allow(dead_code)]

use std::collections::HashMap;

use crate::{
    ast::{
        declarations,
        expressions::{self, Param, Symbol},
        toplevels::Prog,
    },
    visitor::{
        control::{Control, VisitorResult},
        visitor::Visitor,
    },
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
    pub bindings: Bindings,
    pub ctx: Vec<IvySort>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut s = TypeChecker {
            bindings: Bindings::new(),
            ctx: vec![],
        };
        s.bindings.push_scope();
        s
    }

    fn resolve(&mut self, sort: &IvySort) -> IvySort {
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

    fn unify(&mut self, lhs: &IvySort, rhs: &IvySort) -> Result<IvySort, Error> {
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

    pub fn visit(&mut self, prog: &mut Prog) {
        self.visit_prog(prog).unwrap();
    }
}

impl Visitor<IvySort, Error> for TypeChecker {
    // Terminals

    fn visit_boolean(&mut self, _: &mut bool) -> VisitorResult<IvySort, Error> {
        Ok(Continue(IvySort::Bool))
    }
    fn visit_param(&mut self, p: &mut Param) -> VisitorResult<IvySort, Error> {
        let sort = match &mut p.sort {
            Some(idents) => {
                // TODO: I don't know how to do instance resolution yet, argh
                assert!(idents.len() == 1);
                let mut sym = idents.get_mut(0).unwrap();
                match self.visit_symbol(&mut sym)? {
                    Continue(sort) => sort,
                    Control::Remove => unreachable!(),
                }
            }
            None => self.new_sortvar(),
        };
        Ok(Continue(sort))
    }
    fn visit_number(&mut self, _: &mut i64) -> VisitorResult<IvySort, Error> {
        Ok(Continue(IvySort::Number))
    }
    fn visit_symbol(&mut self, sym: &mut Symbol) -> VisitorResult<IvySort, Error> {
        match sym.as_str() {
            "bool" => Ok(Continue(IvySort::Bool)),
            // TODO: and of course other builtins.
            _ => match self.bindings.lookup(sym) {
                Some(sort) => Ok(Continue(sort.clone())),
                None => Err(Error::UnboundVariable(sym.clone())),
            },
        }
    }

    // Nonterminals

    // Exprs

    fn visit_binop(
        &mut self,
        lhs: &mut expressions::Expr,
        op: &expressions::Verb,
        rhs: &mut expressions::Expr,
    ) -> VisitorResult<IvySort, Error> {
        let lhs_sort = match self.visit_expr(lhs)? {
            Continue(s) => s,
            Control::Remove => unreachable!(),
        };
        let rhs_sort = match self.visit_expr(rhs)? {
            Continue(s) => s,
            Control::Remove => unreachable!(),
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

            _ => unimplemented!(),
        }
    }

    fn visit_field_access(
        &mut self,
        lhs: &mut expressions::Expr,
        rhs: &mut Symbol,
    ) -> VisitorResult<IvySort, Error> {
        let recordsort = match self.visit_expr(lhs)? {
            Continue(IvySort::Module(proc)) => proc,
            Continue(sort) => return Err(Error::NotARecord(sort)),
            Control::Remove => unreachable!(),
        };

        match recordsort
            .impl_fields
            .get(rhs)
            .or(recordsort.spec_fields.get(rhs))
            .or(recordsort.commonspec_fields.get(rhs))
        {
            Some(sort) => Ok(Continue(sort.clone())),
            None => Err(Error::UnboundVariable(rhs.clone())),
        }
    }

    fn visit_app(&mut self, e: &mut expressions::AppExpr) -> VisitorResult<IvySort, Error> {
        let fsort = match self.visit_expr(&mut e.func)? {
            Continue(f) => f,
            Control::Remove => unreachable!(),
        };

        let mut argsorts = vec![];
        for a in &mut e.args {
            let a = match self.visit_expr(a)? {
                Continue(a) => a,
                Control::Remove => unreachable!(),
            };
            argsorts.push(a);
        }
        let retsort = self.new_sortvar();

        let expected_sort = IvySort::Function(argsorts, Box::new(retsort));
        if let Ok(IvySort::Function(_, ret)) = self.unify(&fsort, &expected_sort) {
            Ok(Continue(*ret))
        } else {
            Err(Error::InvalidApplication(fsort))
        }
    }

    // Decls

    fn visit_relation(
        &mut self,
        obj: &mut declarations::Relation,
    ) -> VisitorResult<IvySort, Error> {
        // A relation is a bool-producing function for our purposes.
        // TODO: contemplate defaultdict-style "default functions" like the C++
        // extraction code uses.
        let paramsorts = obj
            .params
            .iter_mut()
            .map(|p| match self.visit_param(p) {
                Ok(Continue(sort)) => Ok(sort),
                Ok(Control::Remove) => unreachable!(),
                Err(e) => Err(e),
            })
            .collect::<Result<Vec<_>, _>>()?;

        let relsort = IvySort::Function(paramsorts, Box::new(IvySort::Bool));
        self.bindings.append(obj.name.clone(), relsort)?;

        Ok(Continue(IvySort::Unit))
    }

    fn visit_module(
        &mut self,
        module: &mut declarations::ModuleDecl,
    ) -> VisitorResult<IvySort, Error> {
        todo!();
    }

    fn visit_typedecl(
        &mut self,
        ident: &expressions::TypeName,
        sort: &mut IvySort,
    ) -> VisitorResult<IvySort, Error> {
        let sortname = match ident {
            expressions::TypeName::Name(n) => n,
            expressions::TypeName::This => todo!(),
        };

        self.bindings.append(sortname.clone(), sort.clone())?;
        Ok(Continue(IvySort::Unit))
    }

    fn visit_vardecl(&mut self, term: &mut expressions::Term) -> VisitorResult<IvySort, Error> {
        let sort = match &mut term.sort {
            None => self.new_sortvar(),
            Some(s) => match self.visit_identifier(s)? {
                Continue(s) => s,
                Control::Remove => unreachable!(),
            },
        };
        self.bindings.append(term.id.clone(), sort)?;
        Ok(Continue(IvySort::Unit))
    }
}
