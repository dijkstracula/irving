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

pub struct Context(Vec<HashMap<Symbol, IvySort>>);

impl Context {
    pub fn new() -> Self {
        Context(vec![])
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

pub struct ConstraintGenerator {
    pub ctx: Context,
    next_sortvar_id: usize,
}

impl ConstraintGenerator {
    pub fn new() -> Self {
        ConstraintGenerator {
            ctx: Context::new(),
            next_sortvar_id: 0,
        }
    }

    pub fn visit(prog: &mut Prog) {
        let mut cg = Self::new();
        cg.visit_prog(prog).unwrap();
    }

    fn next_sortvar(&mut self) -> IvySort {
        let id = self.next_sortvar_id;
        self.next_sortvar_id += 1;
        IvySort::SortVar(id)
    }
}

impl Visitor<(IvySort, Vec<Constraint>), Error> for ConstraintGenerator {
    // Terminals

    fn visit_boolean(&mut self, _: &mut bool) -> VisitorResult<(IvySort, Vec<Constraint>), Error> {
        Ok(Continue((IvySort::Bool, vec![])))
    }
    fn visit_param(&mut self, p: &mut Param) -> VisitorResult<(IvySort, Vec<Constraint>), Error> {
        //if let Some(annotated) = p.
        match self.ctx.lookup(&p.id) {
            None => Err(Error::UnboundVariable(p.id.clone())),
            Some(sort) => Ok(Continue((sort.clone(), vec![]))),
        }
    }
    fn visit_number(&mut self, _: &mut i64) -> VisitorResult<(IvySort, Vec<Constraint>), Error> {
        Ok(Continue((IvySort::Number, vec![])))
    }

    // Nonterminals

    fn visit_binop(
        &mut self,
        lhs: &mut expressions::Expr,
        op: &expressions::Verb,
        rhs: &mut expressions::Expr,
    ) -> VisitorResult<(IvySort, Vec<Constraint>), Error> {
        let (lhs_sort, mut lhs_constraints) = match self.visit_expr(lhs)? {
            Continue((s, c)) => (s, c),
            crate::visitor::control::Control::Remove => unreachable!(),
        };
        let (rhs_sort, mut rhs_constraints) = match self.visit_expr(rhs)? {
            Continue((s, c)) => (s, c),
            crate::visitor::control::Control::Remove => unreachable!(),
        };

        lhs_constraints.append(&mut rhs_constraints);
        let mut constraints = lhs_constraints;

        match op {
            // Boolean operators
            expressions::Verb::Iff
            | expressions::Verb::Or
            | expressions::Verb::And
            | expressions::Verb::Not
            | expressions::Verb::Arrow => {
                constraints.push(Constraint(lhs_sort.clone(), rhs_sort.clone()));
                constraints.push(Constraint(lhs_sort, IvySort::Bool));
                constraints.push(Constraint(IvySort::Bool, rhs_sort));
                Ok(Continue((IvySort::Bool, constraints)))
            }

            // Equality and comparison
            expressions::Verb::Lt
            | expressions::Verb::Le
            | expressions::Verb::Gt
            | expressions::Verb::Ge => {
                constraints.push(Constraint(lhs_sort.clone(), rhs_sort.clone()));
                constraints.push(Constraint(lhs_sort, IvySort::Number));
                constraints.push(Constraint(IvySort::Number, rhs_sort));
                Ok(Continue((IvySort::Bool, constraints)))
            }

            expressions::Verb::Equals | expressions::Verb::Notequals => {
                constraints.push(Constraint(lhs_sort, rhs_sort));
                Ok(Continue((IvySort::Bool, constraints)))
            }

            // Numeric operators
            expressions::Verb::Plus
            | expressions::Verb::Minus
            | expressions::Verb::Times
            | expressions::Verb::Div => {
                constraints.push(Constraint(lhs_sort.clone(), rhs_sort.clone()));
                constraints.push(Constraint(lhs_sort, IvySort::Number));
                constraints.push(Constraint(IvySort::Number, rhs_sort));
                Ok(Continue((IvySort::Number, constraints)))
            }

            // Field indexing
            expressions::Verb::Dot => {
                // TODO: We need to check that the rhs expression is valid for the lhs's record type.
                // We don't have record types yet, though.
                Ok(Continue((rhs_sort, constraints)))
            }
        }
    }

    fn visit_call(
        &mut self,
        e: &mut expressions::AppExpr,
    ) -> VisitorResult<(IvySort, Vec<Constraint>), Error> {
        let mut argsorts = vec![];
        let mut constraints = vec![];
        for arg in &mut e.args {
            let (arg_sort, mut arg_cstrs) = match self.visit_expr(arg)? {
                Continue((a, c)) => (a, c),
                crate::visitor::control::Control::Remove => unreachable!(),
            };
            argsorts.push(arg_sort);
            constraints.append(&mut arg_cstrs);
        }
        let (funcsort, mut func_cstrs) = match self.visit_expr(&mut e.func)? {
            Continue((a, c)) => (a, c),
            crate::visitor::control::Control::Remove => unreachable!(),
        };
        constraints.append(&mut func_cstrs);

        // TODO: can't we annotate the return value with `(returns foo: sort)`?  Do we drop this at the parser level?
        let fnsort = IvySort::Function(argsorts, Box::new(self.next_sortvar()));

        // TODO: at present this assumes that the function is already in the context.  Either we'll have to collect
        // them in advance or just write Ivy programs where declarations precede use (might be impossible for us?)
        //constraints.push(Constraint(fnsort, ()))
        todo!()
    }
}
