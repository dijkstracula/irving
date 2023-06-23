use anyhow::bail;

use crate::{
    ast::expressions::{self, Expr, Type},
    typechecker::TypeError,
    visitor::visitor::{self, Control, Visitable, Visitor},
};

use super::{sorts::IvySort, unifier::Resolver};

pub struct TypeChecker {
    pub bindings: Resolver,
}

impl TypeChecker {
    // TODO: this should take a ref to bindings because the visitor will
    // want to hold onto it.
    pub fn new() -> Self {
        TypeChecker {
            bindings: Resolver::new(),
        }
    }
}

impl Visitor<IvySort> for TypeChecker {
    /*
    fn begin_binop(&mut self, ast: &mut expressions::BinOp) -> visitor::VisitorResult<IvySort> {
        let lhs_sort = match ast.lhs.visit(self)? {
            Control::Produce(s) => s,
            _ => unreachable!()
        };
        let rhs_sort = match ast.rhs.visit(self)? {
            Control::Produce(s) => s,
            _ => unreachable!()
        };

        match ast.op {
            // Boolean operators
            expressions::Verb::Iff
            | expressions::Verb::Or
            | expressions::Verb::And
            | expressions::Verb::Not
            | expressions::Verb::Arrow => {
                if self
                    .bindings
                    .unify(&lhs_sort, &IvySort::Bool)
                    .and(self.bindings.unify(&IvySort::Bool, &rhs_sort))
                    .is_err()
                {
                    bail!(TypeError::UnificationError(lhs_sort, rhs_sort))
                } else {
                    Ok(Control::Produce(IvySort::Bool))
                }
            }

            // Equality and comparison
            expressions::Verb::Lt
            | expressions::Verb::Le
            | expressions::Verb::Gt
            | expressions::Verb::Ge => {
                if self
                    .bindings
                    .unify(&lhs_sort, &IvySort::Number)
                    .and(self.bindings.unify(&IvySort::Number, &rhs_sort))
                    .is_err()
                {
                    bail!(TypeError::UnificationError(lhs_sort, rhs_sort))
                } else {
                    Ok(IvySort::Bool)
                }
            }

            expressions::Verb::Equals | expressions::Verb::Notequals => {
                if self.bindings.unify(&lhs_sort, &rhs_sort).is_err() {
                    bail!(TypeError::UnificationError(lhs_sort, rhs_sort))
                } else {
                    Ok(IvySort::Bool)
                }
            }

            // Numeric operators
            expressions::Verb::Plus
            | expressions::Verb::Minus
            | expressions::Verb::Times
            | expressions::Verb::Div => {
                if self
                    .bindings
                    .unify(&lhs_sort, &IvySort::Number)
                    .and(self.bindings.unify(&IvySort::Number, &rhs_sort))
                    .is_err()
                {
                    bail!(TypeError::UnificationError(lhs_sort, rhs_sort))
                } else {
                    Ok(IvySort::Number)
                }
            }

            _ => unimplemented!(),
        }
        Ok(Control::SkipSiblings)
    }
    */

    fn boolean(&mut self, b: &mut bool) -> visitor::VisitorResult<IvySort, bool> {
        Ok(Control::Produce(IvySort::Bool))
    }

    fn number(&mut self, _n: &mut i64) -> visitor::VisitorResult<IvySort, i64> {
        Ok(Control::Produce(IvySort::Number))
    }

    fn param(
        &mut self,
        p: &mut expressions::Param,
    ) -> visitor::VisitorResult<IvySort, expressions::Param> {
        match &mut p.sort {
            Some(idents) => {
                // TODO: I don't know how to do instance resolution yet, argh
                assert!(idents.len() == 1);
                let sym = idents.get_mut(0).unwrap();
                sym.visit(self)
            }
            None => Ok(Control::Produce(self.bindings.new_sortvar())),
        }
    }

    fn symbol(
        &mut self,
        sym: &mut expressions::Symbol,
    ) -> visitor::VisitorResult<IvySort, expressions::Symbol> {
        match sym.as_str() {
            "bool" => Ok(Control::Produce(IvySort::Bool)),
            // TODO: and of course other builtins.
            _ => match self.bindings.lookup(sym) {
                Some(sort) => Ok(Control::Produce(sort.clone())),
                None => bail!(TypeError::UnboundVariable(sym.clone())),
            },
        }
    }
}
